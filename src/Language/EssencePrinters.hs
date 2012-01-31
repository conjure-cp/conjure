{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ViewPatterns     #-}

module Language.EssencePrinters ( prSpec, prExpr
                                , prType, prKind, prKindInteractive
                                , prRuleRepr
                                ) where


import Control.Applicative hiding ( empty )
import Control.Monad ( forM, msum )
import Data.Maybe ( isNothing, maybeToList, mapMaybe )
import Data.List ( isPrefixOf, intersperse )
import Data.Generics.Uniplate.Direct ( Biplate, transformBi )

import Language.Essence
import PrintUtils
import Utils ( safeStr, ppShow )
import {-# SOURCE #-} Language.EssenceKinds ( textAfterBe )


--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

prExpr :: Expr -> Maybe Doc
prExpr = prExprPrec 0

type Prec = Int

prExprPrec :: Prec -> Expr -> Maybe Doc
prExprPrec prec x = msum $ map (\ pr -> pr prec x) [ prIdentifier, prValue, prGenericNode
                                                   , prDomain, prDeclLambda "lambda" -- , \ _ -> prDeclQuantifier ""
                                                   , prExprQuantifier, const prReplace
                                                   ]

prIdentifier :: Prec -> Expr -> Maybe Doc
prIdentifier _ (Identifier i) = return $ text i
prIdentifier _ _ = Nothing


--------------------------------------------------------------------------------
-- Value* ----------------------------------------------------------------------
--------------------------------------------------------------------------------

prValue :: Prec -> Expr -> Maybe Doc
prValue _ (ValueBoolean False) = return $ text "false"
prValue _ (ValueBoolean True ) = return $ text "true"
prValue _ (ValueInteger i    ) = return $ integer i
prValue _ (ValueMatrix    xs ) = brackets . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueTuple     xs ) = parens   . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueSet       xs ) = (text "set"       <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueMSet      xs ) = (text "mset"      <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueFunction  xs ) = (text "function"  <+>) . braces . sep . punctuate comma <$> elements
    where elements = forM xs $ \ (i,j) -> do i' <- prExpr i
                                             j' <- prExpr j
                                             return (i' <+> text "->" <+> j')
prValue _ (ValueRelation  xs ) = (text "relation"  <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValuePartition xss) = (text "partition" <+>) . braces . sep . punctuate comma <$> mapM elements xss
    where elements xs = braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ _ = Nothing


--------------------------------------------------------------------------------
-- Domain* ---------------------------------------------------------------------
--------------------------------------------------------------------------------

prDomain :: Prec -> Expr -> Maybe Doc
prDomain _ DomainBoolean = return $ text "bool"
prDomain _ (DomainIntegerFromTo fr to) = do
    frDom <- maybe (return empty) prExpr fr
    toDom <- maybe (return empty) prExpr to
    return $ text "int" <> parens (frDom <> text ".." <> toDom)
prDomain _ (DomainIntegerList []) = return $ text "int"
prDomain _ (DomainIntegerList xs) = do
    xs' <- mapM prExpr xs
    return $ text "int" <> parens (sep (punctuate comma xs'))
prDomain _ (DomainUnnamed {theSize,representation}) = do
    s <- prExpr theSize
    return $ text "new"
         <+> text "type"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> text "of"
         <+> text "size"
         <+> s
prDomain _ (DomainEnum {enums,representation}) =
    return $ text "enum"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> braces (sep (punctuate comma (map text enums)))
prDomain _ (DomainMatrix i j) = do
    is' <- mapM prExpr is
    k'  <- prExpr k
    return $ text "matrix"
         <+> text "indexed"
         <+> text "by"
         <+> brackets (sep (punctuate comma is'))
         <+> text "of"
         <+> k'
    where
        (is,k) = helper i j
        helper a (DomainMatrix b c) = let (d,e) = helper b c in (a:d,e)
        helper a b = ([a],b)
prDomain _ (DomainTuple {components,representation}) = do
    cs <- mapM prExpr components
    return $ text "tuple"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> text "of"
         <+> parens (sep (punctuate comma cs))
prDomain _ p@(DomainSet {element}) = do
    let
        attrs :: [Doc]
        attrs = mapMaybe (attrToDoc p) [ "size", "minSize", "maxSize"
                                       , "representation", "attrDontCare"
                                       ]
    element' <- prExpr element
    return $  text "set"
          <+> parensIf (not (null attrs)) (sep (punctuate comma attrs))
          <+> text "of"
          <+> element'
prDomain _ p@(DomainMSet {element}) = do
    let
        attrs :: [Doc]
        attrs = mapMaybe (attrToDoc p) [ "size", "minSize", "maxSize"
                                       , "occr", "minOccr", "maxOccr"
                                       , "representation", "attrDontCare"
                                       ]
    element' <- prExpr element
    return $  text "mset"
          <+> parensIf (not (null attrs)) (sep (punctuate comma attrs))
          <+> text "of"
          <+> element'
prDomain _ p@(DomainFunction {functionFrom,functionTo}) = do
    let
        attrs :: [Doc]
        attrs = mapMaybe (attrToDoc p) [ "total", "partial"
                                       , "injective", "bijective", "surjective"
                                       , "representation", "attrDontCare"
                                       ]
    from' <- prExpr functionFrom
    to'   <- prExpr functionTo
    return $  text "function"
          <+> parensIf (not (null attrs)) (sep (punctuate comma attrs))
          <+> from'
          <+> text "->"
          <+> to'
prDomain _ p@(DomainRelation {components})  = do
    let
        attrs :: [Doc]
        attrs = mapMaybe (attrToDoc p) ["representation", "attrDontCare"]
    components' <- mapM prExpr components
    return $  text "relation"
          <+> parensIf (not (null attrs)) (sep (punctuate comma attrs))
          <+> text "of"
          <+> parens (sep (punctuate (text " *") components'))
prDomain _ p@(DomainPartition {element})  = do
    let
        attrs :: [Doc]
        attrs = mapMaybe (attrToDoc p) [ "regular", "complete"
                                       , "size", "minSize", "maxSize"
                                       , "partSize", "minPartSize", "maxPartSize"
                                       , "numParts", "minNumParts", "maxNumParts"
                                       , "representation", "attrDontCare"
                                       ]
    element' <- prExpr element
    return $  text "partition"
          <+> parensIf (not (null attrs)) (sep (punctuate comma attrs))
          <+> text "from"
          <+> element'
prDomain _ _ = Nothing


attrToDoc :: Expr -> String -> Maybe Doc

attrToDoc (representation -> Just i) "representation" = (text "representation" <+>) <$> return (text i)
attrToDoc (attrDontCare   -> True  ) "attrDontCare"   = return $ text "_"

attrToDoc (DomainSet {    size = Just i })    "size" = (text    "size" <+>) <$> prExpr i
attrToDoc (DomainSet { minSize = Just i }) "minSize" = (text "minSize" <+>) <$> prExpr i
attrToDoc (DomainSet { maxSize = Just i }) "maxSize" = (text "maxSize" <+>) <$> prExpr i

attrToDoc (DomainMSet {    size = Just i })    "size" = (text    "size" <+>) <$> prExpr i
attrToDoc (DomainMSet { minSize = Just i }) "minSize" = (text "minSize" <+>) <$> prExpr i
attrToDoc (DomainMSet { maxSize = Just i }) "maxSize" = (text "maxSize" <+>) <$> prExpr i
attrToDoc (DomainMSet {    occr = Just i })    "occr" = (text    "occr" <+>) <$> prExpr i
attrToDoc (DomainMSet { minOccr = Just i }) "minOccr" = (text "minOccr" <+>) <$> prExpr i
attrToDoc (DomainMSet { maxOccr = Just i }) "maxOccr" = (text "maxOccr" <+>) <$> prExpr i

attrToDoc (DomainFunction { isTotal      = True }) "total"      = return $ text "total"
attrToDoc (DomainFunction { isPartial    = True }) "partial"    = return $ text "partial"
attrToDoc (DomainFunction { isInjective  = True }) "injective"  = return $ text "injective"
attrToDoc (DomainFunction { isBijective  = True }) "bijective"  = return $ text "bijective"
attrToDoc (DomainFunction { isSurjective = True }) "surjective" = return $ text "surjective"

attrToDoc (DomainPartition { isRegular   = True   }) "regular"     = return $ text "regular"
attrToDoc (DomainPartition { isComplete  = True   }) "complete"    = return $ text "complete"
attrToDoc (DomainPartition { size        = Just i }) "size"        = (text "size"        <+>) <$> prExpr i
attrToDoc (DomainPartition { minSize     = Just i }) "minSize"     = (text "minSize"     <+>) <$> prExpr i
attrToDoc (DomainPartition { maxSize     = Just i }) "maxSize"     = (text "maxSize"     <+>) <$> prExpr i
attrToDoc (DomainPartition { partSize    = Just i }) "partSize"    = (text "partSize"    <+>) <$> prExpr i
attrToDoc (DomainPartition { minPartSize = Just i }) "minPartSize" = (text "minPartSize" <+>) <$> prExpr i
attrToDoc (DomainPartition { maxPartSize = Just i }) "maxPartSize" = (text "maxPartSize" <+>) <$> prExpr i
attrToDoc (DomainPartition { numParts    = Just i }) "numParts"    = (text "numParts"    <+>) <$> prExpr i
attrToDoc (DomainPartition { minNumParts = Just i }) "minNumParts" = (text "minNumParts" <+>) <$> prExpr i
attrToDoc (DomainPartition { maxNumParts = Just i }) "maxNumParts" = (text "maxNumParts" <+>) <$> prExpr i

attrToDoc _ _ = Nothing


--------------------------------------------------------------------------------
-- GenericNode -----------------------------------------------------------------
--------------------------------------------------------------------------------

prGenericNode :: Prec -> Expr -> Maybe Doc
-- uncomment the following, if you want to use TwoBars in the output.
-- I don't, hence commented out.
-- prGenericNode _ (GenericNode op [x]) | op `elem` [Abs,Card] = do
--     x' <- prExpr x
--     return $ char '|' <> x' <> char '|'
prGenericNode _ (GenericNode Index [m,i]) = do
    a'  <- prExpr a
    bs' <- mapM (\ t -> prMatrixSlice t <|> prExpr t ) (reverse (i:bs))
    return $ a' <> brackets (sep (punctuate comma bs'))
    where
        (a,bs) = helper m
        helper (GenericNode Index [x,y]) = let (z,js) = helper x in (z,y:js)
        helper x = (x,[])
prGenericNode _ (GenericNode Replace [x,fr,to]) = do
    x'  <- prExpr x
    fr' <- prExpr fr
    to' <- prExpr to
    return $ x' <+> braces (fr' <+> text "->" <+> to')
prGenericNode prec (GenericNode op xs) = prOpExpr prec (opDescriptor op) xs
prGenericNode _ _ = Nothing


prOpExpr :: Int -> OpDescriptor -> [Expr] -> Maybe Doc
prOpExpr _ OpSpecial _ = error "prOpExpr: OpSpecial"
prOpExpr _ (OpLispy {face}) xs = do
    elements <- mapM prExpr xs
    return $ text face <> parens (sep (punctuate comma elements))
prOpExpr p (OpInfixL {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec precedence a
    b' <- prExprPrec (precedence+1) b
    return $ sep [a', text face, b']
prOpExpr p (OpInfixN {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec (precedence+1) a
    b' <- prExprPrec (precedence+1) b
    return $ sep [a', text face, b']
prOpExpr p (OpInfixR {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec (precedence+1) a
    b' <- prExprPrec precedence b
    return $ sep [a', text face, b']
prOpExpr p (OpPrefix {face,precedence}) [a]   = parensIf (p > precedence) <$> do
    a' <- prExprPrec precedence a
    return $ text face <> a'
prOpExpr _ _ _ = error "prOpExpr: unknown case"


prExprQuantifier :: Prec -> Expr -> Maybe Doc
prExprQuantifier p (ExprQuantifier (Identifier qName) (Identifier qVar) qOver qGuard qBody) = parensIf (p > 0) <$> do
    qOver'  <- (colon <+>) <$> prExpr qOver
    qGuard' <- case qGuard of Nothing -> return empty
                              Just g  -> (comma <+>) <$> prExpr g
    qBody'  <- (char '.' <+>) <$> prExpr qBody
    return $ text qName
         <+> text qVar
         <+> qOver'
         <+> qGuard'
         <+> qBody'
prExprQuantifier _ _ = Nothing

prReplace :: Expr -> Maybe Doc
prReplace (GenericNode Replace [x,i,j]) = do
    x' <- prExpr x
    i' <- prExpr i
    j' <- prExpr j
    return $ x' <+> braces (i' <+> text "->" <+> j')
prReplace _ = Nothing

prMatrixSlice :: Expr -> Maybe Doc
prMatrixSlice (MatrixSlice Nothing   Nothing) = return $ text ".."
prMatrixSlice (MatrixSlice (Just a)  Nothing) = (<> text "..") <$> prExpr a
prMatrixSlice (MatrixSlice Nothing  (Just b)) = (text ".." <>) <$> prExpr b
prMatrixSlice (MatrixSlice (Just a) (Just b)) = (\ i j -> i <> text ".." <> j) <$> prExpr a <*> prExpr b
prMatrixSlice _ = Nothing


--------------------------------------------------------------------------------
-- DeclLambda ------------------------------------------------------------------
--------------------------------------------------------------------------------

prDeclLambda :: String -> Prec -> Expr -> Maybe Doc
prDeclLambda keyword _ (DeclLambda args x) = do
    let braces' i = char '{' <+> i <+> char '}'
    args' <- forM args $ \ (nm,t) -> do t' <- prType t
                                        return (text nm <+> colon <+> t')
    x' <- prExpr x
    return $ text keyword
         <+> braces' (sep (punctuate comma args') <+> text "->" <+> x')
prDeclLambda _ _ _ = Nothing


--------------------------------------------------------------------------------
-- DeclQuantifier --------------------------------------------------------------
--------------------------------------------------------------------------------

prDeclQuantifier :: String -> Expr -> Maybe Doc
prDeclQuantifier name (DeclQuantifier l1 l2 iden) = do
    let
        pre :: String
        pre = "__" ++ name ++ "_"

    let l1_ = doRenamings pre l1
    let l2_ = doRenamings pre l2

    l1'   <- prDeclLambda "append" undefined l1_ <|> prIdentifier undefined l1_
    l2'   <- prDeclLambda "guard"  undefined l2_ <|> prIdentifier undefined l2_
    iden' <- (text "identity" <+>) <$> prExpr iden
    return $ vcat [ lbrace
                  , nest 2 $ vcat [l1',l2',iden']
                  , rbrace
                  ]
prDeclQuantifier _ _ = Nothing


--------------------------------------------------------------------------------
-- Spec ------------------------------------------------------------------------
--------------------------------------------------------------------------------

prSpec :: Spec -> Maybe Doc
prSpec Spec{language,version,topLevelBindings,topLevelWheres,objective,constraints} = do
    let
        removeKnownQuans :: Binding -> Maybe Binding
        removeKnownQuans (Letting, op, _) | op `elem` words "forall exists sum" = Nothing
        removeKnownQuans x = Just x
    let topLevelBindings' = mapMaybe removeKnownQuans topLevelBindings
    let langline = text "language" <+> text language <+> hcat (intersperse (text ".") (map int version))
    -- let langline = text "language ESSENCE' 2.0"
    let reportError msg mx = case mx of
                                Nothing -> error ("error while rendering: " ++ msg)
                                _       -> mx
    bindings <- reportError "bindings"    $ mapM (prBinding topLevelBindings) topLevelBindings'
    wheres   <- reportError "wheres"      $ mapM prWhere     topLevelWheres
    obj      <- reportError "objective"   $ mapM prObjective (maybeToList objective)
    cons     <- reportError "constraints" $ mapM (\ c -> fmap (nest 4) $ reportError ("constraint: " ++ ppShow c) $ prExpr c) constraints

    return . vcat . concat $
        [ [ langline ]
        , if null topLevelBindings' then [] else emptyLine : bindings
        , if null topLevelWheres    then [] else emptyLine : wheres
        , if isNothing objective    then [] else emptyLine : obj
        , if null constraints       then [] else emptyLine : text "such" <+> text "that" : punctuate comma cons
        , [ emptyLine ]
        ]

prBinding :: [Binding] -> Binding -> Maybe Doc
prBinding _  (Find,nm,x) = do
    x' <- prExpr x
    return $ text "find" <+> text nm <+> colon <+> x'
prBinding _  (Given,nm,x) = do
    x' <- prExpr x
    return $ text "given" <+> text nm <+> colon <+> x'
prBinding bs (Letting,nm,x) = do
    x' <- prDeclQuantifier nm x <|> prExpr x
    return $ text "letting"
         <+> text nm
         <+> text "be"
         <+> textAfterBe bs x
         <+> x'
prBinding _ (InRule,_,_) = error "EssencePrinters.prBinding InRule"

prWhere :: Where -> Maybe Doc
prWhere w = do
    w' <- prExpr w
    return $ text "where" <+> w'

prObjective :: Objective -> Maybe Doc
prObjective (Minimising,x) = do
    x' <- prExpr x
    return $ text "minimising" <+> x'
prObjective (Maximising,x) = do
    x' <- prExpr x
    return $ text "maximising" <+> x'


--------------------------------------------------------------------------------
-- Type ------------------------------------------------------------------------
--------------------------------------------------------------------------------

prType :: Type -> Maybe Doc
prType TypeUnknown        = return $ text "?"
prType (TypeIdentifier i) = return $ text i
prType TypeBoolean        = return $ text "bool"
prType TypeInteger        = return $ text "int"
prType TypeUnnamed        = return $ text "unnamed"
prType TypeEnum           = return $ text "enum"
prType (TypeMatrix t)     = do t' <- prType t
                               return $ text "matrix"
                                    <+> text "of"
                                    <+> t'
prType (TypeTuple ts)     = do ts' <- mapM prType ts
                               return $ text "tuple"
                                    <+> text "of"
                                    <+> parens (sep (punctuate comma ts'))
prType (TypeSet t)        = do t' <- prType t
                               return $ text "set"
                                    <+> text "of"
                                    <+> t'
prType (TypeMSet t)       = do t' <- prType t
                               return $ text "mset"
                                    <+> text "of"
                                    <+> t'
prType (TypeFunction a b) = do a' <- prType a
                               b' <- prType b
                               return $ text "function"
                                    <+> a'
                                    <+> text "->"
                                    <+> b'
prType (TypeRelation ts)  = do ts' <- mapM prType ts
                               return $ text "relation"
                                    <+> text "of"
                                    <+> parens (sep (punctuate comma ts'))
prType (TypePartition t)  = do t' <- prType t
                               return $ text "partition"
                                    <+> text "from"
                                    <+> t'
prType (TypeLambda args x) = do args' <- mapM prType args
                                x'    <- prType x
                                return $ text "lambda"
                                     <+> braces (sep (punctuate comma args'))
                                     <+> text "->"
                                     <+> x'


--------------------------------------------------------------------------------
-- Type ------------------------------------------------------------------------
--------------------------------------------------------------------------------

prKind :: Kind -> Maybe Doc
prKind KindUnknown = return $ text "?"
prKind KindDomain  = return $ text "domain"
prKind KindValue   = return $ text "value"
prKind KindExpr    = return $ text "expression"
prKind KindLambda  = return $ text "lambda"
prKind KindFind    = return $ text "find"
prKind KindGiven   = return $ text "given"

prKindInteractive :: Kind -> Doc
prKindInteractive KindUnknown = text "of unknown kind"
prKindInteractive KindDomain  = text "a domain"
prKindInteractive KindValue   = text "a value"
prKindInteractive KindExpr    = text "an expression"
prKindInteractive KindLambda  = text "a lambda"
prKindInteractive KindFind    = text "a decision variable"
prKindInteractive KindGiven   = text "a parameter"


--------------------------------------------------------------------------------
-- RuleRepr --------------------------------------------------------------------
--------------------------------------------------------------------------------

prRuleRepr :: String -> RuleRepr -> Maybe Doc
prRuleRepr filename repr' = do

    let repr = doRenamings ("__" ++ safeStr filename ++ "_") repr'

    let name   =   leadsto <+>        text (reprName repr)
    template   <- (leadsto <+>) <$> prExpr (reprTemplate repr)
    structural <- case reprPrologueStructural repr of
        Nothing -> return empty
        Just t  -> do t' <- prExpr t
                      return $ leadsto <+> t'
    wheres      <- let ws = reprPrologueWheres   repr in mapM (fmap indent . prWhere) ws
    bindings    <- let bs = reprPrologueBindings repr in mapM (fmap indent . prBinding bs) bs
    cases       <- mapM prRuleReprCase (reprCases repr)
    return . vcat . concat $
        [ [ emptyLine  ]
        , [ name       ]
        , [ template   ]
        , [ structural ]
        ,   bindings
        ,   wheres
        , [ emptyLine  ]
        , cases
        ]

prRuleReprCase :: RuleReprCase -> Maybe Doc
prRuleReprCase reprcase = do
    pattern <- (text "***" <+>) <$> prExpr (reprCasePattern reprcase)
    structural <- case reprCaseStructural reprcase of
        Nothing -> return empty
        Just t  -> do t' <- prExpr t
                      return . indent $ leadsto <+> t'
    wheres      <- let ws = reprCaseWheres   reprcase in mapM (fmap indent . prWhere) ws
    bindings    <- let bs = reprCaseBindings reprcase in mapM (fmap indent . prBinding bs) bs
    return . vcat . concat $
        [ [ pattern    ]
        , [ structural ]
        ,   bindings
        ,   wheres
        , [ emptyLine  ]
        ]


doRenamings ::
    ( Biplate a Expr
    , Biplate a (String, Type)
    , Biplate a Binding
    ) => String -> a -> a
doRenamings pre = transformBi renaming1 . transformBi renaming2 . transformBi renaming3
    where
        renameStr :: String -> String
        renameStr s | isPrefixOf pre s = drop (length pre) s
                    | otherwise        = s

        renaming1 :: Expr -> Expr
        renaming1 (Identifier i) = Identifier $ renameStr i
        renaming1 x = x

        renaming2 :: (String,Type) -> (String,Type)
        renaming2 (i,t) = ( renameStr i, t )

        renaming3 :: Binding -> Binding
        renaming3 (bEnum,nm,x) = (bEnum,renameStr nm,x)


emptyLine :: Doc
emptyLine = text ""

indent :: Doc -> Doc
indent = nest 4

leadsto :: Doc
leadsto = text "~~>"
