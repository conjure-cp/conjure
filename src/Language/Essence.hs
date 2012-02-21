{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.Essence where

import GenericOps.Coerce
import GenericOps.Core
import ParsecUtils
import ParsePrint ( ParsePrint(..), fromPairs, prettyList, prettyListDoc )
import PrintUtils ( (<+>), (<>), text )
import qualified PrintUtils as Pr
import Utils ( padRight )

-- import Control.Monad
-- import Data.Char
-- import Data.List
-- import Debug.Trace
import Control.Arrow ( first )
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Data.Default ( def )
import Data.List
import Data.Generics ( Data )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary(..), Testable, choose, elements, quickCheckWith, stdArgs, Args(..) )
import Test.QuickCheck.Gen ( oneof )
-- import Unsafe.Coerce ( unsafeCoerce )



runQC :: Testable prop => prop -> IO ()
runQC = quickCheckWith stdArgs { maxSize = 3, maxSuccess = 1000 }


incr :: Value -> Value
incr (VTuple [V (VInt 1),V (VInt 2)]) = VHole $ Identifier "found!"
incr (VInt i) = VInt (i+1)
incr v = v

incrIO :: Value -> IO Value
incrIO (VTuple [V (VInt 1),V (VInt 2)]) = do
    putStrLn "first eq."
    return $ VHole $ Identifier "found!"
incrIO (VInt i) = do
    putStrLn "second eq."
    return $ VInt (i+1)
incrIO v = do
    putStrLn "third eq."
    return v

pr :: GNode -> IO GNode
pr g = do
    putStrLn $ showG g
    return g


matchTest :: String -> String -> IO ()
matchTest p a = do
    xp :: Expr <- parseIO (parse <* eof) p
    xa :: Expr <- parseIO (parse <* eof) a
    -- binds <- execStateT (runErrorT (gmatch (mkG xp) (mkG xa))) def
    binds <- runErrorT (execStateT (match xp xa) def)
    case binds of
        Left err -> error err
        Right (r,_) -> mapM_ (\ (nm,GNode _ x) -> putStrLn (nm ++ ": " ++ show (pretty x)) ) r


promoteTest :: Expr -> IO ()
promoteTest x = do
    putStrLn " [ ==================== ]"
    mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG x
    putStrLn " [ ==================== ]"
    mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG $ deepPromote x
    putStrLn " [ ==================== ]"


errorArbitrary :: a
errorArbitrary = error "in Arbitrary"



--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Expr = EHole Identifier
    | V Value
    | D Domain
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Expr

instance Hole Expr where
    hole (EHole (Identifier "_")) = UnnamedHole
    hole (EHole (Identifier nm) ) = NamedHole nm
    hole _           = NotAHole

instance GPlate Expr where
    gplate p@(EHole {}) = gplateLeaf p
    gplate (V x) = gplateSingle V x
    gplate (D x) = gplateSingle D x

instance MatchBind Expr

instance ParsePrint Expr where
    parse = choiceTry
        [ EHole <$> parse
        , V     <$> parse
        , D     <$> parse
        ]
    pretty (EHole x) = pretty x
    pretty (V     x) = pretty x
    pretty (D     x) = pretty x

instance Arbitrary Expr where
    arbitrary = deepPromote <$> oneof
        [ EHole <$> arbitrary
        , V     <$> arbitrary
        , D     <$> arbitrary
        ]
    shrink (V x) = map V $ shrink x
    shrink (D x) = map D $ shrink x
    shrink _     = []



--------------------------------------------------------------------------------
-- Identifier ------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Identifier = Identifier String
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Identifier

instance Hole Identifier

instance GPlate Identifier

instance MatchBind Identifier

instance ParsePrint Identifier where
    parse = Identifier <$> identifier
    pretty (Identifier nm) = text nm

instance Arbitrary Identifier where
    arbitrary = Identifier . return <$> choose ('a', 'z')



--------------------------------------------------------------------------------
-- Value -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Value = VHole Identifier
    | VBool   Bool
    | VInt   Integer
    | VMatrix    [Expr]         -- uniform type.
    | VTuple     [Expr]
    | VSet       [Expr]         -- uniform type. unique.
    | VMSet      [Expr]         -- uniform type.
    | VFunction  [Expr]         -- VTuple#2. uniform type.
    | VRelation  [Expr]         -- VTuple. uniform type.
    | VPartition [Expr]         -- VSet. uniform type.
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Value

instance Hole Value where
    hole (VHole (Identifier "_")) = UnnamedHole
    hole (VHole (Identifier nm) ) = NamedHole nm
    hole _           = NotAHole

instance GPlate Value where
    gplate p@(VHole {}) = gplateLeaf p
    gplate p@(VBool {}) = gplateLeaf p
    gplate p@(VInt  {}) = gplateLeaf p
    gplate   (VMatrix    xs) = gplateUniList VMatrix    xs
    gplate   (VTuple     xs) = gplateUniList VTuple     xs
    gplate   (VSet       xs) = gplateUniList VSet       xs
    gplate   (VMSet      xs) = gplateUniList VMSet      xs
    gplate   (VFunction  xs) = gplateUniList VFunction  xs
    gplate   (VRelation  xs) = gplateUniList VRelation  xs
    gplate   (VPartition xs) = gplateUniList VPartition xs

instance MatchBind Value

instance ParsePrint Value where
    parse = choiceTry
                [ pHole, pBool, pInt
                , pMatrix, pTuple, pSet, pMSet
                , pFunction, pRelation, pPartition
                ]
        where
            pHole = VHole <$> parse

            pBool = VBool False <$ reserved "false"
                    <|>
                    VBool True  <$ reserved "true"

            pInt = VInt <$> integer

            pMatrix = VMatrix <$> brackets (sepBy parse comma)

            pTuple = try (do reserved "tuple"; VTuple <$> parens (sepBy parse comma))
                     <|>
                     VTuple <$> parens (countSepAtLeast 2 parse comma)

            pSet = do reserved "set"; VSet <$> braces (sepBy parse comma)

            pMSet = do reserved "mst"; VMSet <$> braces (sepBy parse comma)

            pFunction = do reserved "function"; VFunction <$> braces (sepBy pTuple2 comma)
                where
                    pTuple2 :: Parser Expr
                    pTuple2 = do
                        i <- parse
                        reservedOp "->"
                        j <- parse
                        return $ V (VTuple [i,j])

            pRelation = do reserved "relation"; VRelation <$> braces (sepBy (V <$> pTuple) comma)

            pPartition = do reserved "partition"; VPartition <$> braces (sepBy aPart comma)
                where
                    aPart :: Parser Expr
                    aPart = (V . VSet) <$> braces (sepBy parse comma)

    pretty (VHole (Identifier nm)) = text nm
    pretty (VBool False) = text "false"
    pretty (VBool True ) = text "true"
    pretty (VInt  i    ) = Pr.integer i
    pretty (VMatrix xs) = prettyList Pr.brackets Pr.comma xs
    pretty (VTuple [] ) = text "tuple ()"
    pretty (VTuple [x]) = text "tuple" <+> Pr.parens (pretty x)
    pretty (VTuple xs ) = prettyList Pr.parens Pr.comma xs
    pretty (VSet  xs) = text "set" <+> prettyList Pr.braces Pr.comma xs
    pretty (VMSet xs) = text "mset" <+> prettyList Pr.braces Pr.comma xs
    pretty (VFunction xs) = text "function" <+> prettyListDoc Pr.braces Pr.comma (map prE xs)
        where
            prE (V (VTuple [i,j])) = pretty i <+> text "->" <+> pretty j
            prE p = pretty p
    pretty (VRelation  xs) = text "relation"  <+> prettyList Pr.braces Pr.comma xs
    pretty (VPartition xs) = text "partition" <+> prettyListDoc Pr.braces Pr.comma (map prE xs)
        where
            prE (V (VSet vs)) = prettyList Pr.braces Pr.comma vs
            prE p = pretty p

instance Arbitrary Value where
    arbitrary = deepPromote . VHole <$> arbitrary



--------------------------------------------------------------------------------
-- Domain ----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Domain = DHole Identifier
    | DBool
    | DInt                (Range Expr)
    | DEnum    Identifier (Range Identifier)
    | DUnnamed Identifier
    | DMatrix  Domain Domain
    | AnyDom { dConstr  :: AnyDomEnum
             , dElement :: [Domain]
             , dAttrs   :: [DomainAttr]
             }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Domain

instance Hole Domain where
    hole (DHole (Identifier "_")) = UnnamedHole
    hole (DHole (Identifier nm) ) = NamedHole nm
    hole _           = NotAHole

instance GPlate Domain where
    gplate p@(DHole {}) = gplateLeaf p
    gplate p@(DBool {}) = gplateLeaf p
    gplate (DInt x) = gplateSingle DInt x
    gplate (DEnum nm x) =
        ( [mkG nm, mkG x]
        , \ xs ->
            case xs of
                [mnm,mx] ->
                    case (fromG mnm, fromG mx) of
                        (Just nm', Just x') -> DEnum nm' x'
                        _                   -> gplateError
                _ -> gplateError
        )
    gplate (DUnnamed x) = gplateSingle DUnnamed x
    gplate (DMatrix i e) = gplateUniList (\ [i',e'] -> DMatrix i' e' ) [i,e]
    gplate (AnyDom nm es as) = 
        ( mkG nm : map mkG es ++ map mkG as
        , \ xs -> let nm' = fromGs $ take 1 xs
                      es' = fromGs $ take (length es) $ drop 1 xs
                      as' = fromGs $ take (length as) $ drop (length es) $ drop 1 xs
                  in  if length nm' == 1 &&
                         length es' == length es &&
                         length as' == length as
                          then AnyDom (head nm') es' as'
                          else gplateError
        )

instance MatchBind Domain

instance ParsePrint Domain where
    parse = choiceTry
                [ pBool, pInt, pEnum, pUnnamed, pMatrix
                , pTuple, pSetMSet "set" DSet, pSetMSet "mset" DMSet
                , pFunction, pRelation, pPartition
                , pDHole
                ]
        where
            pDHole = DHole <$> parse

            pBool = DBool <$ reserved "bool"

            pInt     = do reserved "int" ; DInt  <$>           (try parse <|> return RAll)

            pEnum    = do reserved "enum"; DEnum <$> parse <*> (try parse <|> return RAll)

            -- needed to disambiguate from DHole
            -- DHole can still be resolved to DUnnamed, after parsing.
            pUnnamed = do reserved "unnamed";  DUnnamed <$> parse

            pMatrix = do
                reserved "matrix"
                reserved "indexed"
                reserved "by"
                is <- brackets (parse `sepBy1` comma)
                reserved "of"
                e  <- parse
                return $ foldr DMatrix e is

            pTuple = do
                reserved "tuple"
                as <- pDomainAttrs
                reserved "of"
                es <- parens (parse `sepBy` comma)
                return $ AnyDom DTuple es as

            pSetMSet kw en = do
                reserved kw
                as <- pDomainAttrs
                reserved "of"
                e  <- parse
                return $ AnyDom en [e] as

            pFunction = do
                reserved "function"
                as <- pDomainAttrs
                fr <- parse
                reservedOp "->"
                to <- parse
                return $ AnyDom DFunction [fr,to] as

            pRelation = do
                reserved "relation"
                as <- pDomainAttrs
                reserved "of"
                es <- parens (parse `sepBy` (reservedOp "*"))
                return $ AnyDom DRelation es as

            pPartition = do
                reserved "partition"
                as <- pDomainAttrs
                reserved "from"
                e  <- parse
                return $ AnyDom DPartition [e] as

    pretty (DHole (Identifier nm)) = text nm
    pretty DBool = text "bool"
    pretty (DInt RAll) = text "int"
    pretty (DInt r   ) = text "int" <> pretty r
    pretty (DEnum i RAll) = text "enum" <+> pretty i
    pretty (DEnum i r   ) = text "enum" <+> pretty i <> pretty r
    pretty (DUnnamed i) = text "unnamed" <+> pretty i
    pretty (DMatrix i e) = text "matrix" <+> text "indexed"
                       <+> text "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> text "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of DMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (AnyDom DTuple es as) = text "tuple" <+> prDomainAttrs as <+> text "of"
                                                <+> prettyList Pr.parens Pr.comma es
    pretty (AnyDom DSet  [e] as) = text "set"  <+> prDomainAttrs as <+> text "of" <+> pretty e
    pretty (AnyDom DMSet [e] as) = text "mset" <+> prDomainAttrs as <+> text "of" <+> pretty e
    pretty (AnyDom DFunction [fr,to] as) = text "function"  <+> prDomainAttrs as <+> pretty fr <+> text "->" <+> pretty to
    pretty (AnyDom DRelation es as) = text "relation" <+> prDomainAttrs as <+> text "of"
                                                      <+> prettyList Pr.parens (text "*") es
    pretty (AnyDom DPartition [e] as) = text "partition" <+> prDomainAttrs as <+> text "from" <+> pretty e
    pretty p = error ("Invalid domain: " ++ show p)

instance Arbitrary Domain where
    arbitrary = deepPromote <$> oneof
        [ DHole    <$> arbitrary
        , return DBool
        , DInt     <$> arbitrary
        , DEnum    <$> arbitrary <*> arbitrary
        , DUnnamed <$> arbitrary
        , DMatrix  <$> arbitrary <*> arbitrary
        , AnyDom DTuple     <$> arbitrary              <*> return []
        , AnyDom DSet       <$> (return <$> arbitrary) <*> (sort . nub <$> arbitrary)
        , AnyDom DMSet      <$> (return <$> arbitrary) <*> (sort . nub <$> arbitrary)
        , do (fr,to) <- arbitrary; as <- arbitrary; return $ AnyDom DFunction [fr,to] (sort (nub as))
        , AnyDom DRelation  <$> arbitrary              <*> (sort . nub <$> arbitrary)
        , AnyDom DPartition <$> (return <$> arbitrary) <*> (sort . nub <$> arbitrary)
        ]



data Range a = RAll | RList [a] | RFromTo (Maybe a) (Maybe a)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Data a =>  NodeTag (Range a)

instance Hole (Range a)

instance (Eq a, Show a, Data a, GPlate a) => GPlate (Range a) where
    gplate RAll = gplateLeaf RAll
    gplate (RList xs) = gplateUniList RList xs
    gplate p@(RFromTo Nothing  Nothing ) = gplateLeaf p
    gplate   (RFromTo Nothing  (Just y)) = gplateSingle  (\ y'      -> RFromTo Nothing   (Just y') ) y
    gplate   (RFromTo (Just x) Nothing ) = gplateSingle  (\ x'      -> RFromTo (Just x') Nothing   ) x
    gplate   (RFromTo (Just x) (Just y)) = gplateUniList (\ [x',y'] -> RFromTo (Just x') (Just y') ) [x,y]

instance MatchBind a => MatchBind (Range a)

instance ParsePrint a => ParsePrint (Range a) where
    parse = parens (try pRList <|> pRFromTo)
        where
            pRList = do
                i <- optionMaybe parse
                dot; dot
                j <- optionMaybe parse
                return $ RFromTo i j
            pRFromTo = RList <$> sepBy parse comma
    pretty RAll = error "do not call pretty Range.RAll"
    pretty (RList      xs) = prettyList Pr.parens Pr.comma xs
    pretty (RFromTo fr to) = Pr.parens (fr' <> text ".." <> to')
        where
            fr' = fromMaybe Pr.empty (pretty <$> fr)
            to' = fromMaybe Pr.empty (pretty <$> to)

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RAll
        , RList   <$> arbitrary
        , RFromTo <$> arbitrary <*> arbitrary
        ]



data AnyDomEnum = DTuple | DSet | DMSet | DFunction | DRelation | DPartition
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag AnyDomEnum

instance Hole AnyDomEnum

instance GPlate AnyDomEnum

instance MatchBind AnyDomEnum

instance ParsePrint AnyDomEnum where
    isoParsePrint = fromPairs
                        [ ( DTuple    , "tuple"     )
                        , ( DSet      , "set"       )
                        , ( DMSet     , "mset"      )
                        , ( DFunction , "function"  )
                        , ( DRelation , "set"       )
                        , ( DPartition, "partition" )
                        ]

instance Arbitrary AnyDomEnum where
    arbitrary = elements [minBound .. maxBound]



data DomainAttr
    = OnlyName DomainAttrEnum
    | NameValue DomainAttrEnum Expr
    | DontCare
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag DomainAttr

instance Hole DomainAttr

instance GPlate DomainAttr where
    gplate (OnlyName e) = gplateSingle OnlyName e
    gplate (NameValue e x) =
        ( [mkG e, mkG x]
        , \ ex ->
            case ex of
                [me,mx] ->
                    case (fromG me, fromG mx) of
                        (Just e', Just x') -> NameValue e' x'
                        _ -> gplateError
                _ -> gplateError
        )
    gplate p@(DontCare {}) = gplateLeaf p

instance MatchBind DomainAttr

pDomainAttrs :: Parser [DomainAttr]
pDomainAttrs = fromMaybe [] <$> optionMaybe (parens (parse `sepBy` comma))

prDomainAttrs :: [DomainAttr] -> Pr.Doc
prDomainAttrs [] = Pr.empty
prDomainAttrs xs = prettyList Pr.parens Pr.comma xs

instance ParsePrint DomainAttr where
    parse = choiceTry [pNameValue, pOnlyName, pDontCare]
        where
            pOnlyName  = OnlyName  <$> parse
            pNameValue = NameValue <$> parse <*> parse
            pDontCare  = DontCare  <$  reservedOp "_"
    pretty (OnlyName e) = pretty e
    pretty (NameValue e x) = pretty e <+> pretty x
    pretty DontCare = text "_"

instance Arbitrary DomainAttr where
    arbitrary = oneof
        [ OnlyName  <$> arbitrary
        , NameValue <$> arbitrary <*> arbitrary
        , return DontCare
        ]



data DomainAttrEnum
    = AttrRepresentation
    | AttrSize
    | AttrMinSize
    | AttrMaxSize
    | AttrOccr
    | AttrMinOccr
    | AttrMaxOccr
    | AttrTotal
    | AttrPartial
    | AttrInjective
    | AttrSurjective
    | AttrBijective
    | AttrRegular
    | AttrComplete
    | AttrPartSize
    | AttrMinPartSize
    | AttrMaxPartSize
    | AttrNumParts
    | AttrMinNumParts
    | AttrMaxNumParts
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag DomainAttrEnum

instance Hole    DomainAttrEnum

instance GPlate  DomainAttrEnum

instance MatchBind DomainAttrEnum

instance ParsePrint DomainAttrEnum where
    isoParsePrint = fromPairs
            [ ( AttrRepresentation , "representation" )
            , ( AttrSize           , "size"           )
            , ( AttrMinSize        , "minSize"        )
            , ( AttrMaxSize        , "maxSize"        )
            , ( AttrOccr           , "occr"           )
            , ( AttrMinOccr        , "minOccr"        )
            , ( AttrMaxOccr        , "maxOccr"        )
            , ( AttrTotal          , "total"          )
            , ( AttrPartial        , "partial"        )
            , ( AttrInjective      , "injective"      )
            , ( AttrSurjective     , "surjective"     )
            , ( AttrBijective      , "bijective"      )
            , ( AttrRegular        , "regular"        )
            , ( AttrComplete       , "complete"       )
            , ( AttrPartSize       , "partSize"       )
            , ( AttrMinPartSize    , "minPartSize"    )
            , ( AttrMaxPartSize    , "maxPartSize"    )
            , ( AttrNumParts       , "numParts"       )
            , ( AttrMinNumParts    , "minNumParts"    )
            , ( AttrMaxNumParts    , "maxNumParts"    )
            ]

instance Arbitrary DomainAttrEnum where
    arbitrary = elements [minBound .. maxBound]



--------------------------------------------------------------------------------
-- Type ------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Type = THole Identifier
    | TBool
    | TInt
    | TEnum (Maybe [Identifier])
    | TMatrix Type Type
    | TLambda [Type] Type
    | AnyType AnyTypeEnum [Type]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Type

instance Hole Type where
    hole (THole (Identifier "_")) = UnnamedHole
    hole (THole (Identifier nm) ) = NamedHole nm
    hole _           = NotAHole

instance GPlate Type where
    gplate p@(THole {}) = gplateLeaf p
    gplate p@(TBool {}) = gplateLeaf p
    gplate p@(TInt  {}) = gplateLeaf p
    gplate p@(TEnum {}) = gplateLeaf p
    gplate (TMatrix i  e) = gplateUniList (\ [i',e']  -> TMatrix i' e'  ) [i,e]
    gplate (TLambda is o) = gplateUniList (\ (o':is') -> TLambda is' o' ) (o:is)
    gplate (AnyType e ts) = gplateUniList (AnyType e) ts

instance MatchBind Type

instance ParsePrint Type where
    parse = choiceTry
                [ pTHole, pTBool, pTInt, pEnum, pMatrix
                , pTTuple, pTSet, pTMSet
                , pTFunction, pTRelation, pTPartition
                , pTLambda
                ]
        where
            pTHole  = THole <$> parse
            pTBool  = TBool <$  reserved "bool"
            pTInt   = TInt  <$  reserved "int"
            pEnum   = do reserved "enum" ; TEnum <$> optionMaybe (braces (sepBy parse comma))
            pMatrix = do
                reserved "matrix"
                reserved "indexed"
                reserved "by"
                is <- brackets (parse `sepBy1` comma)
                reserved "of"
                e  <- parse
                return $ foldr TMatrix e is
            pTTuple = do reserved "tuple"; reserved "of"; AnyType TTuple <$> parens (sepBy parse comma)
            pTSet   = do reserved "set"  ; reserved "of"; AnyType TSet  . return <$> parse
            pTMSet  = do reserved "mset" ; reserved "of"; AnyType TMSet . return <$> parse
            pTFunction = do
                reserved "function"
                fr <- parse
                reservedOp "->"
                to <- parse
                return (AnyType TFunction [fr,to])
            pTRelation  = do reserved "relation" ; reserved "of"  ; AnyType TRelation  <$> parens (sepBy parse (reservedOp "*"))
            pTPartition = do reserved "partition"; reserved "from"; AnyType TPartition . return <$> parse
            pTLambda = do
                reserved "lambda"
                braces $ do
                    is <- sepBy1 parse comma
                    reservedOp "->"
                    o  <- parse
                    return (TLambda is o)

    pretty (THole i) = pretty i
    pretty TBool = text "bool"
    pretty TInt  = text "int"
    pretty (TEnum Nothing  ) = text "enum"
    pretty (TEnum (Just xs)) = text "enum" <+> prettyList Pr.braces Pr.comma xs
    pretty (TMatrix i e) = text "matrix" <+> text "indexed"
                       <+> text "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> text "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of TMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (TLambda is  o) = text "lambda" <+> Pr.braces (prettyList id Pr.comma is <+> text "->" <+> pretty o)
    pretty (AnyType TTuple ts) = text "tuple" <+> text "of" <+> prettyList Pr.parens Pr.comma ts
    pretty (AnyType TSet  [t]) = text "set"  <+> text "of" <+> pretty t
    pretty (AnyType TMSet [t]) = text "mset" <+> text "of" <+> pretty t
    pretty (AnyType TFunction [fr,to]) = text "function" <+> pretty fr <+> text "->" <+> pretty to
    pretty (AnyType TRelation  ts ) = text "relation"  <+> text "of"   <+> prettyList Pr.parens (text "*") ts
    pretty (AnyType TPartition [t]) = text "partition" <+> text "from" <+> pretty t
    pretty p = error ("Invalid type: " ++ show p)

instance Arbitrary Type where
    arbitrary = oneof
        [ THole <$> arbitrary
        , return TBool
        , return TInt
        , TEnum <$> arbitrary
        , TMatrix <$> arbitrary <*> arbitrary
        , do (i,is,o) <- arbitrary; return $ TLambda (i:is) o
        , AnyType TTuple              <$> arbitrary
        , AnyType TSet  . return      <$> arbitrary
        , AnyType TMSet . return      <$> arbitrary
        , do (fr,to)  <- arbitrary; return $ AnyType TFunction [fr,to]
        , AnyType TRelation           <$> arbitrary
        , AnyType TPartition . return <$> arbitrary
        ]
    -- shrink (TLambda is o) = do
    --     is' <- shrink is
    --     o'  <- shrink o
    --     THole (Identifier "_") : o' : is' ++ map (\ t -> TLambda t o') (drop 1 $ take (length is) $ inits is')
    -- shrink (AnyType enum is) | enum `elem` [TTuple, TRelation] = do
    --     is' <- shrink is
    --     THole (Identifier "_") : is' ++ map (AnyType enum) (take (length is) $ inits is')
    -- shrink (AnyType enum ts) = do
    --     ts' <- shrink ts
    --     return $ AnyType enum ts'
    -- shrink _ = []



data AnyTypeEnum = TTuple | TSet | TMSet | TFunction | TRelation | TPartition
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag AnyTypeEnum

instance Hole    AnyTypeEnum

instance GPlate  AnyTypeEnum

instance MatchBind AnyTypeEnum

instance ParsePrint AnyTypeEnum where
    isoParsePrint = fromPairs
                        [ ( TTuple    , "tuple"     )
                        , ( TSet      , "set"       )
                        , ( TMSet     , "mset"      )
                        , ( TFunction , "function"  )
                        , ( TRelation , "set"       )
                        , ( TPartition, "partition" )
                        ]

instance Arbitrary AnyTypeEnum where
    arbitrary = elements [minBound .. maxBound]



--------------------------------------------------------------------------------
-- Coerce instances ------------------------------------------------------------
--------------------------------------------------------------------------------

instance Coerce Expr Expr where
    promote = id
    demote = Just

instance Coerce Value Expr where
    promote (VHole x) = EHole x
    promote x = V x

    demote (EHole x) = Just $ VHole x
    demote (V x) = Just x
    demote _     = Nothing

instance Coerce Domain Expr where
    promote (DHole x) = EHole x
    promote x = D x

    demote (EHole x) = Just $ DHole x
    demote (D x) = Just x
    demote _     = Nothing


deepPromote :: GPlate a => a -> a
deepPromote = unliftG (bottomUp (liftG f))
    where
        f (V x) = promote x
        f (D x) = promote x
        f x     = promote x


--------------------------------------------------------------------------------
-- QuickCheck properties -------------------------------------------------------
--------------------------------------------------------------------------------

propCoerceExpr :: Expr -> Bool
propCoerceExpr = propCoerce

propCoerceValue :: Value -> Bool
propCoerceValue = propCoerce

propCoerceDomain :: Domain -> Bool
propCoerceDomain = propCoerce

propCoerce :: (Eq a, Coerce a Expr) => a -> Bool
propCoerce x = demote (promote x :: Expr) == Just x


propParsePrintExpr :: Expr -> Bool
propParsePrintExpr = propParsePrint

propParsePrintValue :: Value -> Bool
propParsePrintValue = propParsePrint

propParsePrintDomain :: Domain -> Bool
propParsePrintDomain = propParsePrint

propParsePrintType :: Type -> Bool
propParsePrintType = propParsePrint

propParsePrint :: (Eq a, ParsePrint a, GPlate a) => a -> Bool
propParsePrint a = Just a == parseMaybe (parse <* eof) (show $ pretty a)


