{-# LANGUAGE FlexibleContexts #-}

module Phases.Repr where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Data.Default ( def )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import Language.EssenceParsers
import Language.EssenceEvaluator
import PrintUtils
import ParsecUtils
import Utils -- ( runRWSE )


type Domain = Expr
type Structural = Maybe Expr
type ErrMsg = String

runApplyToDom :: Domain -> RuleRepr -> (Either String (Domain, Structural), [Log])
runApplyToDom dom repr = runRWSE () [] $ applyToDom dom repr

applyToDom ::
    ( MonadError String m
    , MonadWriter [Log] m
    , MonadState [Binding] m
    ) => Domain
      -> RuleRepr
      -> m (Domain, Structural)
applyToDom dom repr = do
    results <- mapM (applyCaseToDom dom) cases
    result  <- firstRight results
    case result of
        Nothing -> throwError "Cannot apply repr rule to domain."
        Just r  -> return r
    where
        cases :: [RuleReprCase]
        cases = flip map (reprCases repr) $ \ c -> c { reprCaseStructural = conjunct <$> reprCaseStructural c <*> reprPrologueStructural repr
                                                     , reprCaseWheres     = reprCaseWheres   c ++ reprPrologueWheres   repr
                                                     , reprCaseBindings   = reprCaseBindings c ++ reprPrologueBindings repr
                                                     }


applyCaseToDom ::
    ( MonadWriter [Log] m
    , MonadState [Binding] m
    ) => Domain -> RuleReprCase -> m (Either String (Domain, Structural))
applyCaseToDom dom reprcase = runErrorT $ do
    initBindings <- get
    matchPattern (reprCasePattern reprcase) dom
    mapM_ checkWheres $ reprCaseWheres reprcase
    b <- get
    tell $ map show b
    put initBindings
    return (dom, Nothing)


matchPattern ::
    ( MonadError String m
    , MonadState [Binding] m
    ) => Domain
      -> Domain
      -> m ()

matchPattern (Identifier nm) dom = addBinding InRule nm dom

matchPattern DomainBoolean
             DomainBoolean
             = return ()

matchPattern (DomainIntegerFromTo Nothing Nothing)
             (DomainIntegerFromTo Nothing Nothing)
             = return ()
matchPattern (DomainIntegerFromTo Nothing (Just t1))
             (DomainIntegerFromTo Nothing (Just t2))
             = matchPattern t1 t2
matchPattern (DomainIntegerFromTo (Just f1) Nothing)
             (DomainIntegerFromTo (Just f2) Nothing)
             = matchPattern f1 f2
matchPattern (DomainIntegerFromTo (Just f1) (Just t1))
             (DomainIntegerFromTo (Just f2) (Just t2))
             = matchPattern f1 f2 >> matchPattern t1 t2

matchPattern p@(DomainIntegerList xs)
             v@(DomainIntegerList ys)
             = if length xs == length ys
                 then zipWithM_ matchPattern xs ys
                 else matchPatternError p v

matchPattern (DomainUnnamed s1 _)
             (DomainUnnamed s2 _)
             = matchPattern s1 s2

matchPattern (DomainMatrix i1 e1)
             (DomainMatrix i2 e2)
             = matchPattern i1 i2 >> matchPattern e1 e2

matchPattern p@(DomainTuple xs _)
             v@(DomainTuple ys _)
             = if length xs == length ys
                 then zipWithM_ matchPattern xs ys
                 else matchPatternError p v

matchPattern (DomainSet s1 mns1 mxs1 dontcare e1 _)
             (DomainSet s2 mns2 mxs2 _        e2 _)
             = matchIfJust dontcare s1 s2 >>
               matchIfJust dontcare mns1 mns2 >>
               matchIfJust dontcare mxs1 mxs2 >>
               matchPattern e1 e2

-- TODO: add rest of the domains here!

matchPattern pattern value = matchPatternError pattern value


matchPatternError ::
    ( MonadError String m
    , MonadState [Binding] m
    ) => Domain -> Domain -> m ()
matchPatternError pattern value = do
    pattern' <- maybe (throwError ("cannot render: " ++ show pattern)) return $ prExpr pattern
    value'   <- maybe (throwError ("cannot render: " ++ show value  )) return $ prExpr value
    throwError . render $ text "matchPattern:" <+> pattern'
                                               <+> text "~~"
                                               <+> value'


matchIfJust ::
    ( MonadError [Char] m
    , MonadState [Binding] m
    ) => Bool
      -> Maybe Domain
      -> Maybe Domain
      -> m ()
matchIfJust _     Nothing  Nothing   = return ()
matchIfJust _     (Just i) (Just j)  = matchPattern i j
matchIfJust True  Nothing  _         = return ()
matchIfJust False Nothing  (Just {}) = throwError "missing attribute in pattern."
matchIfJust _     (Just {}) _        = throwError "extra attribute in pattern."


addBinding :: MonadState [Binding] m => BindingEnum -> String -> Expr -> m ()
addBinding e nm x = modify ((e,nm,x) :)


checkWheres ::
    ( MonadError String m
    , MonadState [Binding] m
    ) => Where -> m ()
checkWheres x = do
    bindings <- get
    let (x',logs) = runEvaluateExpr bindings x
    case x' of
        ValueBoolean True  -> return ()
        ValueBoolean False -> do
            xOut <- maybe (throwError ("cannot render: " ++ show x)) return $ prExpr x
            throwError . render $ text "where statement evaluated to false:" <+> xOut
        _                  -> do
            xOut <- maybe (throwError ("cannot render: " ++ show x)) return $ prExpr x
            throwError . render $ text "where statement cannot be fully evaluated:" <+> xOut
                              $+$ vcat (map text logs)


------------------------------------------------------------
-- helper functions ----------------------------------------
------------------------------------------------------------

conjunct :: Expr -> Expr -> Expr
conjunct x y = GenericNode And [x,y]

firstRight :: MonadWriter [Log] m => [Either Log r] -> m (Maybe r)
firstRight []            = return Nothing
firstRight (Right r :_ ) = return (Just r)
firstRight (Left err:es) = tell [err] >> firstRight es
