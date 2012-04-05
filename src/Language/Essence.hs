module Language.Essence ( module All ) where

import Language.Essence.Binding             as All
import Language.Essence.Domain              as All
import Language.Essence.Expr                as All
import Language.Essence.Identifier          as All
import Language.Essence.Lambda              as All
import Language.Essence.Metadata            as All
import Language.Essence.Objective           as All
import Language.Essence.Op                  as All
import Language.Essence.OpDescriptor        as All
import Language.Essence.QuantifiedExpr      as All
import Language.Essence.QuantifierDecl      as All
import Language.Essence.Range               as All
import Language.Essence.RuleRefn            as All
import Language.Essence.RuleRepr            as All
import Language.Essence.Spec                as All
import Language.Essence.StructuredVar       as All
import Language.Essence.Type                as All
import Language.Essence.Value               as All
import Language.Essence.Where               as All



-- runTests :: IO ()
-- runTests = do
--     print =<< runHUnit
--     print =<< runQC
-- 
-- runHUnit :: IO Counts
-- runHUnit = runTestTT (getParsePrintUnitTests (undefined :: Expr))
-- 
-- runQC :: IO Bool
-- runQC = $forAllProperties runner
--     where
--         runner = quickCheckWithResult stdArgs { maxSize = 2, maxSuccess = 1000 }

-- runQC :: Testable prop => prop -> IO Result
-- runQC = quickCheckWithResult stdArgs { maxSize = 2, maxSuccess = 1000 }


-- incr :: Value -> Value
-- incr (VTuple [V (VInt 1),V (VInt 2)]) = VHole $ Identifier "found!"
-- incr (VInt i) = VInt (i+1)
-- incr v = v

-- incrIO :: Value -> IO Value
-- incrIO (VTuple [V (VInt 1),V (VInt 2)]) = do
--     putStrLn "first eq."
--     return $ VHole $ Identifier "found!"
-- incrIO (VInt i) = do
--     putStrLn "second eq."
--     return $ VInt (i+1)
-- incrIO v = do
--     putStrLn "third eq."
--     return v


-- import ParsecUtils
-- import Control.Monad.Writer
-- import GenericOps.Core
-- import ParsePrint ( parse, pretty )
-- import Control.Applicative
-- 
-- -- 1. pattern
-- -- 2. template
-- -- 3. param
-- -- 1 ~ 2 ~ 3
-- gParseAndAply :: String -> String -> String -> IO () -- String
-- gParseAndAply pattern template param = do
--     pattern'  <- parseIO (parse <* eof) pattern
--     -- template' <- parseIO (D <$> (parse <* eof)) template
--     template' <- parseIO (parse <* eof) template
--     param'    <- parseIO (parse <* eof) param
--     (x, logs) <- runWriterT $ gapplyDeep "foo"
--                                     pattern'
--                                     template'
--                                     param'
--     -- ppPrint x
--     mapM_ print logs
--     putStrLn ""
--     print $ pretty (x :: Expr)
--     -- return $ show $ pretty x



-- matchTest :: String -> String -> IO ()
-- matchTest p a = do
--     xp :: Expr <- parseIO (parse <* eof) p
--     xa :: Expr <- parseIO (parse <* eof) a
--     -- binds <- execStateT (runErrorT (gmatch (mkG xp) (mkG xa))) def
--     binds <- runErrorT (execStateT (execStateT (match xp xa) def) def)
--     case binds of
--         Left err -> error err
--         Right (r,_) -> mapM_ (\ (nm,GNode _ x) -> putStrLn (nm ++ ": " ++ show (pretty x)) ) (M.toList r)


-- promoteTest :: Expr -> IO ()
-- promoteTest x = do
--     putStrLn " [ ==================== ]"
--     mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG x
--     putStrLn " [ ==================== ]"
--     mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG $ deepPromote x
--     putStrLn " [ ==================== ]"




-- 
-- 
-- --------------------------------------------------------------------------------
-- -- Coerce instances ------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- 
-- instance Coerce Expr Expr where
--     promote = id
--     demote = Just
-- 
-- instance Coerce Value Expr where
--     promote (VHole x) = EHole x
--     promote x = V x
-- 
--     demote (EHole x) = Just $ VHole x
--     demote (V x) = Just x
--     demote _     = Nothing
-- 
-- instance Coerce Domain Expr where
--     promote (DHole x) = EHole x
--     promote x = D x
-- 
--     demote (EHole x) = Just $ DHole x
--     demote (D x) = Just x
--     demote _     = Nothing
-- 
-- 
-- deepPromote :: GPlate a => a -> a
-- deepPromote = unliftG (bottomUp (liftG f))
--     where
--         f (V x) = promote x
--         f (D x) = promote x
--         f x     = promote x
-- 
-- 
-- 
-- --------------------------------------------------------------------------------
-- -- QuickCheck properties -------------------------------------------------------
-- --------------------------------------------------------------------------------
-- 
-- prop_CoerceExpr :: Expr -> Bool
-- prop_CoerceExpr = propCoerce
-- 
-- prop_CoerceValue :: Value -> Bool
-- prop_CoerceValue = propCoerce
-- 
-- prop_CoerceDomain :: Domain -> Bool
-- prop_CoerceDomain = propCoerce
-- 
-- propCoerce :: (Eq a, Coerce a Expr) => a -> Bool
-- propCoerce x = demote (promote x :: Expr) == Just x
-- 
-- 
-- prop_ParsePrintExpr :: Expr -> Bool
-- prop_ParsePrintExpr = propParsePrint
-- 
-- prop_ParsePrintValue :: Value -> Bool
-- prop_ParsePrintValue = propParsePrint
-- 
-- prop_ParsePrintDomain :: Domain -> Bool
-- prop_ParsePrintDomain = propParsePrint
-- 
-- prop_ParsePrintType :: Type -> Bool
-- prop_ParsePrintType = propParsePrint
-- 
-- propParsePrint :: (Eq a, ParsePrint a, GPlate a) => a -> Bool
-- propParsePrint a = Right a == parseEither (parse <* eof) (show $ pretty a)
-- 
