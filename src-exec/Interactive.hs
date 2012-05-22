{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad ( (>=>), forM_, when, void )
import Control.Monad.Error ( runErrorT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State ( MonadState, get, gets, put, StateT, evalStateT, execStateT )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Writer ( runWriter )
import Data.Char ( toLower )
import Data.Default ( def )
import Data.Either ( lefts )
import Data.List ( delete, intercalate, isPrefixOf )
import Data.List.Split ( splitOn )
import System.Console.Haskeline ( InputT, runInputT, getInputLine , Settings(..) )
import System.Console.Haskeline.Completion ( completeFilename )
import System.Environment ( getArgs )
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

import Paths_conjure_cp ( getBinDir )

import Constants ( figlet )
import Nested ( Nested(..), nestedToDoc )
import GenericOps.Core ( GPlate, GNode(..), runMatch, BindingsMap )
import Language.EssenceLexerP ( eof, parseEither )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( Doc, nest, vcat, text, (<+>), ($$) )
import Utils ( fromJust, ppPrint, strip )

import Language.Essence
import Language.Essence.Phases.CleanUp ( quanRenameFinal )
import Language.Essence.Phases.ReadIn ( runReadIn )
import Language.Essence.Phases.ToETyped ( toETypedG )
import Language.EssenceEvaluator ( deepSimplify )



data Command = EvalTypeKind String
             | Evaluate String
             | TypeOf String
             | KindOf String
             | ShowFullAST
             | ShowAST String
             | Load FilePath
             | Save FilePath
             | Record String        -- might record a declaration, where clause, objective, or constraint
             | RmDeclaration String
             | RmConstraint String
             | RmObjective
             | Rollback
             | DisplaySpec
             | TestExprMatch String
             | TestDomMatch String
             | Quit
    deriving (Eq, Ord, Read, Show)


-- given the input line, returns either an error message or Command to be
-- excuted.
parseCommand :: String -> Either String Command
parseCommand s =
    case strip s of
        (':':ss) -> do
            firstWord <- case words ss of []    -> Right ""
                                          (i:_) -> Right $ map toLower i
            let restOfLine = strip $ drop (length firstWord) ss
            let actions = [ ( "evaluate"      , Evaluate      restOfLine )
                          , ( "typeof"        , TypeOf        restOfLine )
                          , ( "kindof"        , KindOf        restOfLine )
                          , ( "showast"       , ShowAST       restOfLine )
                          , ( "fullshowast"   , ShowFullAST              )
                          , ( "load"          , Load          restOfLine )
                          , ( "save"          , Save          restOfLine )
                          , ( "record"        , Record        restOfLine )
                          , ( "rmdeclaration" , RmDeclaration restOfLine )
                          , ( "rmconstraint"  , RmConstraint  restOfLine )
                          , ( "rmobjective"   , RmObjective              )
                          , ( "rollback"      , Rollback                 )
                          , ( "displayspec"   , DisplaySpec              )
                          , ( "testexprmatch" , TestExprMatch restOfLine )
                          , ( "testdommatch"  , TestDomMatch  restOfLine )
                          , ( "quit"          , Quit                     )
                          ]
            case filter (\ (i,_) -> isPrefixOf firstWord i ) actions of
                []        -> Left "no such action"
                [(_,act)] -> Right act
                xs        -> Left $ "ambigious: " ++ intercalate ", " (map fst xs) ++ "?"
        line -> Right $ EvalTypeKind line


data REPLState = REPLState { currentSpec :: Spec
                           , oldSpecs    :: [Spec]
                           , flags       :: [REPLFlag]
                           }
    deriving (Eq, Ord, Read, Show)

data REPLFlag = PrintLogs
    deriving (Eq, Ord, Read, Show)


initREPLState :: REPLState
initREPLState = REPLState { currentSpec = sp
                          , oldSpecs    = []
                          , flags       = []
                          }
    where
        sp :: Spec
        sp = Spec { language    = "Essence"
                  , version     = [2,0]
                  , topLevels   = []
                  , objective   = Nothing
                  , constraints = []
                  , metadata    = []
                  }

modifySpec :: MonadState REPLState m => (Spec -> Spec) -> m ()
modifySpec f = do
    st <- get
    let sp  = currentSpec st
    let sp' = f sp
    put st { currentSpec = sp'
           , oldSpecs    = sp : oldSpecs st
           }


returningTrue :: Applicative f => f () -> f Bool
returningTrue f = pure True <* f

withParsed :: (Applicative m, MonadIO m, ParsePrint a) => String -> (a -> m ()) -> m Bool
withParsed s comp = returningTrue $ case parseEither (parse <* eof) s of
    Left msg -> liftIO $ print $ nestedToDoc msg
    Right x  -> comp x

withParsedGPlate :: (Applicative m, MonadIO m, ParsePrint a, GPlate a) => String -> (a -> m ()) -> m Bool
withParsedGPlate s comp = returningTrue $ case (parseEither (parse <* eof) >=> toETypedG) s of
    Left msg -> liftIO $ print $ nestedToDoc msg
    Right x  -> comp x


-- prettyPrint :: (MonadIO m, Show a) => (a -> Maybe Doc) -> a -> m ()
-- prettyPrint f x = liftIO . putStrLn $ render f x

displayLogs :: (MonadIO m, MonadState REPLState m) => [Doc] -> m ()
displayLogs logs = do
    fs <- gets flags
    when ( PrintLogs `elem` fs )
         ( liftIO $ putStrLn "[LOGS]" >> mapM_ (print . nest 4) logs )

-- displayRaw :: (MonadIO m, MonadState REPLState m, Show a) => a -> m ()
-- displayRaw x = do
--     flag <- gets flagRawOutput
--     when flag $ liftIO $ ppPrint x


step :: (Applicative m, MonadState REPLState m, MonadIO m) => Command -> m Bool
step (EvalTypeKind _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
-- withParsedGPlate pExpr s $ \ x -> do
--     let err = liftIO $ putStrLn "Error. Try :e, :t, :k individually to see what it was."
--     bs <- gets $ topLevelBindings . currentSpec
--     (xEval,logsEval) <- runEvaluateExpr bs x
--     let (exType, logsType) = runTypeOf bs x
--     let (exKind, logsKind) = runKindOf bs x
--     displayLogs (logsEval ++ logsType ++ logsKind)
--     displayRaw x
--     displayRaw xEval
--     case (exType, exKind) of
--         (Right xType, Right xKind) ->
--             case (prExpr x, prExpr xEval, prType xType, prKindInteractive xKind) of
--                 (Just inp, Just outp, Just t, k) -> do
--                     let firstLine  = sep [inp, text "is", k, text "of type", t]
--                     let secondLine = sep [text "Can be evaluated to:", outp]
--                     liftIO . putStrLn . renderDoc $
--                         if x == xEval
--                             then firstLine
--                             else vcat [firstLine, secondLine]
--                 _ -> err
--         _ -> err
step (Evaluate s) = withParsedGPlate s $ \ x -> do
    spec <- gets currentSpec
    let (x',logs) = runWriter $ runErrorT $ flip evalStateT ( def :: ( BindingsMap
                                                                     , [GNode]
                                                                     , [(GNode,GNode)]
                                                                     )) $ do
                        mapM_ addBinding' builtIns
                        mapM_ addBinding' (lefts (topLevels spec))
                        deepSimplify (x :: Expr)
    displayLogs logs
    liftIO $ case x' of
        Left  err     -> print $ nestedToDoc err
        Right (x'',_) -> print $ pretty x''
step (TypeOf s) = withParsedGPlate s $ \ x -> do
    sp <- gets currentSpec
    let (t,logs) = runWriter $ runErrorT $ flip evalStateT (def :: ( BindingsMap
                                                                   , [GNode]
                                                                   ,[(GNode,GNode)]
                                                                   )) $ do
                        mapM_ addBinding' builtIns
                        mapM_ addBinding' (lefts (topLevels sp))
                        typeOf (x :: Expr)
    displayLogs logs
    case t of
        Left err -> liftIO $ print $ vcat [ "Error while type-checking."
                                          , nest 4 (nestedToDoc err) ]
        Right t' -> liftIO $ print $ pretty t'
    -- bs <- gets $ topLevelBindings . currentSpec
    -- let (et, logs) = runTypeOf bs x
    -- displayRaw x
    -- displayRaw et
    -- case et of
    --     Left err -> liftIO $ putStrLn $ "Error while type-checking: " ++ err
    --     Right t  -> do
    --         displayLogs logs
    --         prettyPrint prType t
step (KindOf _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
step ShowFullAST = returningTrue $ do sp <- gets currentSpec; liftIO $ ppPrint sp
step (ShowAST s) = withParsedGPlate s $ \ x -> liftIO $ ppPrint (x :: Expr)
step (Load   fp) = returningTrue $ do
    msp <- liftIO readIt
    case msp of
        Left  e  -> liftIO $ print $ nestedToDoc e
        Right sp -> modifySpec $ const sp
    where
        readIt :: IO (Either (Nested Doc) Spec)
        readIt = do
            contents <- T.readFile fp
            let (res,logs) = runReadIn (Just fp) contents
            mapM_ print logs
            return $ do s <- res
                        quanRenameFinal s
step (Save fp) = returningTrue $ do
    sp <- gets currentSpec
    liftIO $ writeFile fp $ show $ pretty sp
step (Record s) = withParsed s $ \ res -> case res of
    Left tl         -> modifySpec $ \ sp -> sp { topLevels   = topLevels   sp ++ tl  }
    Right (Left o)  -> modifySpec $ \ sp -> sp { objective   = Just o }
    Right (Right x) -> modifySpec $ \ sp -> sp { constraints = constraints sp ++ [x] }

step (RmDeclaration _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
step (RmConstraint  _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
step RmObjective       = returningTrue $ modifySpec $ \ sp -> sp { objective = Nothing }

step Rollback = returningTrue $ do
    st <- get
    let olds = oldSpecs st
    case olds of
        []     -> return ()
        (s:ss) -> put st { currentSpec = s, oldSpecs = ss }
step (TestDomMatch s) = testMatch (undefined :: Domain) ":testdommatch pattern ~~ domain" s
step (TestExprMatch s) = testMatch (undefined :: Expr) ":testexprmatch pattern ~~ expr" s
step DisplaySpec = returningTrue $ do
    sp <- gets currentSpec
    liftIO $ print $ pretty sp
step Quit = return False

testMatch :: (Applicative m, MonadIO m, GPlate a) => a -> String -> String -> m Bool
testMatch undef msg s = returningTrue $ do
    let pa = splitOn "~~" s
    case pa of
        [p,a] -> case (parseEither (parse <* eof) (strip p), parseEither (parse <* eof) (strip a)) of
            (Left err,_) -> liftIO $ print $ "Error while parsing:" <+> text p $$ nestedToDoc err
            (_,Left err) -> liftIO $ print $ "Error while parsing:" <+> text p $$ nestedToDoc err
            (Right px, Right ax) -> case execStateT (runMatch (px `asTypeOf` undef) ax) M.empty of
                Left err -> liftIO $ print $ "Error while pattern matching: " $$ nestedToDoc err
                Right bs -> liftIO $ do
                    putStrLn "Bindings: "
                    forM_ (M.toList bs) $ \ (i, GNode ty g) -> putStrLn $ "[" ++ show ty ++ "] " ++ i ++ ": " ++ show (pretty g)
        _ -> liftIO $ putStrLn $ "Usage: " ++ msg

-- stepFlag :: (MonadIO m, MonadState REPLState m) => String -> m ()
-- stepFlag "rawOutput" = do
--     st  <- get
--     val <- gets flagRawOutput
--     put $ st { flagRawOutput = not val }
-- stepFlag "logging" = do
--     st  <- get
--     val <- gets flagLogs
--     put $ st { flagLogs = not val }
-- stepFlag flag = liftIO $ putStrLn $ "no such flag: " ++ flag


main :: IO ()
main = do
    (initSt, args) <- do
        args <- getArgs
        return $
            if "--printlogs" `elem` args
                then ( initREPLState { flags = [PrintLogs] }
                     , delete "--printlogs" args
                     )
                else ( initREPLState
                     , args
                     )
    bindir <- getBinDir
    let settings = Settings { complete = completeFilename
                            , historyFile = Just $ bindir ++ "/iconjure.hist"
                            , autoAddHistory = True
                            }
    let run k = evalStateT (runInputT settings k) initSt
    putStrLn figlet
    case args of
        []   -> run repl
        [fp] -> do
            putStrLn ("Loading from: " ++ fp)
            run $ do
                lift $ do
                    void $ step $ Load fp
                    void $ step $ DisplaySpec
                repl
        _    ->
            putStrLn $ unlines [ "This program accepts 1 optional argument,"
                               , "which must be a file path pointing to an Essence specification."
                               , ""
                               , "You've given several arguments."
                               ]
    where
        repl :: InputT (StateT REPLState IO) ()
        repl = do
            maybeLine <- getInputLine "# "
            case (maybeLine, parseCommand (fromJust "fromJust repl.maybeLine" maybeLine)) of
                (Nothing, _            ) -> return () -- EOF / control-d
                (Just "", _            ) -> repl
                (Just _ , Left msg     ) -> do liftIO $ putStrLn msg
                                               repl
                (Just _ , Right command) -> do c <- lift $ step command
                                               when c repl
