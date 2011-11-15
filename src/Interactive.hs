module Main where


import Control.Applicative
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, gets, put )
import Data.Char ( toLower )
import Data.List ( intercalate, isPrefixOf )
import Data.Maybe ( fromJust )
import System.Console.Readline ( addHistory, readline )

import Language.Essence ( Spec(..), Expr(..) )
import Language.EssenceEvaluator ( runEvaluateExpr )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import Language.EssenceTypes ( runTypeOf )
import ParsecUtils ( parseEither, eof )
import PrintUtils ( render )
import Utils ( ppPrint, strip )


data Command = EvalTypeKind String
             | Eval String
             | TypeOf String
             | KindOf String
             | Load FilePath
             | Save FilePath
             | Record String        -- might record a declaration, where clause, objective, or constraint
             | RmDeclaration String
             | RmConstraint String
             | RmObjective
             | Rollback
             | DisplaySpec
             | Quit
             | Flag String
    deriving (Eq, Ord, Read, Show)


-- given the input line, returns either an error message or Command to be
-- excuted.
parseCommand :: String -> Either String Command
parseCommand s = do
    case strip s of
        (':':ss) -> do
            firstWord <- case words ss of []    -> Right ""
                                          (i:_) -> Right $ map toLower i
            let restOfLine = strip $ drop (length firstWord) ss
            let actions = [ ( "evaluate"      , Eval restOfLine          )
                          , ( "typeof"        , TypeOf restOfLine        )
                          , ( "kindof"        , KindOf restOfLine        )
                          , ( "load"          , Load restOfLine          )
                          , ( "save"          , Save restOfLine          )
                          , ( "record"        , Record restOfLine        )
                          , ( "rmdeclaration" , RmDeclaration restOfLine )
                          , ( "rmconstraint"  , RmConstraint restOfLine  )
                          , ( "rmobjective"   , RmObjective              )
                          , ( "rollback"      , Rollback                 )
                          , ( "displayspec"   , DisplaySpec              )
                          , ( "quit"          , Quit                     )
                          , ( "flag"          , Flag restOfLine          )
                          ]
            case filter (\ (i,_) -> isPrefixOf firstWord i ) actions of
                []        -> Left "no such action"
                [(_,act)] -> Right act
                xs        -> Left $ "ambigious: " ++ intercalate ", " (map fst xs) ++ "?"
        line -> Right $ EvalTypeKind line


data REPLState = REPLState { currentSpec   :: Spec
                           , oldSpecs      :: [Spec]
                           , commandHist   :: [Command]
                           , flagLogs      :: Bool
                           , flagRawOutput :: Bool
                           }
    deriving (Eq, Ord, Read, Show)


initREPLState :: REPLState
initREPLState = REPLState { currentSpec   = sp
                          , oldSpecs      = []
                          , commandHist   = []
                          , flagLogs      = False
                          , flagRawOutput = False
                          }
    where
        sp :: Spec
        sp = Spec { language         = "Essence"
                  , version          = [2,0]
                  , topLevelBindings = []
                  , topLevelWheres   = []
                  , objective        = Nothing
                  , constraints      = []
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

withParsed :: (Applicative m, MonadIO m) => Parser a -> String -> (a -> m ()) -> m Bool
withParsed p s comp = returningTrue $ case parseEither (p <* eof) s of
    Left msg -> liftIO $ putStrLn msg
    Right x  -> comp x

prettyPrint :: (MonadIO m, Show a) => (a -> Maybe Doc) -> a -> m ()
prettyPrint f x = case f x of
    Nothing  -> liftIO $ putStrLn $ "Error while printing this: " ++ show x
    Just doc -> liftIO $ putStrLn $ render doc

displayLogs :: (MonadIO m, MonadState REPLState m) => [Log] -> m ()
displayLogs logs = do
    flag <- gets flagLogs
    when flag $ liftIO $ putStrLn $ unlines $ "[LOGS]" : map ("  "++) logs

displayRaws :: (MonadIO m, MonadState REPLState m, Show a, Show b) => a -> b -> m ()
displayRaws x y = do
    flag <- gets flagRawOutput
    when flag $ do
        liftIO $ ppPrint x
        liftIO $ ppPrint y


step :: Command -> StateT REPLState IO Bool
step (Eval s) = do
    case parseEither (pExpr <* eof) s of
        Left msg -> liftIO $ putStrLn msg
        Right x  -> do
            sp <- gets currentSpec
            let (x', logs) = runEvaluateExpr (topLevelBindings sp) x
            liftIO $ putStrLn $ unlines $ "[LOGS]" : map ("  "++) logs
            case prExpr x' of
                Nothing  -> liftIO $ putStrLn $ "Error while printing this: " ++ show x'
                Just doc -> do
                    flag <- gets flagppPrint
                    when flag $ do
                        liftIO $ ppPrint x
                        liftIO $ ppPrint x'
                    liftIO $ putStrLn $ render doc
    return True
step (TypeOf s) = do
    case parseEither (pExpr <* eof) s of
        Left msg -> liftIO $ putStrLn msg
        Right x  -> do
            sp <- gets currentSpec
            let (et, logs) = runTypeOf (topLevelBindings sp) x
            liftIO $ putStrLn $ unlines $ "[LOGS]" : map ("  "++) logs
            case et of
                Left err -> liftIO $ putStrLn $ "Error while typechecking: " ++ err
                Right t  -> do
                    flag <- gets flagppPrint
                    when flag $ do
                        liftIO $ ppPrint x
                        liftIO $ ppPrint t
                    liftIO $ putStrLn $ show t
    return True
step (Flag "ppPrint") = do
    st  <- get
    val <- gets flagppPrint
    put $ st { flagppPrint = not val }
    return True
step (Flag flag) = do
    liftIO $ putStrLn $ "no such flag: " ++ flag
    return True
step Quit = return False
step c = do
    liftIO $ putStrLn $ "feature not implemented yet: " ++ show c
    return True


stepFlag :: (MonadIO m, MonadState REPLState m) => String -> m ()
stepFlag "rawOutput" = do
    st  <- get
    val <- gets flagRawOutput
    put $ st { flagRawOutput = not val }
stepFlag "logging" = do
    st  <- get
    val <- gets flagLogs
    put $ st { flagLogs = not val }
stepFlag flag = liftIO $ putStrLn $ "no such flag: " ++ flag


main :: IO ()
main = evalStateT repl initREPLState
    where
        repl :: StateT REPLState IO ()
        repl = do
            maybeLine <- liftIO $ readline "# "
            case (maybeLine, parseCommand (fromJust maybeLine)) of
                (Nothing  , _            ) -> return () -- EOF / control-d
                (Just ""  , _            ) -> repl
                (Just line, Left msg     ) -> do liftIO $ addHistory line
                                                 liftIO $ putStrLn msg
                                                 repl
                (Just line, Right command) -> do liftIO $ addHistory line
                                                 c <- step command
                                                 when c repl
