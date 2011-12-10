{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Exception ( SomeException, try )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State ( MonadState, get, gets, put )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT )
import Data.ByteString.Char8 ( unpack)
import Data.Char ( toLower )
import Data.FileEmbed ( embedFile )
import Data.List ( intercalate, isPrefixOf )
import Data.Maybe ( fromJust )
import System.Console.Haskeline ( InputT, defaultSettings, getInputLine, runInputT )
import System.Environment ( getArgs )

import Language.Essence ( Spec(..), Log )
import Language.EssenceEvaluator ( runEvaluateExpr )
import Language.EssenceKinds ( runKindOf )
import Language.EssenceParsers ( pSpec, pExpr, pTopLevels, pObjective )
import Language.EssencePrinters ( prSpec, prExpr, prType, prKind, prKindInteractive )
import Language.EssenceTypes ( runTypeOf )
import ParsecUtils ( Parser, parseEither, parseFromFile, eof, choiceTry )
import PrintUtils ( Doc, text, render, sep, vcat )
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

displayRaw :: (MonadIO m, MonadState REPLState m, Show a) => a -> m ()
displayRaw x = do
    flag <- gets flagRawOutput
    when flag $ liftIO $ ppPrint x


step :: Command -> StateT REPLState IO Bool
step (EvalTypeKind s) = withParsed pExpr s $ \ x -> do
    let err = liftIO $ putStrLn "Error. Try :e, :t, :k individually to see what it was."
    bs <- gets $ topLevelBindings . currentSpec
    let (xEval,  logsEval) = runEvaluateExpr bs x
    let (exType, logsType) = runTypeOf bs x
    let (exKind, logsKind) = runKindOf bs x
    displayLogs (logsEval ++ logsType ++ logsKind)
    displayRaw x
    displayRaw xEval
    case (exType, exKind) of
        (Right xType, Right xKind) ->
            case (prExpr x, prExpr xEval, prType xType, prKindInteractive xKind) of
                (Just inp, Just outp, Just t, k) -> do
                    let firstLine  = sep [inp, text "is", k, text "of type", t]
                    let secondLine = sep [text "Can be evaluated to:", outp]
                    liftIO . putStrLn . render $
                        if x == xEval
                            then firstLine
                            else vcat [firstLine, secondLine]
                _ -> err
        _ -> err
step (Eval s) = withParsed pExpr s $ \ x -> do
    bs <- gets $ topLevelBindings . currentSpec
    let (x', logs) = runEvaluateExpr bs x
    displayLogs logs
    displayRaw x
    displayRaw x'
    prettyPrint prExpr x'
step (TypeOf s) = withParsed pExpr s $ \ x -> do
    bs <- gets $ topLevelBindings . currentSpec
    let (et, logs) = runTypeOf bs x
    displayRaw x
    displayRaw et
    case et of
        Left err -> liftIO $ putStrLn $ "Error while type-checking: " ++ err
        Right t  -> do
            displayLogs logs
            prettyPrint prType t
step (KindOf s) = withParsed pExpr s $ \ x -> do
    bs <- gets $ topLevelBindings . currentSpec
    let (ek, logs) = runKindOf bs x
    displayRaw x
    displayRaw ek
    case ek of
        Left err -> liftIO $ putStrLn $ "Error while kind-checking: " ++ err
        Right k  -> do
            displayLogs logs
            prettyPrint prKind k
step (Load fp) = returningTrue $ do
    esp <- liftIO readIt
    case esp of
        Left e   -> liftIO $ putStrLn $ "IO Error: " ++ show e
        Right sp -> modifySpec $ \ _ -> sp
    where
        readIt :: IO (Either SomeException Spec)
        readIt = try $ parseFromFile pSpec id fp id
step (Save fp) = returningTrue $ do
    sp <- gets currentSpec
    case prSpec sp of
        Nothing  -> liftIO $ putStrLn "Error while rendering the current specification."
        Just doc -> liftIO $ writeFile fp $ render doc

step (Record s) = withParsed (choiceTry [ Left . Right <$> pObjective
                                        , Right        <$> pExpr
                                        , Left . Left  <$> pTopLevels
                                        ]) s $ \ res -> case res of
    Left (Left (bs,ws)) -> modifySpec $ \ sp -> sp { topLevelBindings = topLevelBindings sp ++ bs
                                                   , topLevelWheres   = topLevelWheres   sp ++ ws
                                                   }
    Left (Right o)      -> modifySpec $ \ sp -> sp { objective = Just o }
    Right x             -> modifySpec $ \ sp -> sp { constraints = constraints sp ++ [x] }

step (RmDeclaration _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
step (RmConstraint  _) = returningTrue $ liftIO $ putStrLn "not implemented, yet."
step RmObjective       = returningTrue $ modifySpec $ \ sp -> sp { objective = Nothing }

step Rollback = returningTrue $ do
    st <- get
    let olds = oldSpecs st
    case olds of
        []     -> return ()
        (s:ss) -> put st { currentSpec = s, oldSpecs = ss }
step DisplaySpec = returningTrue $ do
    sp <- gets currentSpec
    prettyPrint prSpec sp
step (Flag nm) = returningTrue $ stepFlag nm
step Quit = return False


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
main = do
    let run k = evalStateT (runInputT defaultSettings k) initREPLState
    putStrLn figlet
    args <- getArgs
    case args of
        []   -> run repl
        [fp] -> do
            putStrLn ("Loading from: " ++ fp)
            run $ lift (step (Load fp)) >> repl
        _    -> do
            putStrLn $ unlines [ "This program accepts 1 optional argument,"
                               , "which must be a file path pointing to an Essence specification."
                               , ""
                               , "You've given several arguments."
                               ]
    where
        repl :: InputT (StateT REPLState IO) ()
        repl = do
            maybeLine <- getInputLine "# "
            case (maybeLine, parseCommand (fromJust maybeLine)) of
                (Nothing, _            ) -> return () -- EOF / control-d
                (Just "", _            ) -> repl
                (Just _ , Left msg     ) -> do liftIO $ putStrLn msg
                                               repl
                (Just _ , Right command) -> do c <- lift $ step command
                                               when c repl


figlet :: String
figlet = unpack $(embedFile "datafiles/conjure.figlet")
