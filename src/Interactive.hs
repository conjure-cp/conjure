module Main where


import Control.Applicative
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, gets, put )
import Data.Char ( toLower )
import Data.List ( intercalate, isPrefixOf )
import Data.Maybe ( fromJust )
import System.Console.Readline ( addHistory, readline )

import Language.EssenceEvaluator ( runEvaluateExpr )
import Language.Essence ( Spec(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( parseEither, eof )
import PrintUtils ( render )
import Utils ( ppPrint, strip )


data Command = Eval String
             | Load FilePath
             | Save FilePath
             | Record String        -- might record a declaration, where clause, objective, or constraint
             | RmDeclaration String
             | RmConstraint String
             | RmObjective
             | Undo
             | Redo
             | Quit
             | Flag String
    deriving (Eq, Ord, Read, Show)


parseCommand :: String -> Either String Command
parseCommand s = do
    case strip s of
        (':':ss) -> do
            firstWord <- case words ss of []    -> Left "Cannot parse command."
                                          (i:_) -> Right $ map toLower i
            let restOfLine = strip $ drop (length firstWord) ss
            let actions = [ ( "evaluate"      , Eval restOfLine          )
                          , ( "load"          , Load restOfLine          )
                          , ( "save"          , Save restOfLine          )
                          , ( "record"        , Record restOfLine        )
                          , ( "rmdeclaration" , RmDeclaration restOfLine )
                          , ( "rmconstraint"  , RmConstraint restOfLine  )
                          , ( "rmobjective"   , RmObjective              )
                          , ( "undo"          , Undo                     )
                          , ( "redo"          , Redo                     )
                          , ( "quit"          , Quit                     )
                          , ( "flag"          , Flag restOfLine          )
                          ]
            case filter (\ (i,_) -> isPrefixOf firstWord i ) actions of
                []        -> Left "no such action"
                [(_,act)] -> Right act
                xs        -> Left $ "ambigious: " ++ intercalate ", " (map fst xs) ++ "?"
        line -> Right $ Eval line


data REPLState = REPLState { currentSpec :: Spec
                           , oldSpecs    :: [Spec]
                           , commandHist :: [Command]
                           , flagppPrint :: Bool
                           }
    deriving (Eq, Ord, Read, Show)


initREPLState :: REPLState
initREPLState = REPLState { currentSpec = sp
                          , oldSpecs    = []
                          , commandHist = []
                          , flagppPrint = False
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
