module Main where


import Control.Applicative
import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, gets )
import System.Console.Readline ( addHistory, readline )

import Language.EssenceEvaluator ( runEvaluateExpr )
import Language.Essence ( Spec(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( parseEither, eof )
import PrintUtils ( render )
import Utils ( strip )


data Command = Eval String
             | Load FilePath
             | Save FilePath
             | Record String        -- might record a declaration, where clause, objective, or constraint
             | RmDeclaration String
             | RmConstraint Int
             | RmObjective
             | Undo
             | Redo
             | Quit
    deriving (Eq, Ord, Read, Show)


parseCommand :: String -> Maybe Command
parseCommand s = do
    firstWord <- case words s of [] -> Nothing; (i:_) -> Just i
    case firstWord of
        ":eval" -> return . Eval . strip . drop (length firstWord) $ s
        ":quit" -> return Quit
        ":q"    -> return Quit
        _       -> Nothing


data REPLState = REPLState { currentSpec :: Spec
                           , oldSpecs    :: [Spec]
                           , commandHist :: [Command]
                           }
    deriving (Eq, Ord, Read, Show)


initREPLState :: REPLState
initREPLState = REPLState { currentSpec = sp
                          , oldSpecs    = []
                          , commandHist = []
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
                    liftIO $ print x
                    liftIO $ print x'
                    liftIO $ putStrLn $ render doc
    return True
step Quit = return False
step _ = do
    liftIO $ putStrLn "what?"
    return True


main :: IO ()
main = evalStateT repl initREPLState
    where
        repl :: StateT REPLState IO ()
        repl = do
            maybeLine <- liftIO $ readline "# "
            case (maybeLine, parseCommand =<< maybeLine) of
                (Nothing  , _           ) -> return () -- EOF / control-d
                (Just ""  , _           ) -> repl
                (Just line, Nothing     ) -> do liftIO $ addHistory line
                                                liftIO $ putStrLn "Cannot parse command."
                                                repl
                (Just line, Just command) -> do liftIO $ addHistory line
                                                c <- step command
                                                when c repl
