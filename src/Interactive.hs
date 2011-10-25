module Main where

import Control.Monad.IO.Class ( liftIO )
import System.Console.Readline ( addHistory, readline )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT )


import Language.Essence ( Spec(..) )
import Language.EssenceParsers ( pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils ( parseIO )
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
    deriving (Eq, Ord, Read, Show)


parseCommand :: String -> Maybe Command
parseCommand s = do
    firstWord <- case words s of [] -> Nothing; (i:_) -> Just i
    case firstWord of
        ":eval" -> return . Eval . strip . drop (length firstWord) $ s
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


step :: Command -> StateT REPLState IO ()
step (Eval s) = do
    x <- liftIO $ parseIO pExpr s
    x' <- return x
    case prExpr x' of
        Nothing  -> liftIO $ putStrLn $ "Error while printing this: " ++ show x'
        Just doc -> liftIO $ putStrLn $ render doc
step _ = liftIO $ putStrLn "what?"


main :: IO ()
main = evalStateT repl initREPLState
    where
        repl :: StateT REPLState IO ()
        repl = do
            maybeLine <- liftIO $ readline "# "
            case (maybeLine, parseCommand =<< maybeLine) of
                (Nothing  , _           ) -> return () -- EOF / control-d
                (Just line, Nothing     ) -> do liftIO $ addHistory line
                                                liftIO $ putStrLn "Cannot parse command."
                                                repl
                (Just line, Just command) -> do liftIO $ addHistory line
                                                step command
                                                repl
