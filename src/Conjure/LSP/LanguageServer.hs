module Conjure.LSP.LanguageServer where
import Language.LSP.Server
import Language.LSP.Types

import Conjure.Prelude

data LSPConfig = LSPConfig {} 


startServer :: LSPConfig -> IO ()
startServer cfg = do 
    putStrLn "LSP"
    return ()