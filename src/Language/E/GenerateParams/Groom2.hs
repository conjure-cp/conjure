module Language.E.GenerateParams.Groom2(groom, groomString) where
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

groomString :: String -> String
groomString s = case parseExp s of
    ParseOk x -> prettyPrintStyleMode style{lineLength=130} defaultMode x
    ParseFailed{} -> s

groom :: Show a => a -> String
groom = groomString . show
