module Conjure.UI.ErrorDisplay where
import Conjure.Prelude
import Conjure.Language.Validator
import Text.Megaparsec
import Data.Void (Void)
import qualified Data.Set as Set
import Conjure.Language.AST.Syntax
import Conjure.Language.AST.ASTParser
import Conjure.Language.NewLexer
import Conjure.Language.Lexemes
import qualified Data.Text
import qualified Data.Text as T



type Parser t = Parsec DiagnosticForPrint Text t

data DiagnosticForPrint = DiagnosticForPrint {
    dStart :: Int,
    dLength :: Int,
    dMessage :: Diagnostic
} deriving (Show,Eq,Ord)

instance ShowErrorComponent DiagnosticForPrint where
  errorComponentLen (DiagnosticForPrint {dLength=l}) = l

  showErrorComponent DiagnosticForPrint {dMessage=message}= case message of
    Error et ->  displayError et
    Warning wt -> "Warning:" ++ show wt
    Info it -> "Info: " ++ show it

displayError :: ErrorType -> String
displayError x = case x of
  TokenError lt -> "Error: " ++ show lt
  SyntaxError txt -> "Syntax Error: " ++ T.unpack txt
  SemanticError txt -> "Semantic error: " ++ T.unpack txt
  CustomError txt -> "Error: " ++ T.unpack txt
  InternalError -> "Pattern match failiure"
  InternalErrorS txt -> "Something went wrong:" ++ T.unpack txt

showDiagnosticsForConsole :: [ValidatorDiagnostic] -> Text -> String
showDiagnosticsForConsole errs text
    =   case runParser (captureErrors errs) "Errors" text of
            Left peb -> errorBundlePretty peb
            Right _ -> "No printable errors from :" ++ (show . length $ errs)


captureErrors :: [ValidatorDiagnostic] -> Parser ()
captureErrors = mapM_ captureError

captureError :: ValidatorDiagnostic -> Parser ()
captureError (ValidatorDiagnostic GlobalRegion message) = do
    let printError = DiagnosticForPrint 0 0 message
    registerFancyFailure (Set.singleton(ErrorCustom printError) )
captureError (ValidatorDiagnostic area message) = do
    setOffset $ drOffset area
    let printError = DiagnosticForPrint (drOffset area) (drLength area) message
    registerFancyFailure (Set.singleton(ErrorCustom printError) )



val :: String -> IO ()
val s = do
    let str = s
    let other = []
    let txt = Data.Text.pack str
    let lexed = parseMaybe eLex txt
    let stream = ETokenStream txt $ fromMaybe other lexed
    -- parseTest parseProgram stream
    let progStruct = runParser parseProgram "TEST" stream

    case progStruct of
        Left _ -> putStrLn "error"
        Right p@(ProgramTree{}) -> let qpr = runValidator (validateModel p) def in
            case qpr of
                (model, vds) -> do
                    print (maybe "" show model)
                    putStrLn $ show vds
                    putStrLn $ showDiagnosticsForConsole vds txt


            -- putStrLn $ show qpr


valFile :: String -> IO ()
valFile p = do
    path <- readFileIfExists p
    case path of
      Nothing -> putStrLn "NO such file"
      Just s -> val s
    return ()
-- putStrLn validateFind
