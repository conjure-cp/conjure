{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
{-# LANGUAGE CPP #-}

module Language.E.Up.Debug where


import Text.PrettyPrint

import Language.E hiding (trace)
import qualified Debug.Trace ( trace )
import qualified Text.Groom
groom :: Show a => a -> String
groom = Text.Groom.groom


#ifdef UP_DEBUG


upBug :: Pretty a =>  String -> [a] -> t
upBug = errpM

upBugi :: (Show a, Pretty a1) => String -> (a, [a1]) -> t
upBugi = erriM

tracer :: Pretty a => String -> a -> a
tracer s a = trace (s ++ '\n' : (show . pretty $ a) ++ "\n" ) a

tracee :: String -> a -> a
tracee s a = trace (s ++ "\n") a


#else

upBug :: Pretty a =>  String -> [a] -> t
upBug = errpM
{-upBug s es = bug (pretty s <+> vcat (map pretty es) )-}

upBugi :: (Show a, Pretty a1) => String -> (a, [a1]) -> t
upBugi = erriM
{-upBugi s (a,_) = bug (pretty s <+> pretty (groom a)  )-}


tracer :: Pretty a => String -> a -> a
tracer _ a = a

tracee :: String -> a -> a
tracee _ a = a


#endif

trace :: String -> a -> a
trace = Debug.Trace.trace

_t :: a -> String -> a
_t a s = trace s a

_g :: Show a1 => a -> (String, a1) -> a
_p :: Pretty a1 => a -> (String, [a1]) -> a

__groomPrint :: Show a => a -> IO ()
__groomPrintM :: Show a => String -> a -> IO ()

__q :: Pretty a => [a] -> String
__s :: Show a1 => (String, a1) -> a -> a
__q2 :: Pretty a => (String, [a]) -> String

prettyAsBoth :: E -> Doc
prettyAsBoth a = vcat [prettyAsTree a, pretty a]
nlToTab :: String -> String
nlToTab = map (\a -> if a == '\n' then '\t' else a )

__groomPrint = putStrLn . groom
__groomPrintM a b = putStr (a ++ " ⦙\n ") >> (putStrLn . groom) b

__q arr = "\n#\n" ++ (show . vcat . map pretty) arr ++ "\n##\n"
__s (msg,b) = trace ("\n##" ++ msg ++ " ⦙ " ++ groom b ++ "\n")
__q2 (msg,b) =  "##" ++ msg ++ " ⦙ " ++ (nlToTab . show . vcat . map pretty) b ++ "\n"

_x :: Show a => a  -> IO ()
_x a = (putStrLn . groom) a

#ifdef UP_DEBUG
_p a (msg,b) = 
    let str = (show . vcat . map pretty) b
    in if (length str) < 30 then 
           trace ( __q2 (msg,b)) a
       else
            trace (msg ++ __q b) a

_g a b = __s b $ a

#else
_p  a _ =  a
_g  a _ =  a
#endif

errr :: Show a => a -> t
errp :: Pretty a => a -> t
errt :: [E] -> t
errs :: [E] -> t
errbM :: String -> [E] -> t
errpM :: Pretty a => String -> [a] -> t
erriM :: (Show a, Pretty a1) => String -> (a, [a1]) -> t

errr a   = error $ '\n' : groom a
errp a   = error $ '\n' : (show . pretty) a
errt arr = error $ '\n' : (show . vcat . map prettyAsTree) arr
errs arr = error $ '\n' : (show . vcat . map prettyAsPaths) arr
errbM msg arr = error $ msg ++  '\n' : (show . vcat . map prettyAsBoth) arr
errpM msg arr = error $ msg ++  '\n' : (show . vcat . map pretty) arr
erriM msg (ts,arr) = error $  msg ++  '\n' :  groom ts  ++  '\n' : (show . vcat . map pretty) arr

