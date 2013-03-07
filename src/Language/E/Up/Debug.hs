{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
{-# LANGUAGE CPP #-}

module Language.E.Up.Debug where


import Text.PrettyPrint

#ifdef UP_DEBUG
import Language.E hiding (trace)

import qualified Debug.Trace ( trace )
import qualified Text.Groom

trace :: String -> a -> a
trace = Debug.Trace.trace

groom :: Show a => a -> String
groom = Text.Groom.groom

upBug :: String -> [E] -> t
upBug = errpM

upBugi :: (Show a, Pretty a1) => String -> (a, [a1]) -> t
upBugi = erriM

#else
import Language.E
groom :: Show a => a -> String
groom = show

upBug :: String -> [E] -> t
{-upBug = errpM-}
upBug s _ = bug (pretty s)

upBugi :: (Show a, Pretty a1) => String -> (a, [a1]) -> t
{-upBugi = erriM-}
upBugi s _ = bug (pretty s)

#endif



-- type Signatures to get rid of 
-- defaulting the following constraint(s) to type `String' warnnings
_e :: a -> (String, [E]) -> a
_f :: Show a1 => a -> (String, a1) -> a
_g :: Show a1 => a -> (String, a1) -> a
_i :: (Show a1) => a -> (String, (a1, [E])) -> a
_k :: (Show a1, Pretty a2) => a -> (String, (a1, [a2])) -> a
_p :: Pretty a1 => a -> (String, [a1]) -> a

__groomPrint :: Show a => a -> IO ()
__groomPrintM :: Show a => String -> a -> IO ()
__s2 :: Pretty primitive => (String, Generic primitive) -> String

__p :: [E] -> String
__p2 :: [[E]] -> String
__i :: Show a => a -> [E] -> String
__k :: (Show a, Pretty a1) => a -> [a1] -> String
__b :: [E] -> String
__q :: Pretty a => [a] -> String
__q2 :: Pretty a => (String, [a]) -> String
__g :: Show a1 => a1 -> a -> a
__s :: Show a1 => (String, a1) -> a -> a
__h :: Show a => String -> a -> a
__j :: String -> E -> E
_e2 :: a -> (String, [[E]]) -> a
_b :: a -> (String, [E]) -> a

prettyAsBoth :: E -> Doc
prettyAsBoth a = vcat [prettyAsTree a, pretty a]
nlToTab :: String -> String
nlToTab = map (\a -> if a == '\n' then '\t' else a )

__groomPrint = putStrLn . groom
__groomPrintM a b = putStr (a ++ " ⦙\n ") >> (putStrLn . groom) b
__s2 (msg,b) =  "\n##" ++ msg ++ " ⦙ " ++ (show . prettyAsTree) b ++ "\n"

__p arr = "\n#\n" ++ (show . vcat . map prettyAsTree) arr ++ "\n##\n"
__p2 arr = "\n#\n" ++  (show . vcat) (concatMap (map prettyAsBoth) arr) ++ "\n##\n"
__i a b = "\n#\n"++ groom a ++ "\n\n" ++ (show . vcat . map prettyAsBoth) b ++  "\n##\n"
__k a b = "\n#\n"++ groom a ++ "\n\n" ++ (show . vcat . map pretty) b ++  "\n##\n"
__b arr = "\n#\n" ++ (show . vcat . map prettyAsBoth) arr ++ "\n##\n"
__q arr = "\n#\n" ++ (show . vcat . map pretty) arr ++ "\n##\n"
__q2 (msg,b) =  "##" ++ msg ++ " ⦙ " ++ (nlToTab . show . vcat . map pretty) b ++ "\n"


__g a = trace ("\n#\n"++ groom a ++ "\n##\n")
__s (msg,b) = trace ("\n##" ++ msg ++ " ⦙ " ++ groom b ++ "\n")


#ifdef UP_DEBUG

__h msg a = trace ("\n#" ++ msg ++ "\n"++ groom a ++ "\n##\n") a

__j msg a = trace ("\n#" ++ msg ++ "\n"++ show (prettyAsTree a) ++ "\n##\n") a

_e a (msg, [b@[xMatch| _ := value.literal |]]) =  trace ( __s2 (msg,b) ) a
_e a (msg, [b@[xMatch| _ := literal |]])       =  trace ( __s2 (msg,b) ) a
_e a (msg,b) = trace (msg ++ __p b) a

_e2 a (msg,b) = trace (msg ++ __p2 b) a
_b a (msg,b)  = trace (msg ++ __b b) a

_p a (msg,b) = 
    let str = (show . vcat . map pretty) b
    in if (length str) < 30 then 
           trace ( __q2 (msg,b)) a
       else
            trace (msg ++ __q b) a

_f a (msg,b) = trace (msg ++ "\n#\n" ++ groom b ++ "\n##\n") a
_g a b = __s b $ a
_i a (msg,(c,d)) = trace (msg ++ __i c d) a
_k a (msg,(c,d)) = trace (msg ++ __k c d) a
_x :: Show a => a  -> IO ()
_x a = (putStrLn . groom) a

#else

__h _ a = a

__j _ a = a

_e  a _ =  a
_e2 a _ =  a
_b  a _ =  a
_p  a _ =  a
_f  a _ =  a
_g  a _ =  a
_i  a _ =  a
_k  a _ =  a

#endif

errr :: Show a => a -> t
errp :: Pretty a => a -> t
errt :: [E] -> t
errs :: [E] -> t
errb :: [E] -> t
errbM :: String -> [E] -> t
errpM :: String -> [E] -> t
errc :: [E] -> [E] -> t
erri :: (Show a, Pretty a1) => (a, [a1]) -> t
erriM :: (Show a, Pretty a1) => String -> (a, [a1]) -> t

errr a   = error $ '\n' : groom a
errp a   = error $ '\n' : (show . pretty) a
errt arr = error $ '\n' : (show . vcat . map prettyAsTree) arr
errs arr = error $ '\n' : (show . vcat . map prettyAsPaths) arr
errb arr = error $ '\n' : (show . vcat . map prettyAsBoth) arr
errbM msg arr = error $ msg ++  '\n' : (show . vcat . map prettyAsBoth) arr
errpM msg arr = error $ msg ++  '\n' : (show . vcat . map pretty) arr
errc arr arr2 = error $ '\n' : (show . vcat . map prettyAsBoth) arr  ++ '\n' : '\n' : (show . vcat . map prettyAsBoth) arr2
erri (ts,arr) = error $ '\n' :  groom ts  ++  '\n' : (show . vcat . map pretty) arr
erriM msg (ts,arr) = error $  msg ++  '\n' :  groom ts  ++  '\n' : (show . vcat . map pretty) arr

