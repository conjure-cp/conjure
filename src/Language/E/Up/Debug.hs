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

#else
import Language.E
groom :: Show a => a -> String
groom = show

#endif


prettyAsBoth a = vcat [prettyAsTree a, pretty a]
nlToTab = map (\a -> if a == '\n' then '\t' else a )

__groomPrint a = (putStrLn . groom) a
__groomPrintM a b = putStr (a ++ " ⦙\n ") >> (putStrLn . groom) b
__s2 (msg,b) =  ("\n##" ++ msg ++ " ⦙ " ++ (show . prettyAsTree) b ++ "\n")

__p arr = "\n#\n" ++ (show . vcat . map prettyAsTree) arr ++ "\n##\n"
__p2 arr = "\n#\n" ++  (show . vcat) (concatMap (map prettyAsBoth) arr) ++ "\n##\n"
__i a b = "\n#\n"++ groom a ++ "\n\n" ++ (show . vcat . map prettyAsBoth) b ++  "\n##\n"
__k a b = "\n#\n"++ groom a ++ "\n\n" ++ (show . vcat . map pretty) b ++  "\n##\n"
__b arr = "\n#\n" ++ (show . vcat . map prettyAsBoth) arr ++ "\n##\n"
__q arr = "\n#\n" ++ (show . vcat . map pretty) arr ++ "\n##\n"
__q2 (msg,b) =  ("##" ++ msg ++ " ⦙ " ++ (nlToTab . show . vcat . map pretty) b ++ "\n")


__g a = trace ("\n#\n"++ groom a ++ "\n##\n")
__s (msg,b) = trace ("\n##" ++ msg ++ " ⦙ " ++ groom b ++ "\n")


#ifdef UP_DEBUG

__h msg a = trace ("\n#" ++ msg ++ "\n"++ groom a ++ "\n##\n") a

__j msg a = trace ("\n#" ++ msg ++ "\n"++ show (prettyAsTree a) ++ "\n##\n") a
__r arr = "\n#\n" ++ (show . vcat . map prettyAsTree) arr ++ "\n##\n" arr

_e :: a -> (String, [E]) -> a
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
_x a = (putStrLn . groom) a

#else

__f a = a
__h msg a = a

__j msg a = a
__r arr   = arr

_e a b  = a
_e2 a b  = a
_b a b  = a
_p a b  = a
_p2 a b  = a
_f a b  = a
_g a b  = a
_i a b  = a
_k a b  = a
_x a b  = a

#endif

errr a   = error $ '\n' : groom a
errp a   = error $ '\n' : (show . pretty) a
errt arr = error $ '\n' : (show . vcat . map prettyAsTree) arr
errs arr = error $ '\n' : (show . vcat . map prettyAsPaths) arr
errb arr = error $ '\n' : (show . vcat . map prettyAsBoth) arr
errbM msg arr = error $ msg ++  '\n' : (show . vcat . map prettyAsBoth) arr
errpM msg arr = error $ msg ++  '\n' : (show . vcat . map pretty) arr
errc arr arr2 = error $ '\n' : (show . vcat . map prettyAsBoth) arr  ++ '\n' : '\n' : (show . vcat . map prettyAsBoth) arr2
erri (ts,arr) = error $ '\n' :  groom ts  ++  '\n' : (show . vcat . map prettyAsBoth) arr
erriM msg (ts,arr) = error $  msg ++  '\n' :  groom ts  ++  '\n' : (show . vcat . map pretty) arr

