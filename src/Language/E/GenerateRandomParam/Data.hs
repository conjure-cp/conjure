{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateRandomParam.Data where

import Language.E
import qualified Text.PrettyPrint as Pr

type Essence      = Spec
type EssenceParam = Spec
type Size         = Range

-- Data type representing choices
data Choice =
     CBool
   | CInt     Integer [Range]
   | CTuple  [Choice]
   | CMatrix [Range]   Choice
   | CSet     Size     Choice
   | CRel     Size    [Choice]
   | CFunc    Size     FAttrs   Choice   Choice
     deriving (Show,Eq)

--TODO MSet partition funcion and where

data FAttrs = FAttrs
    {fTotal      :: Bool
    ,fInjective  :: Bool
    ,fSurjective :: Bool
    } deriving(Show,Eq)

data PAttrs = PAttrs
    {pRegular  :: Bool
    ,pNumParts :: Integer
    ,pPartSize :: Integer
    }deriving(Show,Eq)

data Range  =
    RSingle Integer
  | RRange  Integer Integer
    deriving (Show, Eq)

-- This assumes no overlapping ranges
instance Ord Range where
  (RSingle a ) <= (RSingle b ) = a <= b
  (RRange a _) <= (RRange c _) = a <= c
  (RRange _ b) <= (RSingle c ) = b <= c
  (RSingle a)  <= (RRange b _) = a <= b

instance Pretty Range  where pretty = pretty . show
instance Pretty Choice where
    pretty (CBool)           = "CBool"
    pretty (CInt i rs )      = "CInt{" <> pretty i <> "}"    <+> sep (map pretty rs)
    pretty (CTuple vs )      = "CTuple" <+> "(" <+>
                               sep (map (\a -> pretty a <+> " ") vs) <> ")"
    pretty (CMatrix rs cs)   = "CMatrix" <+> sep (map pretty rs) <+> "[" <+> pretty cs <+> "]"
    pretty (CSet rs dom)     = "CSet" <+> pretty rs <+> "OF" <+> pretty dom
    pretty (CRel rs vs)      = "CRel" <+> pretty rs <+> "⟪" <+>
                               sep (map (\a -> pretty a <+> " ") vs) <> "⟫"

    pretty (CFunc rs attrs from to) = Pr.hang
                                      ("CFunc" <> pretty attrs  <+> pretty rs)
                                      8
                                      (pretty from   <+> "-->" <+> pretty to)

instance Pretty FAttrs where
    pretty (FAttrs{fTotal=t,fInjective=i,fSurjective=s}) =
          "{" <> (Pr.hsep . map func) [(t,"Total"),(i,"Injective"),(s,"Surjective")]  <> "}"
        where
        func (True, str) = str
        func (False, _) = Pr.empty

