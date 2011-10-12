module Language.EssencePrinters where


import Control.Applicative
import Control.Monad
import Language.Essence
import Text.PrettyPrint


prExpr :: Expr -> Maybe Doc
prExpr x = msum $ map ($x) [prIdentifier, prValue]


prIdentifier :: Expr -> Maybe Doc
prIdentifier (Identifier i) = return $ text i
prIdentifier _ = Nothing


--------------------------------------------------------------------------------
-- Printers for inline values --------------------------------------------------
--------------------------------------------------------------------------------

prValue :: Expr -> Maybe Doc
prValue (ValueBoolean False) = return $ text "false"
prValue (ValueBoolean True ) = return $ text "true"
prValue (ValueInteger i    ) = return $ integer i
prValue (ValueMatrix    xs ) = brackets . hsep . punctuate comma <$> mapM prExpr xs
prValue (ValueTuple     xs ) = parens   . hsep . punctuate comma <$> mapM prExpr xs
prValue (ValueSet       xs ) = (text "set"       <+>) . braces . hsep . punctuate comma <$> mapM prExpr xs
prValue (ValueMSet      xs ) = (text "mset"      <+>) . braces . hsep . punctuate comma <$> mapM prExpr xs
prValue (ValueFunction  xs ) = (text "function"  <+>) . braces . hsep . punctuate comma <$> elements
    where elements = forM xs $ \ (i,j) -> do i' <- prExpr i
                                             j' <- prExpr j
                                             return (i' <+> text "->" <+> j')
prValue (ValueRelation  xs ) = (text "relation"  <+>) . braces . hsep . punctuate comma <$> mapM prExpr xs
prValue (ValuePartition xss) = (text "partition" <+>) . braces . hsep . punctuate comma <$> mapM elements xss
    where elements xs = braces . hsep . punctuate comma <$> mapM prExpr xs
prValue _ = Nothing
