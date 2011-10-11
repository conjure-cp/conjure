{-# LANGUAGE ScopedTypeVariables #-}

module ZippyUtils ( bottomUpApply, replaceHoleL ) where


import Prelude hiding ( log )
import Data.Generics.Uniplate.Data ( Uniplate )
import Data.Generics.Uniplate.Zipper ( Zipper, right, up, down, hole, replaceHole )
import Data.Maybe ( fromJust, fromMaybe )

import MonadInterleave


-- bottomUpApply applies a given transformation to a zipper in a bottom up manner.
-- the transformation should never produce an empty list, in which case the overall result will be the empty list.
bottomUpApply ::
    forall from to m . ( Uniplate to , Show to , MonadInterleave m )
    => (Zipper from to -> m [Zipper from to])
    -> Zipper from to
    -> m [Zipper from to]
bottomUpApply f z = concatMapM f =<< case down z of

        -- is this a leaf? yes: `z` has no immediate children. return it unmodified in a singleton list.
        -- `f` is to be applied to immediate children, if any.
        Nothing -> sequence [yield z]

        -- is this a leaf? no: call helper function `next`, on the first (leftmost) child.
        Just leftmost -> mapM (yield . unsafeUp) =<< next leftmost

    where

        -- applies `bottomUpApply f` to the current node, then proceeds to the siblings on the right.
        -- 
        next :: Zipper from to -> m [Zipper from to]
        next this = do

            -- apply the transformation to the input child
            these :: [Zipper from to] <- bottomUpApply f this

            -- has more siblings to the right?
            case right this of

                -- no: go up and return the zipper
                Nothing -> yield these

                -- yes: recurse. (we know there is a right, so using fromJust is fine.)

                -- this is the naive implementation. fix this child and proceed to other siblings.
                -- however it results in wasted applications of `f`. branches too early.
                -- this behaviour can be examined simply by passing an `f` in the IO monad and reporting status in `f`.
                -- Just _  -> concatMapM next $ map (fromJust . right) these

                -- this is a more involved implementation. uses fewer applications of `f`.
                -- proceeds to the right using the "right" of the unmodified tree.
                -- extracts the results (first line)
                -- later inserts them to the modifie tree(s)
                Just r  -> do
                    holes <- mapM (yield . hole) =<< next r
                    concatMapM (yield . replaceHoleL holes . fromJust . right) these

-- | not every zipper has a parent.
--   `unsafeUp` assumes its argument does.
--   throws a runtime error otherwise.
unsafeUp :: Show to => Zipper from to -> Zipper from to
unsafeUp z = fromMaybe (error ("unsafeUp: " ++ show (hole z))) (up z)


-- | the missing `concatMapM`.
concatMapM :: MonadInterleave m => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = do
    bbs <- mapM f as
    return (concat bbs)


-- | a version of `replaceHole` which works on lists.
replaceHoleL :: Uniplate to => [to] -> Zipper from to -> [Zipper from to]
replaceHoleL xs z = [ replaceHole x z | x <- xs ]
