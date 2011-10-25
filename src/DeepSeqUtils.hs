module DeepSeqUtils ( deepid, deepseq ) where

import Control.DeepSeq

deepid :: NFData a => a -> a
deepid x = x `deepseq` x
