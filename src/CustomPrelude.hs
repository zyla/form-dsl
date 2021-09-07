module CustomPrelude
  ( module X
  , module CustomPrelude
  ) where

import Prelude as X hiding (lookup)

import GHC.Stack

import Data.Text as X (Text)
import qualified Data.Text as Text

import Data.Map as X (Map)
import Data.Set as X (Set)

tshow :: Show a => a -> Text
tshow = Text.pack . show

terror :: HasCallStack => Text -> a
terror = error . Text.unpack
