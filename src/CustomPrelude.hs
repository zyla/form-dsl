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

import Data.Maybe as X (fromMaybe)
import Control.Monad as X

tshow :: Show a => a -> Text
tshow = Text.pack . show

terror :: HasCallStack => Text -> a
terror = error . Text.unpack
