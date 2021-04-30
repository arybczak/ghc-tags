module GhcTags.ETag
  ( module X
  , compareTags
  ) where

import           Data.Function (on)

import           GhcTags.ETag.Formatter as X
import           GhcTags.ETag.Parser    as X

import           GhcTags.Tag ( Tag (..)
                             , ETag
                             )


-- | Order 'ETag's according to addr, name and kind.
--
compareTags :: ETag -> ETag -> Ordering
compareTags t0 t1 =
       on compare tagAddr t0 t1
    <> on compare tagName t0 t1
    <> on compare tagKind t0 t1
