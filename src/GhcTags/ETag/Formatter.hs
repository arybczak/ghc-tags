-- | Simple etags formatter. See <https://en.wikipedia.org/wiki/Ctags#Etags>
--
module GhcTags.ETag.Formatter
  ( formatETagsFile
  , formatTagsFile
  , formatTag
  , BuilderWithSize (..)
  ) where

import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.Map.Strict    as Map
import qualified Data.Text.Encoding as Text

import           GhcTags.Tag


-- | A product of two monoids: 'Builder' and 'Sum'.
--
data BuilderWithSize = BuilderWithSize {
    builder     :: Builder,
    builderSize :: Int
  }

instance Semigroup BuilderWithSize where
    BuilderWithSize b0 s0 <> BuilderWithSize b1 s1 =
      BuilderWithSize (b0 <> b1) (s0 + s1)

instance Monoid BuilderWithSize where
    mempty = BuilderWithSize mempty 0

formatTag :: ETag -> BuilderWithSize
formatTag Tag {tagName, tagAddr = TagLineOff lineNr byteOffset, tagDefinition} =
           flip BuilderWithSize tagSize $
           BB.byteString tagDefinitionBS
        <> BB.charUtf8 '\DEL' -- or '\x7f'
        <> BB.byteString tagNameBS
        <> BB.charUtf8 '\SOH' -- or '\x01'
        <> BB.intDec lineNr
        <> BB.charUtf8 ','
        <> BB.intDec byteOffset
        <> BB.stringUtf8 endOfLine
  where
    tagDefinitionBS = case tagDefinition of
      NoTagDefinition   -> tagNameBS
      TagDefinition def -> Text.encodeUtf8 def
    tagDefinitionSize = BS.length tagDefinitionBS

    tagNameBS = Text.encodeUtf8 . getTagName $ tagName
    tagNameSize = BS.length tagNameBS

    tagSize =
        3 -- delimiters: '\DEL', '\SOH', ','
      + tagNameSize
      + tagDefinitionSize
      + (length $ show lineNr)
      + (length $ show byteOffset)
      + (length $ endOfLine)


-- | The precondition is that all the tags come frome the same file.
--
formatTagsFile :: TagFileName -> [ETag] -> Builder
formatTagsFile _ [] = mempty
formatTagsFile fileName ts =
    case foldMap formatTag ts of
      BuilderWithSize {builder, builderSize} ->
        if builderSize > 0
          then BB.charUtf8 '\x0c'
            <> BB.stringUtf8 endOfLine
            <> (BB.byteString . Text.encodeUtf8 . getTagFileName $ fileName)
            <> BB.charUtf8 ','
            <> BB.intDec builderSize
            <> BB.stringUtf8 endOfLine
            <> builder
          else mempty


-- | Format a list of tags as etags file.  Tags from the same file must be
-- grouped together.
--
formatETagsFile :: ETagMap -> Builder
formatETagsFile = Map.foldMapWithKey formatTagsFile

endOfLine :: String
endOfLine = "\n"
