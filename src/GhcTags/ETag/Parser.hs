-- | Parser combinators for etags file format
--
module GhcTags.ETag.Parser
  ( parseTagsFile
  , parseTagFileSection
  , parseTag
  ) where

import           Control.Applicative (many, (<|>))
import           Data.Attoparsec.Text  (Parser, (<?>))
import qualified Data.Attoparsec.Text  as AT
import           Data.Functor (($>))
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           GhcTags.Tag
import qualified GhcTags.Utils as Utils


-- | Parse whole etags file
--
parseTagsFile :: Text
              -> IO (Either String ETagMap)
parseTagsFile =
      fmap AT.eitherResult
    . AT.parseWith (pure mempty)
                   (Map.fromList <$> many parseTagFileSection)


-- | Parse tags from a single file (a single section in etags file).
--
parseTagFileSection :: Parser (TagFileName, [ETag])
parseTagFileSection =
    (,) <$> (AT.char '\x0c' *> endOfLine *> parseTagFile)
        <*> many parseTag

parseTagFile :: Parser TagFileName
parseTagFile =
      TagFileName
  <$> AT.takeWhile (\x -> x /= ',' && Utils.notNewLine x)
  <*  AT.char ','
  <*  (AT.decimal :: Parser Int)
  <*  endOfLine
  <?> "parsing tag file name failed"


-- | Parse an 'ETag' from a single line.
--
parseTag :: Parser ETag
parseTag =
          mkTag
      <$> parseTagDefinition
      <*> ((Just <$> parseTagName) <|> pure Nothing)
      <*> AT.decimal
      <*  AT.char ','
      <*> AT.decimal
      <*  endOfLine
      <?> "parsing tag failed"
  where
    mkTag :: Text -> Maybe TagName -> Int -> Int -> ETag
    mkTag tagDefinition mTagName lineNo byteOffset = 
      Tag { tagName       = case mTagName of
                              Nothing   -> TagName tagDefinition
                              Just name -> name
          , tagKind       = NoKind
          , tagAddr       = TagLineOff lineNo byteOffset
          , tagDefinition = case mTagName of
                              Nothing -> NoTagDefinition
                              Just _  -> TagDefinition tagDefinition
          , tagFields     = NoTagFields
          }

    parseTagName :: Parser TagName
    parseTagName = TagName <$> AT.takeWhile (\x -> x /= '\SOH' && Utils.notNewLine x)
                           <*  AT.char '\SOH'
                           <?> "parsing tag name failed"

    parseTagDefinition :: Parser Text
    parseTagDefinition = AT.takeWhile (\x -> x /= '\DEL' && Utils.notNewLine x)
                     <*  AT.char '\DEL'
                     <?> "parsing tag definition failed"

endOfLine :: Parser ()
endOfLine = AT.string "\r\n" $> ()
        <|> AT.char '\r' $> ()
        <|> AT.char '\n' $> ()
