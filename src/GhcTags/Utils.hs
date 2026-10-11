{-# LANGUAGE CPP #-}

module GhcTags.Utils
  ( endOfLine
  , notNewLine
  , endOfInput
  ) where

import Control.Monad
import Data.Attoparsec.Text qualified as AT
import Data.Text qualified as T

-- | Platform dependend eol:
--
-- * windows      "CRNL"
-- * maxos        "CR"
-- * linux (unit) "NL"
endOfLine :: String
#if defined(mingw32_HOST_OS)
endOfLine = "\r\n"
#elif defined(darwin_HIST_OS)
endOfLine = "\r"
#else
endOfLine = "\n"
#endif

notNewLine :: Char -> Bool
notNewLine = \x -> x /= '\n' && x /= '\r'

-- | Fail unless all the input is consumed. A tags file that is only partly
-- readable is rejected as a whole, because otherwise every tag after the first
-- bad line is lost without a word.
endOfInput :: AT.Parser ()
endOfInput = do
  rest <- AT.takeText
  unless (T.null rest) . fail $
    "unexpected input: " ++ show (T.takeWhile (/= '\n') rest)
