{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module GhcTags.CTag.Header
  ( Header (..)
  , defaultHeaders
  , HeaderType (..)
  , SomeHeaderType (..)
  -- * Utils
  , SingHeaderType (..)
  , headerTypeSing
  ) where

import Data.Version
import qualified Data.Text as T

import Paths_ghc_tags (version)

-- | A type safe representation of a /ctag/ header.
--
data Header where
    Header :: forall ty. Show ty =>
              { headerType     :: HeaderType ty
              , headerLanguage :: Maybe T.Text
              , headerArg      :: ty
              , headerComment  :: T.Text
              }
            -> Header

instance Eq Header where
    Header  { headerType = headerType0
            , headerLanguage = headerLanguage0
            , headerArg  = headerArg0
            , headerComment = headerComment0
            }
      ==
      Header { headerType = headerType1
             , headerLanguage = headerLanguage1
             , headerArg  = headerArg1
             , headerComment = headerComment1
             } =
        case (headerType0, headerType1) of
          (FileEncoding, FileEncoding) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FileFormat, FileFormat) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FileSorted, FileSorted) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (OutputMode, OutputMode) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (KindDescription, KindDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (KindSeparator, KindSeparator) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramAuthor, ProgramAuthor) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramName, ProgramName) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramUrl, ProgramUrl) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ProgramVersion, ProgramVersion) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (ExtraDescription, ExtraDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (FieldDescription, FieldDescription) ->
            headerArg0 == headerArg1 &&
            headerLanguage0 == headerLanguage1 &&
            headerComment0 == headerComment1
          (PseudoTag name0, PseudoTag name1) ->
            name0 == name1 &&
            headerLanguage0 == headerLanguage1 &&
            headerArg0 == headerArg1 &&
            headerComment0 == headerComment1
          _ -> False

deriving instance Show Header

-- | Enumeration of header type and values of their corresponding argument
--
data HeaderType ty where
    FileEncoding      :: HeaderType T.Text
    FileFormat        :: HeaderType Int
    FileSorted        :: HeaderType Int
    OutputMode        :: HeaderType T.Text
    KindDescription   :: HeaderType T.Text
    KindSeparator     :: HeaderType T.Text
    ProgramAuthor     :: HeaderType T.Text
    ProgramName       :: HeaderType T.Text
    ProgramUrl        :: HeaderType T.Text
    ProgramVersion    :: HeaderType T.Text

    ExtraDescription  :: HeaderType T.Text
    FieldDescription  :: HeaderType T.Text
    PseudoTag         :: T.Text -> HeaderType T.Text

deriving instance Eq (HeaderType ty)
deriving instance Ord (HeaderType ty)
deriving instance Show (HeaderType ty)

-- | Existential wrapper.
--
data SomeHeaderType where
    SomeHeaderType :: forall ty. HeaderType ty -> SomeHeaderType


-- | Singletons which makes it easier to work with 'HeaderType'
--
data SingHeaderType ty where
    SingHeaderTypeText :: SingHeaderType T.Text
    SingHeaderTypeInt  :: SingHeaderType Int


headerTypeSing :: HeaderType ty -> SingHeaderType ty
headerTypeSing = \case
    FileEncoding     -> SingHeaderTypeText
    FileFormat       -> SingHeaderTypeInt
    FileSorted       -> SingHeaderTypeInt
    OutputMode       -> SingHeaderTypeText
    KindDescription  -> SingHeaderTypeText
    KindSeparator    -> SingHeaderTypeText
    ProgramAuthor    -> SingHeaderTypeText
    ProgramName      -> SingHeaderTypeText
    ProgramUrl       -> SingHeaderTypeText
    ProgramVersion   -> SingHeaderTypeText

    ExtraDescription -> SingHeaderTypeText
    FieldDescription -> SingHeaderTypeText
    PseudoTag {}     -> SingHeaderTypeText

----------------------------------------

defaultHeaders :: [Header]
defaultHeaders =
  [ Header FileFormat     Nothing 2 ""
  , Header FileSorted     Nothing 1 ""
  , Header FileEncoding   Nothing "utf-8" ""
  , Header ProgramName    Nothing "ghc-tags" ""
  , Header ProgramUrl     Nothing "https://hackage.haskell.org/package/ghc-tags" ""
  , Header ProgramVersion Nothing (T.pack $ showVersion version) ""

  , Header FieldDescription haskellLang "type" "type of expression"
  , Header FieldDescription haskellLang "ffi"  "foreign object name"
  , Header FieldDescription haskellLang "file" "not exported term"
  , Header FieldDescription haskellLang "instance" "class, type or data type instance"
  , Header FieldDescription haskellLang "Kind" "kind of a type"

  , Header KindDescription haskellLang "`" "module top level term, but not a function"
  , Header KindDescription haskellLang "λ" "module top level function term"
  , Header KindDescription haskellLang "Λ" "type constructor"
  , Header KindDescription haskellLang "c" "data constructor"
  , Header KindDescription haskellLang "g" "gadt constructor"
  , Header KindDescription haskellLang "r" "record field"
  , Header KindDescription haskellLang "≡" "type synonym"
  , Header KindDescription haskellLang "~" "type signature"
  , Header KindDescription haskellLang "p" "pattern synonym"
  , Header KindDescription haskellLang "C" "type class"
  , Header KindDescription haskellLang "m" "type class member"
  , Header KindDescription haskellLang "i" "type class instance"
  , Header KindDescription haskellLang "F" "type family"
  , Header KindDescription haskellLang "f" "type family instance"
  , Header KindDescription haskellLang "D" "data type family"
  , Header KindDescription haskellLang "d" "data type family instance"
  , Header KindDescription haskellLang "I" "foreign import"
  , Header KindDescription haskellLang "E" "foreign export"
  ]
  where
    haskellLang = Just "Haskell"
