{-# LANGUAGE GADTs #-}

module GhcTags.CTag.Utils
  ( tagKindToChar
  , charToTagKind
  ) where

import           GhcTags.Tag

tagKindToChar :: CTagKind -> Maybe Char
tagKindToChar tk = case tk of
    -- No 'F' kind since it's for a filename.
    TkModule                  -> Just 'M'
    TkTerm                    -> Just '`'
    TkFunction                -> Just 'f'
    TkTypeConstructor         -> Just 'A'
    TkDataConstructor         -> Just 'c'
    TkGADTConstructor         -> Just 'g'
    TkRecordField             -> Just 'r'
    TkTypeSynonym             -> Just '='
    TkTypeSignature           -> Just ':'
    TkPatternSynonym          -> Just 'p'
    TkTypeClass               -> Just 'C'
    TkTypeClassMember         -> Just 'm'
    TkTypeClassInstance       -> Just 'i'
    TkTypeFamily              -> Just 'T'
    TkTypeFamilyInstance      -> Just 't'
    TkDataTypeFamily          -> Just 'd'
    TkDataTypeFamilyInstance  -> Just 'D'
    TkForeignImport           -> Just 'I'
    TkForeignExport           -> Just 'E'

    CharKind c                -> Just c
    NoKind                    -> Nothing


charToTagKind :: Char -> CTagKind
charToTagKind c = case c of
     'M' -> TkModule
     '`' -> TkTerm
     'f' -> TkFunction
     'A' -> TkTypeConstructor
     'c' -> TkDataConstructor
     'g' -> TkGADTConstructor
     'r' -> TkRecordField
     '=' -> TkTypeSynonym
     ':' -> TkTypeSignature
     'p' -> TkPatternSynonym
     'C' -> TkTypeClass
     'm' -> TkTypeClassMember
     'i' -> TkTypeClassInstance
     't' -> TkTypeFamily
     'T' -> TkTypeFamilyInstance
     'd' -> TkDataTypeFamily
     'D' -> TkDataTypeFamilyInstance
     'I' -> TkForeignImport
     'E' -> TkForeignExport

     _   -> CharKind c
