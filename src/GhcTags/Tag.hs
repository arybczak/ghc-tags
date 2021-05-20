module GhcTags.Tag
  ( -- * Tag
    TagType (..)
  , SingTagType (..)
  , Tag (..)
  , ETag
  , CTag
  , ETagMap
  , CTagMap
    -- ** Tag fields
  , TagName (..)
  , TagFileName (..)
  , ExCommand (..)
  , TagAddress (..)
  , CTagAddress
  , ETagAddress
  , TagKind (..)
  , CTagKind
  , ETagKind
  , TagDefinition (..)
  , TagFields (..)
  , CTagFields
  , ETagFields
  , TagField (..)
    -- ** Ordering and combining tags
  , compareTags

  -- * Create 'Tag' from a 'GhcTag'
  , ghcTagToTag
  ) where

import           Control.DeepSeq
import           Data.Function (on)
import           Data.Map.Strict (Map)
import           Data.Text   (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- GHC imports
import           GHC.Driver.Session (DynFlags)
import           GHC.Data.FastString (bytesFS)

import           GHC.Types.SrcLoc
                              ( SrcSpan (..)
                              , srcSpanFile
                              , srcSpanStartLine
                              , srcSpanStartCol
                              )

import           GhcTags.Ghc  ( GhcTag (..)
                              , GhcTagKind (..)
                              )
import qualified GHC.Utils.Outputable as Out

--
-- Tag
--

-- | Promoted data type used to disntinguish 'CTag's from 'ETag's.
--
data TagType = CTag | ETag
  deriving Show


-- | Singletons for promoted types.
--
data SingTagType (tt :: TagType) where
    SingCTag :: SingTagType 'CTag
    SingETag :: SingTagType 'ETag


-- | 'ByteString' which encodes a tag name.
--
newtype TagName = TagName { getTagName :: Text }
  deriving (Eq, Ord, Show)

instance NFData TagName where
  rnf = rnf . getTagName

-- | 'ByteString' which encodes a tag file name.
--
newtype TagFileName = TagFileName { getTagFileName :: Text }
  deriving (Eq, Ord, Show)

instance NFData TagFileName where
  rnf = rnf . getTagFileName

-- | When we parse a `tags` file we can eithera find no kind or recognize the
-- kind of GhcTagKind or we store the found character kind.  This allows us to
-- preserve information from parsed tags files which were not created by
-- `ghc-tags-plugin'
--
-- * 'TkModule' - @`@
-- * 'TkTerm' - @`@
-- * 'TkFunction' - @λ@
-- * 'TkTypeConstructor' - @Λ@
-- * 'TkDataConstructor' - @c@
-- * 'TkGADTConstructor' - @g@
-- * 'TkRecordField' - @r@
-- * 'TkTypeSynonym' - @≡@
-- * 'TkTypeSignature' - @⊢@
-- * 'TkPatternSynonym' - @p@
-- * 'TkTypeClass' - @C@
-- * 'TkTypeClassMember' - @m@
-- * 'TkTypeClassInstance' - @i@
-- * 'TkTypeFamily' - @f@
-- * 'TkTypeFamilyInstance' - @F@
-- * 'TkDataTypeFamily' - @d@
-- * 'TkDataTypeFamilyInstance' - @D@
-- * 'TkForeignImport' - @I@
-- * 'TkForeignExport' - @E@
--
data TagKind (tt :: TagType) where
    TkModule                 :: TagKind tt
    TkTerm                   :: TagKind tt
    TkFunction               :: TagKind tt
    TkTypeConstructor        :: TagKind tt
    TkDataConstructor        :: TagKind tt
    TkGADTConstructor        :: TagKind tt
    TkRecordField            :: TagKind tt
    TkTypeSynonym            :: TagKind tt
    TkTypeSignature          :: TagKind tt
    TkPatternSynonym         :: TagKind tt
    TkTypeClass              :: TagKind tt
    TkTypeClassMember        :: TagKind tt
    TkTypeClassInstance      :: TagKind tt
    TkTypeFamily             :: TagKind tt
    TkTypeFamilyInstance     :: TagKind tt
    TkDataTypeFamily         :: TagKind tt
    TkDataTypeFamilyInstance :: TagKind tt
    TkForeignImport          :: TagKind tt
    TkForeignExport          :: TagKind tt
    CharKind                 :: Char -> TagKind tt
    NoKind                   :: TagKind tt

instance NFData (TagKind tt) where
  rnf x = x `seq` ()

type CTagKind = TagKind 'CTag
type ETagKind = TagKind 'ETag

deriving instance Eq   (TagKind tt)
deriving instance Ord  (TagKind tt)
deriving instance Show (TagKind tt)


newtype ExCommand = ExCommand { getExCommand :: Text }
  deriving (Eq, Ord, Show)


-- | Tag address, either from a parsed file or from Haskell's AST>
--
data TagAddress (tt :: TagType) where
      -- | Precise addres: line and column.  This is what we infer from @GHC@
      -- AST.
      --
      -- The two arguments are line number and either column number or offset
      -- from the begining of the file.
      --
      TagLineCol :: Int -> Int -> TagAddress tt

      -- | ctags can only use range ex-commands as an address (or a sequence of
      -- them separated by `;`). We parse line number specifically, since they
      -- are useful for ordering tags.
      --
      TagLine :: Int -> TagAddress 'CTag

      -- | A tag address can be just an ex command.
      --
      TagCommand :: ExCommand -> TagAddress 'CTag

instance NFData (TagAddress tt) where
  rnf x = x `seq` ()

-- | 'CTag' addresses.
--
type CTagAddress = TagAddress 'CTag

-- | 'ETag' addresses.
--
type ETagAddress = TagAddress 'ETag


deriving instance Eq   (TagAddress tt)
deriving instance Ord  (TagAddress tt)
deriving instance Show (TagAddress tt)


-- | Emacs tags specific field.
--
data TagDefinition (tt :: TagType) where
      TagDefinition   :: Text -> TagDefinition 'ETag
      NoTagDefinition :: TagDefinition tt

instance NFData (TagDefinition tt) where
  rnf x = x `seq` ()

deriving instance Show (TagDefinition tt)
deriving instance Eq   (TagDefinition tt)

-- | Unit of data associated with a tag.  Vim natively supports `file:` and
-- `kind:` tags but it can display any other tags too.
--
data TagField = TagField {
      fieldName  :: Text,
      fieldValue :: Text
    }
  deriving (Eq, Ord, Show)

instance NFData TagField where
  rnf x = x `seq` ()

-- | File field; tags which contain 'fileField' are called static (aka static
-- in @C@), such tags are only visible in the current file)
--
fileField :: TagField
fileField = TagField { fieldName = "file", fieldValue = "" }


-- | Ctags specific list of 'TagField's.
--
data TagFields (tt :: TagType) where
    NoTagFields :: TagFields 'ETag

    TagFields   :: [TagField]
                -> TagFields 'CTag

instance NFData (TagFields tt) where
  rnf NoTagFields    = ()
  rnf (TagFields fs) = rnf fs

deriving instance Show (TagFields tt)
deriving instance Eq   (TagFields tt)
instance Semigroup (TagFields tt) where
    NoTagFields   <> NoTagFields   = NoTagFields
    (TagFields a) <> (TagFields b) = TagFields (a ++ b)
instance Monoid (TagFields 'CTag) where
    mempty = TagFields mempty
instance Monoid (TagFields 'ETag) where
    mempty = NoTagFields

type CTagFields = TagFields 'CTag
type ETagFields = TagFields 'ETag


-- | Tag record.  For either ctags or etags formats.  It is either filled with
-- information parsed from a tags file or from *GHC* ast.
--
data Tag (tt :: TagType) = Tag
  { tagName       :: TagName
    -- ^ name of the tag
  , tagKind       :: TagKind tt
    -- ^ ctags specifc field, which classifies tags
  , tagAddr       :: TagAddress tt
    -- ^ address in source file
  , tagDefinition :: TagDefinition tt
    -- ^ etags specific field; only tags read from emacs tags file contain this
    -- field.
  , tagFields     :: TagFields tt
    -- ^ ctags specific field
  }
  deriving (Show, Eq)

instance NFData (Tag tt) where
  rnf Tag{..} = rnf tagName
          `seq` rnf tagKind
          `seq` rnf tagAddr
          `seq` rnf tagDefinition
          `seq` rnf tagFields

type CTag = Tag 'CTag
type ETag = Tag 'ETag

type TagMap tt = Map TagFileName [Tag tt]
type CTagMap = TagMap 'CTag
type ETagMap = TagMap 'ETag

-- | Total order relation on 'Tag' elements.
--
-- It sorts type classes / type families ('TkTypeClass', 'TkTypeFamily',
-- 'TkDataTypeFamily')  before instances ('TkTypeClassInstance',
-- 'TkTypeFamilyInstance', 'TkDataTypeFamilyInstance'); but also (as a side
-- effect of keeping transitivity property) it will put type classes and their
-- instances before other kinds.
--
-- It satisfies the following properties:
--
-- * anti-symmetry
-- * reflexivity
-- * transitivity
-- * partial consistency with 'Eq' instance: 
--
--   prop> a == b => compareTags a b == EQ
--
compareTags :: forall (tt :: TagType). Ord (TagAddress tt) => Tag tt -> Tag tt -> Ordering
compareTags t0 t1 = on compare tagName t0 t1
                    -- sort type classes / type families before their instances,
                    -- and take precendence over a file where they are defined.
                    -- 
                    -- This will also sort type classes and instances before any
                    -- other terms.
                 <> on compare getTkClass  t0 t1
                 <> on compare tagAddr     t0 t1
                 <> on compare tagKind     t0 t1

    where
      getTkClass :: Tag tt -> Maybe (TagKind tt)
      getTkClass t = case tagKind t of
        TkTypeClass              -> Just TkTypeClass
        TkTypeClassInstance      -> Just TkTypeClassInstance
        TkTypeFamily             -> Just TkTypeFamily
        TkTypeFamilyInstance     -> Just TkTypeFamilyInstance
        TkDataTypeFamily         -> Just TkDataTypeFamily
        TkDataTypeFamilyInstance -> Just TkDataTypeFamilyInstance
        _                        -> Nothing

--
--  GHC interface
--

-- | Create a 'Tag' from 'GhcTag'.
--
ghcTagToTag :: SingTagType tt -> DynFlags -> GhcTag -> Maybe (TagFileName, Tag tt)
ghcTagToTag sing dynFlags GhcTag { gtSrcSpan, gtTag, gtKind, gtIsExported, gtFFI } =
    case gtSrcSpan of
      UnhelpfulSpan {} -> Nothing
      RealSrcSpan realSrcSpan _ ->
        Just . (fileName realSrcSpan, ) $ Tag
          { tagName       = TagName tagName 

          , tagAddr       = TagLineCol (srcSpanStartLine realSrcSpan)
                                       (srcSpanStartCol realSrcSpan)

          , tagKind       = fromGhcTagKind gtKind

          , tagDefinition = NoTagDefinition

          , tagFields     = (    staticField
                              <> ffiField
                              <> kindField
                            ) sing
          }

  where
    fileName = TagFileName . Text.decodeUtf8 . bytesFS . srcSpanFile

    tagName = Text.decodeUtf8 gtTag

    fromGhcTagKind :: GhcTagKind -> TagKind tt
    fromGhcTagKind = \case
      GtkModule                    -> TkModule
      GtkTerm                      -> TkTerm
      GtkFunction                  -> TkFunction
      GtkTypeConstructor {}        -> TkTypeConstructor
      GtkDataConstructor {}        -> TkDataConstructor
      GtkGADTConstructor {}        -> TkGADTConstructor
      GtkRecordField               -> TkRecordField
      GtkTypeSynonym {}            -> TkTypeSynonym
      GtkTypeSignature {}          -> TkTypeSignature
      GtkTypeKindSignature {}      -> TkTypeSignature
      GtkPatternSynonym            -> TkPatternSynonym
      GtkTypeClass                 -> TkTypeClass
      GtkTypeClassMember {}        -> TkTypeClassMember
      GtkTypeClassInstance {}      -> TkTypeClassInstance
      GtkTypeFamily {}             -> TkTypeFamily
      GtkTypeFamilyInstance {}     -> TkTypeFamilyInstance
      GtkDataTypeFamily {}         -> TkDataTypeFamily
      GtkDataTypeFamilyInstance {} -> TkDataTypeFamilyInstance
      GtkForeignImport             -> TkForeignImport
      GtkForeignExport             -> TkForeignExport

    -- static field (wheather term is exported or not)
    staticField :: SingTagType tt -> TagFields tt
    staticField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          if gtIsExported
            then mempty
            else [fileField]

    -- ffi field
    ffiField :: SingTagType tt -> TagFields tt
    ffiField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          case gtFFI of
            Nothing  -> mempty
            Just ffi -> [TagField "ffi" $ Text.pack ffi]


    -- 'TagFields' from 'GhcTagKind'
    kindField :: SingTagType tt -> TagFields tt
    kindField = \case
      SingETag -> NoTagFields
      SingCTag ->
        case gtKind of
          GtkTypeClassInstance hsType ->
            mkField "instance" hsType

          GtkTypeFamily (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkDataTypeFamily (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkTypeSignature hsSigWcType ->
            mkField typeFieldName hsSigWcType

          GtkTypeSynonym hsType ->
            mkField typeFieldName hsType

          GtkTypeConstructor (Just hsKind) ->
            mkField kindFieldName hsKind

          GtkDataConstructor decl ->
            TagFields
              [TagField
                { fieldName  = termFieldName
                , fieldValue = render decl
                }]


          GtkGADTConstructor hsType ->
            mkField typeFieldName hsType

          _ -> mempty


    kindFieldName, typeFieldName, termFieldName :: Text
    kindFieldName = "Kind" -- "kind" is reserverd
    typeFieldName = "type"
    termFieldName = "term"

    --
    -- fields
    --
    
    mkField :: Out.Outputable p => Text -> p -> TagFields 'CTag
    mkField fieldName  p =
      TagFields
        [ TagField
            { fieldName
            , fieldValue = render p
            }]

    render :: Out.Outputable p => p -> Text
    render hsType =
        Text.intercalate " " -- remove all line breaks, tabs and multiple spaces
      . Text.words
      . Text.pack
      $ Out.renderWithStyle
          (Out.initSDocContext
            dynFlags
            (Out.setStyleColoured False
              $ Out.mkErrStyle Out.neverQualify))
          (Out.ppr hsType)
