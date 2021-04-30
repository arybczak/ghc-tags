module GhcTags.Tag
  ( -- * Tag
    TAG_KIND (..)
  , SingTagKind (..)
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

-- | Promoted data type used to disntinguish 'CTAG's from 'ETAG's.
--
data TAG_KIND = CTAG | ETAG


-- | Singletons for promoted types.
--
data SingTagKind (tk :: TAG_KIND) where
    SingCTag :: SingTagKind 'CTAG
    SingETag :: SingTagKind 'ETAG


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
data TagKind (tk :: TAG_KIND) where
    TkModule                 :: TagKind tk
    TkTerm                   :: TagKind tk
    TkFunction               :: TagKind tk
    TkTypeConstructor        :: TagKind tk
    TkDataConstructor        :: TagKind tk
    TkGADTConstructor        :: TagKind tk
    TkRecordField            :: TagKind tk
    TkTypeSynonym            :: TagKind tk
    TkTypeSignature          :: TagKind tk
    TkPatternSynonym         :: TagKind tk
    TkTypeClass              :: TagKind tk
    TkTypeClassMember        :: TagKind tk
    TkTypeClassInstance      :: TagKind tk
    TkTypeFamily             :: TagKind tk
    TkTypeFamilyInstance     :: TagKind tk
    TkDataTypeFamily         :: TagKind tk
    TkDataTypeFamilyInstance :: TagKind tk
    TkForeignImport          :: TagKind tk
    TkForeignExport          :: TagKind tk
    CharKind                 :: Char -> TagKind tk
    NoKind                   :: TagKind tk

instance NFData (TagKind tk) where
  rnf x = x `seq` ()

type CTagKind = TagKind 'CTAG
type ETagKind = TagKind 'ETAG

deriving instance Eq   (TagKind tk)
deriving instance Ord  (TagKind tk)
deriving instance Show (TagKind tk)


newtype ExCommand = ExCommand { getExCommand :: Text }
  deriving (Eq, Ord, Show)


-- | Tag address, either from a parsed file or from Haskell's AST>
--
data TagAddress (tk :: TAG_KIND) where
      -- | Precise addres: line and column.  This is what we infer from @GHC@
      -- AST.
      --
      -- The two arguments are line number and either column number or offset
      -- from the begining of the file.
      --
      TagLineCol :: Int -> Int -> TagAddress tk

      -- | ctags can only use range ex-commands as an address (or a sequence of
      -- them separated by `;`). We parse line number specifically, since they
      -- are useful for ordering tags.
      --
      TagLine :: Int -> TagAddress 'CTAG

      -- | A tag address can be just an ex command.
      --
      TagCommand :: ExCommand -> TagAddress 'CTAG

instance NFData (TagAddress tk) where
  rnf x = x `seq` ()

-- | 'CTag' addresses.
--
type CTagAddress = TagAddress 'CTAG

-- | 'ETag' addresses.
--
type ETagAddress = TagAddress 'ETAG


deriving instance Eq   (TagAddress tk)
deriving instance Ord  (TagAddress tk)
deriving instance Show (TagAddress tk)


-- | Emacs tags specific field.
--
data TagDefinition (tk :: TAG_KIND) where
      TagDefinition   :: Text -> TagDefinition 'ETAG
      NoTagDefinition :: TagDefinition tk

instance NFData (TagDefinition tk) where
  rnf x = x `seq` ()

deriving instance Show (TagDefinition tk)
deriving instance Eq   (TagDefinition tk)

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
data TagFields (tk :: TAG_KIND) where
    NoTagFields :: TagFields 'ETAG

    TagFields   :: [TagField]
                -> TagFields 'CTAG

instance NFData (TagFields tk) where
  rnf NoTagFields    = ()
  rnf (TagFields fs) = rnf fs

deriving instance Show (TagFields tk)
deriving instance Eq   (TagFields tk)
instance Semigroup (TagFields tk) where
    NoTagFields   <> NoTagFields   = NoTagFields
    (TagFields a) <> (TagFields b) = TagFields (a ++ b)
instance Monoid (TagFields 'CTAG) where
    mempty = TagFields mempty
instance Monoid (TagFields 'ETAG) where
    mempty = NoTagFields

type CTagFields = TagFields 'CTAG
type ETagFields = TagFields 'ETAG


-- | Tag record.  For either ctags or etags formats.  It is either filled with
-- information parsed from a tags file or from *GHC* ast.
--
data Tag (tk :: TAG_KIND) = Tag
  { tagName       :: TagName
    -- ^ name of the tag
  , tagKind       :: TagKind tk
    -- ^ ctags specifc field, which classifies tags
  , tagAddr       :: TagAddress tk
    -- ^ address in source file
  , tagDefinition :: TagDefinition tk
    -- ^ etags specific field; only tags read from emacs tags file contain this
    -- field.
  , tagFields     :: TagFields tk
    -- ^ ctags specific field
  }
  deriving (Show, Eq)

instance NFData (Tag tk) where
  rnf Tag{..} = rnf tagName
          `seq` rnf tagKind
          `seq` rnf tagAddr
          `seq` rnf tagDefinition
          `seq` rnf tagFields

type CTag = Tag 'CTAG
type ETag = Tag 'ETAG

type TagMap tk = Map TagFileName [Tag tk]
type CTagMap = TagMap 'CTAG
type ETagMap = TagMap 'ETAG

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
compareTags :: forall (tk :: TAG_KIND). Ord (TagAddress tk) => Tag tk -> Tag tk -> Ordering
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
      getTkClass :: Tag tk -> Maybe (TagKind tk)
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
ghcTagToTag :: SingTagKind tk -> DynFlags -> GhcTag -> Maybe (TagFileName, Tag tk)
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

    fromGhcTagKind :: GhcTagKind -> TagKind tk
    fromGhcTagKind = \case
      --GtkModule                    -> TkModule
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
    staticField :: SingTagKind tk -> TagFields tk
    staticField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          if gtIsExported
            then mempty
            else [fileField]

    -- ffi field
    ffiField :: SingTagKind tk -> TagFields tk
    ffiField = \case
      SingETag -> NoTagFields
      SingCTag ->
        TagFields $
          case gtFFI of
            Nothing  -> mempty
            Just ffi -> [TagField "ffi" $ Text.pack ffi]


    -- 'TagFields' from 'GhcTagKind'
    kindField :: SingTagKind tk -> TagFields tk
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
    
    mkField :: Out.Outputable p => Text -> p -> TagFields 'CTAG
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
