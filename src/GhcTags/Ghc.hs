-- | Generate tags from @'HsModule' 'GhcPs'@ representation.
--
module GhcTags.Ghc
  ( GhcTag (..)
  , GhcTagKind (..)
  , getGhcTags
  , hsDeclsToGhcTags
  ) where

import Data.ByteString (ByteString)
import Data.Maybe
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Hs (HsModule(..), NoExtField(..))
import GHC.Hs.Binds
import GHC.Hs.Decls hiding (famResultKindSignature)
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC.Hs.ImpExp
import GHC.Hs.Type hiding (hsSigWcType)
import GHC.Parser.Annotation
import GHC.Types.Name (nameOccName, occNameFS)
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Unit.Module.Name

-- | Kind of the term.
--
data GhcTagKind
    = GtkModule
    | GtkTerm
    | GtkFunction
    | GtkTypeConstructor        (Maybe (HsKind GhcPs))

    -- | H98 data construtor
    | GtkDataConstructor               (ConDecl GhcPs)

    -- | GADT constructor with its type
    | GtkGADTConstructor               (ConDecl GhcPs)
    | GtkRecordField
    | GtkTypeSynonym                   (HsType GhcPs)
    | GtkTypeSignature                 (HsWildCardBndrs GhcPs (LHsSigType GhcPs))
    | GtkTypeKindSignature             (LHsSigType GhcPs)
    | GtkPatternSynonym
    | GtkTypeClass
    | GtkTypeClassMember               (HsWildCardBndrs GhcPs (LHsSigType GhcPs))
    | GtkTypeClassInstance             (HsType GhcPs)
    | GtkTypeFamily             (Maybe ([HsTyVarBndr () GhcPs], Either (HsKind GhcPs) (HsTyVarBndr () GhcPs)))
    | GtkTypeFamilyInstance     (TyFamInstDecl GhcPs)
    | GtkDataTypeFamily         (Maybe ([HsTyVarBndr () GhcPs], Either (HsKind GhcPs) (HsTyVarBndr () GhcPs)))
    | GtkDataTypeFamilyInstance (Maybe (HsKind GhcPs))
    | GtkForeignImport
    | GtkForeignExport

-- | We can read names from using fields of type 'GHC.Hs.Extensions.IdP' (a type
-- family) which for @'Parsed@ resolved to 'RdrName'
--
data GhcTag = GhcTag {
    gtSrcSpan    :: SrcSpan
    -- ^ term location
  , gtTag        :: ByteString
    -- ^ utf8 encoded tag's name
  , gtKind       :: GhcTagKind
    -- ^ tag's kind
  , gtIsExported :: Bool
    -- ^ 'True' iff the term is exported
  , gtFFI        :: Maybe String
    -- ^ @ffi@ import
  }

-- | Check if an identifier is exported.
--
isExported :: Maybe [IE GhcPs] -> LocatedN RdrName -> Bool
isExported Nothing   _name = True
isExported (Just ies) (L _ name) =
    any (\ie -> listToMaybe (ieNames ie) == Just name) ies

-- | Check if a class member or a type constructors is exported.
--
isMemberExported :: Maybe [IE GhcPs]
                 -> LocatedN RdrName -- member name / constructor name
                 -> LocatedN RdrName -- type class name / type constructor name
                 -> Bool
isMemberExported Nothing    _memberName _className = True
isMemberExported (Just ies) memberName  className  = any go ies
  where
    go :: IE GhcPs -> Bool

    go (IEVar _ (L _ n)) = ieWrappedName n == unLoc memberName

    go (IEThingAbs _ _)  = False

    go (IEThingAll _ (L _ n)) = ieWrappedName n == unLoc className

    go (IEThingWith _ _ IEWildcard{} _) = True

    go (IEThingWith _ (L _ n) NoIEWildcard ns) =
            ieWrappedName n == unLoc className
         && isInWrappedNames
      where
        -- the 'NameSpace' does not agree between things that are in the 'IE'
        -- list and passed member or type class names (constructor / type
        -- constructor names, respectively)
        isInWrappedNames = any ((== occNameFS (rdrNameOcc (unLoc memberName))) . occNameFS . rdrNameOcc . ieWrappedName . unLoc) ns

    go _ = False


-- | Create a 'GhcTag', effectively a smart constructor.
--
mkGhcTag :: LocatedN RdrName
         -- ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level identifier.
         -> GhcTagKind
         -- ^ tag's kind
         -> Bool
         -- ^ is term exported
         -> GhcTag
mkGhcTag (L SrcSpanAnn { locA = gtSrcSpan } rdrName) gtKind gtIsExported =
    case rdrName of
      Unqual occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Qual _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      -- Orig is the only one we are interested in
      Orig _ occName ->
        GhcTag { gtTag = bytesFS (occNameFS occName)
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }

      Exact eName ->
        GhcTag { gtTag = bytesFS (occNameFS (nameOccName eName))
               , gtSrcSpan
               , gtKind
               , gtIsExported
               , gtFFI = Nothing
               }


-- | Generate tags for a module - simple walk over the syntax tree.
--
-- Supported identifiers:
--
--  * /module name/
--  * /top level terms/
--  * /data types/
--  * /record fields/
--  * /type synonyms/
--  * /type classes/
--  * /type class members/
--  * /type class instances/
--  * /type families/
--  * /type family instances/
--  * /data type families/
--  * /data type families instances/
--  * /data type family instances constructors/
--
getGhcTags :: Located HsModule
           -> [GhcTag]
getGhcTags (L _ HsModule { hsmodName, hsmodDecls, hsmodExports }) =
     maybeToList (mkModNameTag <$> hsmodName)
  ++ hsDeclsToGhcTags mies hsmodDecls
  where
    mies :: Maybe [IE GhcPs]
    mies = map unLoc . unLoc <$> hsmodExports

    mkModNameTag :: LocatedA ModuleName -> GhcTag
    mkModNameTag (L l modName) =
      GhcTag { gtSrcSpan = locA l
             , gtTag = bytesFS $ moduleNameFS modName
             , gtKind = GtkModule
             , gtIsExported = True
             , gtFFI = Nothing
             }

hsDeclsToGhcTags :: Maybe [IE GhcPs]
                 -> [LHsDecl GhcPs]
                 -> [GhcTag]
hsDeclsToGhcTags mies = foldr go []
  where
    fixLoc :: SrcSpan -> GhcTag -> GhcTag
    fixLoc loc gt@GhcTag { gtSrcSpan = UnhelpfulSpan {} } = gt { gtSrcSpan = loc }
    fixLoc _   gt                                         = gt

    -- like 'mkGhcTag' but checks if the identifier is exported
    mkGhcTag' :: SrcSpan
              -- ^ declaration's location; it is useful when the term does not
              -- contain useful inforamtion (e.g. code generated from template
              -- haskell splices).
              -> LocatedN RdrName
              --  ^ @RdrName ~ IdP GhcPs@ it *must* be a name of a top level
              --  identifier.
              -> GhcTagKind
              -- ^ tag's kind
              -> GhcTag
    mkGhcTag' l a k = fixLoc l $ mkGhcTag a k (isExported mies a)


    mkGhcTagForMember :: SrcSpan
                      -- ^ declartion's 'SrcSpan'
                      -> LocatedN RdrName -- member name
                      -> LocatedN RdrName -- class name
                      -> GhcTagKind
                      -> GhcTag
    mkGhcTagForMember decLoc memberName className kind =
      fixLoc decLoc $ mkGhcTag memberName kind
                               (isMemberExported mies memberName className)

    -- Main routine which traverse all top level declarations.
    --
    go :: LHsDecl GhcPs -> [GhcTag] -> [GhcTag]
    go (L SrcSpanAnn { locA = decLoc } hsDecl) tags = case hsDecl of

      -- type or class declaration
      TyClD _ tyClDecl ->
        case tyClDecl of

          -- type family declarations
          FamDecl { tcdFam } ->
            case mkFamilyDeclTags decLoc tcdFam Nothing of
              Just tag -> tag : tags
              Nothing  ->       tags

          -- type synonyms
          SynDecl { tcdLName, tcdRhs = L _ hsType } ->
            mkGhcTag' decLoc tcdLName (GtkTypeSynonym hsType) : tags

          -- data declaration:
          --   type,
          --   constructors,
          --   record fields
          --
          DataDecl { tcdLName, tcdDataDefn } ->
            case tcdDataDefn of
              HsDataDefn { dd_cons, dd_kindSig } ->
                     mkGhcTag' decLoc tcdLName (GtkTypeConstructor (unLoc <$> dd_kindSig))
                   : (mkConsTags decLoc tcdLName . unLoc) `concatMap` dd_cons
                  ++ tags

          -- Type class declaration:
          --   type class name,
          --   type class members,
          --   default methods,
          --   default data type instance
          --
          ClassDecl { tcdLName, tcdSigs, tcdMeths, tcdATs, tcdATDefs } ->
              -- class name
              mkGhcTag' decLoc tcdLName GtkTypeClass
               -- class methods
             : (mkClsMemberTags decLoc tcdLName . unLoc) `concatMap` tcdSigs
               -- default methods
            ++ concatMap (\hsBind -> mkHsBindLRTags decLoc (unLoc hsBind)) tcdMeths
            -- associated types
            ++ ((\a -> mkFamilyDeclTags decLoc a (Just tcdLName)) . unLoc) `mapMaybe` tcdATs
            -- associated type defaults (data type families, type families
            -- (open or closed)
            ++ foldr
                (\(L _ decl@(TyFamInstDecl { tfid_eqn })) tags' ->
                    case tfid_eqn of
                      FamEqn { feqn_rhs = L _ hsType } ->
                        case hsTypeTagName hsType of
                          -- TODO: add a `default` field
                          Just a  -> mkGhcTag' decLoc a (GtkTypeFamilyInstance decl) : tags'
                          Nothing -> tags'
                )
                [] tcdATDefs
            ++ tags

      -- Instance declarations
      --  class instances
      --  type family instance
      --  data type family instances
      --
      InstD _ instDecl ->
        case instDecl of
          -- class instance declaration
          ClsInstD { cid_inst } ->
            case cid_inst of
              ClsInstDecl { cid_poly_ty, cid_tyfam_insts, cid_datafam_insts
                          , cid_binds, cid_sigs } ->
                  case cid_poly_ty of
                    -- TODO: @hsbib_body :: LHsType GhcPs@
                    L _ HsSig { sig_body } ->
                      case mkLHsTypeTag decLoc sig_body of
                        Nothing  ->       tags'
                        Just tag -> tag : tags'
                where
                  tags' =
                       -- type family instances
                       mapMaybe (mkTyFamInstDeclTag decLoc . unLoc) cid_tyfam_insts
                       -- data family instances
                    ++ concatMap (mkDataFamInstDeclTag decLoc . unLoc) cid_datafam_insts
                    -- class methods
                    ++ concatMap (mkHsBindLRTags decLoc . unLoc) cid_binds
                       -- optional method signatures
                    ++ concatMap (mkSigTags decLoc . unLoc) cid_sigs
                    ++ tags

          -- data family instance
          DataFamInstD { dfid_inst } ->
            mkDataFamInstDeclTag decLoc dfid_inst ++ tags

          -- type family instance
          TyFamInstD { tfid_inst } ->
            case mkTyFamInstDeclTag decLoc tfid_inst of
              Nothing  ->       tags
              Just tag -> tag : tags

      -- deriving declaration
      DerivD {} -> tags

      -- value declaration
      ValD _ hsBind  -> mkHsBindLRTags decLoc hsBind ++ tags

      -- signature declaration
      SigD _ sig -> mkSigTags decLoc sig ++ tags

      -- standalone kind signatures
      KindSigD _ stdKindSig ->
        case stdKindSig of
          StandaloneKindSig _ ksName sigType ->
           mkGhcTag' decLoc ksName  (GtkTypeKindSignature sigType) : tags

      -- default declaration
      DefD {} -> tags

      -- foreign declaration
      ForD _ foreignDecl ->
        case foreignDecl of
          ForeignImport { fd_name, fd_fi = CImport _ _ _mheader _ (L _ sourceText) } ->
                case sourceText of
                  NoSourceText -> tag
                  -- TODO: add header information from '_mheader'
                  SourceText s -> tag { gtFFI = Just s }
              : tags
            where
              tag = mkGhcTag' decLoc fd_name GtkForeignImport

          ForeignExport { fd_name } ->
              mkGhcTag' decLoc fd_name GtkForeignExport
            : tags

      WarningD {}   -> tags
      AnnD {}       -> tags

      -- TODO: Rules are named it would be nice to get them too
      RuleD {}      -> tags
      SpliceD {}    -> tags
      DocD {}       -> tags
      RoleAnnotD {} -> tags

    -- generate tags of all constructors of a type
    --
    mkConsTags :: SrcSpan
               -> LocatedN RdrName
               -- name of the type
               -> ConDecl GhcPs
               -- constructor declaration
               -> [GhcTag]

    mkConsTags decLoc tyName con@ConDeclGADT { con_names, con_g_args } =
         (\n -> mkGhcTagForMember decLoc n tyName (GtkGADTConstructor con))
         `map` con_names
      ++ mkHsConDeclGADTDetails decLoc tyName con_g_args

    mkConsTags decLoc tyName con@ConDeclH98  { con_name, con_args } =
        mkGhcTagForMember decLoc con_name tyName
          (GtkDataConstructor con)
      : mkHsConDeclH98Details decLoc tyName con_args

    mkHsLocalBindsTags :: SrcSpan -> HsLocalBinds GhcPs -> [GhcTag]
    mkHsLocalBindsTags decLoc (HsValBinds _ (ValBinds _ hsBindsLR sigs)) =
         -- where clause bindings
         concatMap (mkHsBindLRTags decLoc . unLoc) (bagToList hsBindsLR)
      ++ concatMap (mkSigTags decLoc . unLoc) sigs

    mkHsLocalBindsTags _ _ = []

    mkHsConDeclGADTDetails
      :: SrcSpan
      -> LocatedN RdrName
      -> HsConDeclGADTDetails GhcPs
      -> [GhcTag]
    mkHsConDeclGADTDetails decLoc tyName (RecConGADT (L _ fields) _) =
        foldr f [] fields
      where
        f :: LConDeclField GhcPs -> [GhcTag] -> [GhcTag]
        f (L _ ConDeclField { cd_fld_names }) ts = ts ++ map g cd_fld_names

        g :: LFieldOcc GhcPs -> GhcTag
        g (L _ FieldOcc { foLabel }) =
            mkGhcTagForMember decLoc foLabel tyName GtkRecordField
    mkHsConDeclGADTDetails _ _ _ = []

    mkHsConDeclH98Details
      :: SrcSpan
      -> LocatedN RdrName
      -> HsConDeclH98Details GhcPs
      -> [GhcTag]
    mkHsConDeclH98Details decLoc tyName (RecCon (L _ fields)) =
        foldr f [] fields
      where
        f :: LConDeclField GhcPs -> [GhcTag] -> [GhcTag]
        f (L _ ConDeclField { cd_fld_names }) ts = ts ++ map g cd_fld_names

        g :: LFieldOcc GhcPs -> GhcTag
        g (L _ FieldOcc { foLabel }) =
            mkGhcTagForMember decLoc foLabel tyName GtkRecordField
    mkHsConDeclH98Details _ _ _ = []

    mkHsBindLRTags :: SrcSpan
                   -- ^ declaration's 'SrcSpan'
                   -> HsBindLR GhcPs GhcPs
                   -> [GhcTag]
    mkHsBindLRTags decLoc hsBind = case hsBind of
        FunBind { fun_id, fun_matches } ->
          let binds = map (grhssLocalBinds . m_grhss . unLoc)
                    . unLoc
                    . mg_alts
                    $ fun_matches
          in   mkGhcTag' decLoc fun_id GtkFunction
             : concatMap (mkHsLocalBindsTags decLoc) binds

        -- TODO
        -- This is useful fo generating tags for
        -- ````
        -- Just x = lhs
        -- ```
        PatBind {} -> []

        -- According to the GHC documentation VarBinds are introduced by the
        -- type checker, so ghc-tags will never encounter them.
        VarBind {} -> []

        PatSynBind _ PSB { psb_id, psb_args } ->
          mkGhcTag' decLoc psb_id GtkPatternSynonym : case psb_args of
            RecCon fields ->
              let fldLabel = foLabel . recordPatSynField
              in map (\fld -> mkGhcTag' decLoc (fldLabel fld) GtkRecordField) fields
            _ -> []

    mkClsMemberTags :: SrcSpan -> LocatedN RdrName -> Sig GhcPs -> [GhcTag]
    mkClsMemberTags decLoc clsName (ClassOpSig _ _ lhs hsSigWcType) =
      (\n ->  mkGhcTagForMember decLoc n clsName $
        GtkTypeClassMember HsWC { hswc_ext = NoExtField
                                , hswc_body = hsSigWcType
                                }) `map` lhs
    mkClsMemberTags _ _ _ = []


    mkSigTags :: SrcSpan -> Sig GhcPs -> [GhcTag]
    mkSigTags decLoc (TypeSig   _ lhs hsSigWcType) =
      flip (mkGhcTag' decLoc) (GtkTypeSignature hsSigWcType) `map` lhs
    mkSigTags decLoc (PatSynSig _ lhs hsSigWcType) =
      flip (mkGhcTag' decLoc) (GtkTypeSignature HsWC { hswc_ext = NoExtField
                                                     , hswc_body = hsSigWcType
                                                     }) `map` lhs
    mkSigTags decLoc (ClassOpSig _ _ lhs hsSigWcType) =
      flip (mkGhcTag' decLoc) (GtkTypeSignature HsWC { hswc_ext = NoExtField
                                                     , hswc_body = hsSigWcType
                                                     }) `map` lhs
    mkSigTags _ IdSig {}               = []
    -- TODO: generate theses with additional info (fixity)
    mkSigTags _ FixSig {}              = []
    mkSigTags _ InlineSig {}           = []
    -- SPECIALISE pragmas
    mkSigTags _ SpecSig {}             = []
    mkSigTags _ SpecInstSig {}         = []
    -- MINIMAL pragma
    mkSigTags _ MinimalSig {}          = []
    -- SSC pragma
    mkSigTags _ SCCFunSig {}           = []
    -- COMPLETE pragma
    mkSigTags _ CompleteMatchSig {}    = []

    mkFamilyDeclTags :: SrcSpan
                     -> FamilyDecl GhcPs
                     -- ^ declaration's 'SrcSpan'
                     -> Maybe (LocatedN RdrName)
                     -- if this type family is associate, pass the name of the
                     -- associated class
                     -> Maybe GhcTag
    mkFamilyDeclTags decLoc FamilyDecl { fdLName, fdInfo, fdTyVars, fdResultSig = L _ familyResultSig } assocClsName =
      case assocClsName of
        Nothing      -> Just $ mkGhcTag' decLoc fdLName tk
        Just clsName -> Just $ mkGhcTagForMember decLoc fdLName clsName tk
      where

        mb_fdvars = case fdTyVars of
          HsQTvs { hsq_explicit } -> Just $ unLoc `map` hsq_explicit
        mb_resultsig = famResultKindSignature familyResultSig

        mb_typesig = (,) <$> mb_fdvars <*> mb_resultsig

        tk = case fdInfo of
              DataFamily           -> GtkDataTypeFamily mb_typesig
              OpenTypeFamily       -> GtkTypeFamily     mb_typesig
              ClosedTypeFamily {}  -> GtkTypeFamily     mb_typesig

    -- used to generate tag of an instance declaration
    mkLHsTypeTag :: SrcSpan
                 -- declartaion's 'SrcSpan'
                 -> LHsType GhcPs
                 -> Maybe GhcTag
    mkLHsTypeTag decLoc (L _ hsType) =
      (\a -> fixLoc decLoc $ mkGhcTag a (GtkTypeClassInstance hsType) True)
      <$> hsTypeTagName hsType


    hsTypeTagName :: HsType GhcPs -> Maybe (LocatedN RdrName)
    hsTypeTagName hsType =
      case hsType of
        HsForAllTy {hst_body} -> hsTypeTagName (unLoc hst_body)

        HsQualTy {hst_body}   -> hsTypeTagName (unLoc hst_body)

        HsTyVar _ _ a         -> Just $ a

        HsAppTy _ a _         -> hsTypeTagName (unLoc a)
        HsOpTy _ _ _ a _      -> Just $ a
        HsKindSig _ a _       -> hsTypeTagName (unLoc a)

        _                     -> Nothing


    -- data family instance declaration
    --
    mkDataFamInstDeclTag :: SrcSpan -> DataFamInstDecl GhcPs -> [GhcTag]
    mkDataFamInstDeclTag decLoc DataFamInstDecl { dfid_eqn } =
      case dfid_eqn of
        FamEqn { feqn_tycon, feqn_rhs } ->
          case feqn_rhs of
            HsDataDefn { dd_cons, dd_kindSig } ->
                mkGhcTag' decLoc feqn_tycon
                          (GtkDataTypeFamilyInstance
                            (unLoc <$> dd_kindSig))
              : (mkConsTags decLoc feqn_tycon . unLoc)
                `concatMap` dd_cons

    -- type family instance declaration
    --
    mkTyFamInstDeclTag :: SrcSpan -> TyFamInstDecl GhcPs -> Maybe GhcTag
    mkTyFamInstDeclTag decLoc decl@TyFamInstDecl { tfid_eqn } =
      case tfid_eqn of
        -- TODO: should we check @feqn_rhs :: LHsType GhcPs@ as well?
        FamEqn { feqn_tycon } ->
          Just $ mkGhcTag' decLoc feqn_tycon (GtkTypeFamilyInstance decl)

--
--
--

famResultKindSignature :: FamilyResultSig GhcPs
                       -> Maybe (Either (HsKind GhcPs) (HsTyVarBndr () GhcPs))
famResultKindSignature (NoSig _)           = Nothing
famResultKindSignature (KindSig _ ki)      = Just (Left (unLoc ki))
famResultKindSignature (TyVarSig _ bndr)   = Just (Right (unLoc bndr))
