# ghc-tags-1.6 (2022-??-??)
* Handle definitions of class methods properly.
* Enable `ImportQualifiedPost`, `MagicHash`, `QuasiQuotes` and
  `TemplateHaskellQuotes` by default.
* Allow parsing files more than once with different configurations.
* Add support for GHC 9.4 and drop support for GHC 8.10.

# ghc-tags-1.5 (2022-05-15)
* Handle errors more gracefully.
* Enable `CApiFFI` by default.

# ghc-tags-1.4 (2022-02-28)
* Add support for GHC 9.2.
* Require aeson >= 2.0.
* Enable `BlockArguments`, `ExplicitNamespaces`, `GADTSyntax`,
  `NumericUnderscores`, `PatternSynonyms` and `UnicodeSyntax` by default.

# ghc-tags-1.3 (2021-05-23)
* Properly escape ex mode search commands.
* Fix parsing of generated ctags file.
* Remove non-ascii kind symbols (ctags).
* Make generation of Ex mode search commands optional (ctags).
* Don't check stored mtimes if there are no tags.

# ghc-tags-1.2 (2021-05-22)
* Fix sorting of ctags.
* Express addresses of ctags as ex commands.
* Improve performance of parsing a ctags file.

# ghc-tags-1.1 (2021-05-18)
* Fix compatibility with GHC 8.10.
* Drop support for GHC 8.8.
* Make output flag `ctags` compatible.
* Increase allocation area to 4MB for better performance.

# ghc-tags-1.0 (2021-05-17)
* Initial release.
