# ghc-tags-1.11 (2026-??-??)
* Reject a `--threads` value below 1 with a usage error. Earlier versions ended
  the run with an uncaught error from `setNumCapabilities`.
* Enable `BinaryLiterals`, `LinearTypes` and `QualifiedDo` by default, and
  `MultilineStrings` with GHC 9.12 and later.
* Ignore a flag in an `OPTIONS_GHC` pragma that the bundled GHC API doesn't
  know. Earlier versions reported an error and dropped all the tags of the file.
  This made a source tree that is built with a newer compiler hard to index.
* Recover from a broken tags file instead of crashing. A header comment without
  its closing slash and a byte that is not valid UTF-8 both ended the run with
  an uncaught exception.
* Reject a tags file that is only partly readable. Earlier versions stopped at
  the first bad line and dropped every tag after it without a word, and the
  stored modification times stopped those tags from ever coming back. The bad
  line is now reported and all the source files are scanned again.
* Decode source lines and names leniently. A byte that is not valid UTF-8 in a
  source file or in its name no longer crashes the run.
* If the configuration file cannot be parsed, exit with a failure code. The
  error message now goes to standard error. Earlier versions printed the message
  to standard output and exited with success.
* Normalise `exclude_paths` before matching them against source paths. An entry
  that begins with `./` or ends with a path separator now excludes the path it
  names.
* Fix the kind characters of type families in ctags. An uppercase character now
  always marks the family and a lowercase one the instance: `T` and `t` for a
  type family, `D` and `d` for a data type family. Reading a tags file no longer
  swaps `T` with `t`, and a data type family now gets `D` instead of `d`. Delete
  the old tags file to regenerate it from scratch.
* Add support for GHC 9.14 and drop support for GHC 9.8.
* Accept `GHC2021` and `GHC2024` as values of the `language` configuration key.
* Support disabling a language extension in the configuration file by prefixing
  its name with `No`, e.g. `NoStarIsType`.
* Drop the `ghc-lib` dependency and the `ghc-lib` flag. The parser now comes
  from the `ghc` library of the compiler that builds `ghc-tags`. The supported
  syntax is the syntax of that compiler.

# ghc-tags-1.10 (2025-11-19)
* Add support for GHC 9.12 and drop support for GHC 9.6.

# ghc-tags-1.9 (2024-06-07)
* Add support for GHC 9.10 and drop support for GHC 9.4.

# ghc-tags-1.8 (2024-01-24)
* Add support for GHC 9.8 and drop support for GHC 9.2.

# ghc-tags-1.7 (2023-06-29)
* Add support for GHC 9.6 and drop support for GHC 9.0.

# ghc-tags-1.6 (2023-01-30)
* Handle definitions of class methods properly.
* Enable `ImportQualifiedPost`, `MagicHash`, `QuasiQuotes` and
  `TemplateHaskellQuotes` by default.
* Allow parsing files more than once with different configurations.
* Add support for GHC 9.4 and drop support for GHC 8.10.
* Handle record pattern synonyms properly.

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
