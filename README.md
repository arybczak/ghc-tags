# ghc-tags

[![Build Status](https://github.com/arybczak/ghc-tags/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/arybczak/ghc-tags/actions?query=branch%3Amaster)

A command line tool that generates etags (Emacs) and ctags (Vim and other
editors) for efficient code navigation (jump to definition).

Main features:
* Leverages GHC API to obtain accurate information.
* Uses multiple CPU cores when processing source files.
* Supports fast incremental updates.

Supported file extensions:
* `.hs`
* `.hs-boot`
* `.lhs`
* `.x` (requires `alex`)
* `.hsc` (requires `hsc2hs`)

## Usage

For simple projects, i.e. the ones that don't use C pre-processor in non-trivial
ways nor include any C sources it should be enough to execute `ghc-tags -e` (for
etags) or `ghc-tags -c` (for ctags) in the root directory of the project.

For more complicated projects you need to create the configuration file
(`ghc-tags.yaml` by default). It can contain the following keys:

* `source_paths` - a list of paths for `ghc-tags` to process. Directories are
  traversed recursively.
* `exclude_paths` - a list of paths for `ghc-tags` to exclude from processing.
* `language` - the flavour of Haskell, either `Haskell98` or `Haskell2010`.
* `extensions` - a list of GHC language extensions to enable when parsing. Note
  that GHC needs much less extensions for parsing alone, so you should almost
  never need to override this.
* `cpp_includes` - include paths for the C pre-processor.
* `cpp_options` - other options for the C pre-processor, e.g. defines (usually
  undefined `MIN_VERSION_x` macros will go here).

If any of these keys is not specified, an appropriate default value will be
picked instead. You can inspect the defaults by executing `ghc-tags --default`.

**Note:** it is possible to specify multiple project configurations in the
configuration file by separating them with `---`. For example, here is a
configuration for GHC on Linux (compiler, utils and base):

```yaml
source_paths:
- compiler

cpp_includes:
- _build/stage1/compiler/build
- compiler
- includes/dist-derivedconstants/header

---

source_paths:
- libraries/base

exclude_paths:
- libraries/base/GHC/Conc/POSIX/Const.hsc
- libraries/base/GHC/Event/Windows.hsc
- libraries/base/GHC/Event/Windows/ConsoleEvent.hsc
- libraries/base/GHC/Event/Windows/FFI.hsc
- libraries/base/GHC/IO/Windows/Handle.hsc
- libraries/base/System/CPUTime/Windows.hsc
- libraries/base/tests

cpp_includes:
- _build/stage1/libraries/base/build/include
- includes
- libraries/base/include

---

source_paths:
- libraries/ghc-bignum

exclude_paths:
- libraries/ghc-bignum/src/GHC/Num/Backend/Selected.hs

cpp_includes:
- libraries/ghc-bignum/include

---

source_paths:
- libraries/ghc-boot
- libraries/ghc-boot-th
- libraries/ghc-compact
- libraries/ghc-heap
- libraries/ghc-prim

exclude_paths:
- libraries/ghc-compact/tests
- libraries/ghc-heap/tests
- libraries/ghc-prim/tests
```

## Acknowledgments

Thanks to Marcin Szamotulski for his work on
[ghc-tags-plugin](https://github.com/coot/ghc-tags-plugin) `ghc-tags` is based on.
