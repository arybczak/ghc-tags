# ghc-tags

[![Build Status](https://github.com/arybczak/ghc-tags/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/arybczak/ghc-tags/actions?query=branch%3Amaster)

A command line tool that generates etags (Emacs) and ctags (Vim and other
editors) for efficient code navigation (jump to definition).

Main features:
* Leverages GHC API to obtain accurate information.
* Uses multiple CPU cores when processing source files.
* Supports fast incremental updates.

## Usage

For simple projects, i.e. the ones that don't use C pre-processor in non-trivial
ways nor include any C sources it should be enough to execute `ghc-tags -e` or
`ghc-tags -c` in the root directory of the project.

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
picked instead. You can inspect the defaults by executing `ghc-tags
--default-config`.

**Note:** it is possible to specify multiple project configurations in the
configuration file by separating them with `---`. For example, here is a
configuration for GHC (compiler + base):

```yaml
source_paths:
- compiler

cpp_includes:
- compiler
- compiler/stage1/build
- includes/dist-derivedconstants/header
- _build/stage1/compiler/build

---

source_paths:
- libraries/base

exclude_paths:
- libraries/base/tests

cpp_includes:
- libraries/base/include
```

## Acknowledgments

Thanks to Marcin Szamotulski for his work on
[ghc-tags-plugin](https://github.com/coot/ghc-tags-plugin) `ghc-tags` is based on.
