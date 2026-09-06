#!/usr/bin/env bash
#
# Build ghc-tags, run it on the sources of this repository in both formats and
# check the result. ghc-tags exits with 0 even when it fails to parse a file, it
# only reports the file: a parse error goes to stdout, an error of the external
# preprocessor goes to stderr. A successful run prints nothing, so any output is
# a failure.

set -euo pipefail

cd "$(dirname "$0")/.."

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT

cabal build

cabal run -v0 ghc-tags -- -c -f "$work_dir/tags" src > "$work_dir/ctags.log" 2>&1
cabal run -v0 ghc-tags -- -e -f "$work_dir/TAGS" src > "$work_dir/etags.log" 2>&1

cat "$work_dir/ctags.log" "$work_dir/etags.log"

if [ -s "$work_dir/ctags.log" ] || [ -s "$work_dir/etags.log" ]; then
  echo "Error: ghc-tags failed to process some files."
  exit 1
fi

for file in tags TAGS; do
  if [ ! -s "$work_dir/$file" ]; then
    echo "Error: $file is empty."
    exit 1
  fi
  echo "$file: $(wc -l < "$work_dir/$file") lines"
done
