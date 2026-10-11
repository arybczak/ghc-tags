#!/usr/bin/env bash
#
# Run ghc-tags on the sources of this repository in both formats and check the
# result. ghc-tags exits with 0 even when it fails to parse a file, it only
# reports the file on stderr. A successful run prints nothing, so any output is
# a failure.
#
# Usage: check-tags.sh [GHC_TAGS]
#
# GHC_TAGS is the path to a ghc-tags binary. Without it, the script builds
# ghc-tags from this repository.

set -euo pipefail

if [ $# -gt 0 ]; then
  if [ ! -x "$1" ]; then
    echo "Error: $1 is not an executable file."
    exit 1
  fi
  ghc_tags=$(realpath "$1")
fi

cd "$(dirname "$0")/.."

if [ -z "${ghc_tags-}" ]; then
  cabal build
  ghc_tags=$(cabal list-bin ghc-tags)
fi

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT

"$ghc_tags" -c -f "$work_dir/tags" src > "$work_dir/ctags.log" 2>&1
"$ghc_tags" -e -f "$work_dir/TAGS" src > "$work_dir/etags.log" 2>&1

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
