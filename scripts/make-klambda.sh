#!/usr/bin/env bash
#
# Regenerate klambda/ from the Shen sources in the kernel/ submodule.
#
# USAGE
#   scripts/make-klambda.sh /path/to/shen
#
# The argument is any working Shen executable that accepts the standard
# launcher arguments (`eval -l FILE -e EXPR`), for example shen-scheme (the
# upstream default bootstrap), shen-cl or a previous shen-elisp build. The
# submodule is copied to a scratch directory so that its checkout stays clean.
# Generated KL differs between bootstraps only in gensym numbering.
set -euo pipefail

SHEN="${1:?usage: scripts/make-klambda.sh /path/to/shen}"
SHEN="$(cd "$(dirname "$SHEN")" && pwd)/$(basename "$SHEN")"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

if [ ! -f "$ROOT/kernel/make.shen" ]; then
  echo "kernel/ is empty; run: git submodule update --init" >&2
  exit 1
fi

cp -R "$ROOT/kernel" "$WORK/kernel"
rm -rf "$WORK/kernel/.git" "$WORK/kernel/klambda"
mkdir -p "$WORK/kernel/klambda"
(
  cd "$WORK/kernel"
  "$SHEN" eval -l make.shen -e "(make)" </dev/null
  "$SHEN" eval -l make-stlib.shen -e "(make-stlib)" </dev/null
)

rm -rf "$ROOT/klambda"
mkdir -p "$ROOT/klambda"
cp "$WORK/kernel/klambda/"*.kl "$ROOT/klambda/"
VERSION="$(cd "$ROOT/kernel" && git describe --tags --exact-match 2>/dev/null || git rev-parse HEAD)"
COMMIT="$(cd "$ROOT/kernel" && git rev-parse HEAD)"
printf '%s\n%s\n' "$VERSION" "$COMMIT" > "$ROOT/klambda/SOURCE"
echo "klambda/ regenerated from kernel/ ($VERSION, $COMMIT)"
