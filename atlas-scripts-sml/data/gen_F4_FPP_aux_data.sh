#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ATLAS_SCRIPTS_DIR="$ROOT_DIR/atlas-scripts"
DATA_DIR="$ROOT_DIR/atlas-scripts-sml/data"

tmp_out="$(mktemp)"
trap 'rm -f "$tmp_out"' EXIT

cd "$ATLAS_SCRIPTS_DIR"
../atlas all < "$DATA_DIR/gen_F4_FPP_aux_data.at" > "$tmp_out"

awk '/^B /{print $2" "$3" "$4" "$5" "$6}' "$tmp_out" > "$DATA_DIR/F4_FPP_barycenters.txt"
awk '/^L /{print $2" "$3" "$4" "$5" "$6" "$7}' "$tmp_out" > "$DATA_DIR/F4_FPP_lambdas.txt"

echo "Wrote:"
wc -l "$DATA_DIR/F4_FPP_barycenters.txt" "$DATA_DIR/F4_FPP_lambdas.txt"

