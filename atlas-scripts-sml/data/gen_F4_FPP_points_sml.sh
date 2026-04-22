#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

cd "$ROOT_DIR"

overwrite=0
if [[ "${1:-}" == "--overwrite" ]]; then
  overwrite=1
fi

if [[ "$overwrite" == 1 ]]; then
  OVERWRITE=1 poly -q < atlas-scripts-sml/data/gen_F4_FPP_points.sml
else
  poly -q < atlas-scripts-sml/data/gen_F4_FPP_points.sml
fi

echo "Wrote:"
if [[ "$overwrite" == 1 ]]; then
  wc -l atlas-scripts-sml/data/F4_FPP_points.txt
else
  wc -l atlas-scripts-sml/data/F4_FPP_points.txt.new
  echo "To overwrite the tracked fixture, rerun with: $0 --overwrite"
fi

