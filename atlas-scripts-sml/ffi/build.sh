#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FFI_DIR="$ROOT_DIR/atlas-scripts-sml/ffi"
BUILD_DIR="$FFI_DIR/build"

mkdir -p "$BUILD_DIR"

CXX="${CXX:-g++}"

INCLUDES=(
  "-I$ROOT_DIR/sources"
  "-I$ROOT_DIR/sources/utilities"
  "-I$ROOT_DIR/sources/structure"
  "-I$ROOT_DIR/sources/gkmod"
  "-I$ROOT_DIR/sources/io"
  "-I$ROOT_DIR/sources/error"
)

CXXFLAGS=(
  "-O3"
  "-DNDEBUG"
  "-fPIC"
  "-std=gnu++14"
  "${INCLUDES[@]}"
)

echo "Compiling Atlas core (utilities/structure/error) as PIC objects..."

compile_dir() {
  local dir="$1"
  local outdir="$2"
  local exclude_re="${3:-}"
  shopt -s nullglob
  local cpp
  for cpp in "$dir"/*.cpp; do
    local base
    base="$(basename "$cpp" .cpp)"
    if [[ -n "$exclude_re" && "$base" =~ $exclude_re ]]; then
      continue
    fi
    local obj="$outdir/$base.o"
    if [[ ! -f "$obj" || "$cpp" -nt "$obj" ]]; then
      echo "  $cpp"
      "$CXX" "${CXXFLAGS[@]}" -c "$cpp" -o "$obj"
    fi
  done
}

compile_dir "$ROOT_DIR/sources/utilities" "$BUILD_DIR"
compile_dir "$ROOT_DIR/sources/structure" "$BUILD_DIR"
compile_dir "$ROOT_DIR/sources/gkmod" "$BUILD_DIR"
compile_dir "$ROOT_DIR/sources/io" "$BUILD_DIR" '^interactive'
compile_dir "$ROOT_DIR/sources/error" "$BUILD_DIR"

echo "Compiling SML FFI wrapper..."
"$CXX" "${CXXFLAGS[@]}" -c "$FFI_DIR/atlas_smlffi.cpp" -o "$BUILD_DIR/atlas_smlffi.o"

echo "Linking $FFI_DIR/libatlas_smlffi.so ..."
"$CXX" -shared -o "$FFI_DIR/libatlas_smlffi.so" "$BUILD_DIR"/*.o

echo "Done."
