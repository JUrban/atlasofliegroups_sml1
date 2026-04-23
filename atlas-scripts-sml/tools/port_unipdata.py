#!/usr/bin/env python3
"""
port_unipdata.py

Generate SML translations of Atlas `*_unipdata.at` tables.

The `.at` tables are typically of the form:

  set NAME=[(int,ratvec,ratvec)]:
  [
    (x,[...]/den,[...]/den),
    ...
  ]##[...]#(...)

This script extracts every `(x,[...]/d,[...]/d)` triple occurring in the file
and emits a simple SML module:

  structure <BaseName> = struct
    val data : (int * ratvec * ratvec) list = [ ... ]
  end

The output is intended to be checked into `atlas-scripts-sml/` as a dependency-
free data module.
"""

from __future__ import annotations

import argparse
import pathlib
import re
from typing import List, Tuple


ENTRY_RE = re.compile(
    r"\(\s*(\d+)\s*,\s*\[([^\]]*)\]\s*/\s*(\d+)\s*,\s*\[([^\]]*)\]\s*/\s*(\d+)\s*\)"
)


def parse_entries(text: str) -> List[Tuple[int, int, List[str], int, List[str]]]:
    entries: List[Tuple[int, int, List[str], int, List[str]]] = []
    for m in ENTRY_RE.finditer(text):
        x = int(m.group(1))
        lam_nums = [t.strip() for t in m.group(2).split(",") if t.strip()]
        lam_den = int(m.group(3))
        nu_nums = [t.strip() for t in m.group(4).split(",") if t.strip()]
        nu_den = int(m.group(5))
        entries.append((x, lam_den, lam_nums, nu_den, nu_nums))
    return entries


def to_sml_int(tok: str) -> str:
    tok = tok.strip()
    if tok.startswith("-"):
        return "~" + tok[1:]
    return tok


def sml_list(nums: List[str]) -> str:
    return "[" + ", ".join(to_sml_int(t) for t in nums) + "]"


def emit_sml(struct_name: str, at_basename: str, entries) -> str:
    lines: List[str] = []
    lines.append('use "atlas-scripts-sml/Lattice.sml";')
    lines.append("")
    lines.append("(*")
    lines.append(f"  File: atlas-scripts-sml/{struct_name}.sml")
    lines.append("")
    lines.append("  Purpose")
    lines.append(f"  - Direct Standard ML translation of `atlas-scripts/{at_basename}`.")
    lines.append("  - Exposes the hand-curated unipotent data table as triples `(x,lambda,nu)`.")
    lines.append("")
    lines.append("  Notes")
    lines.append("  - This file only packages the data; it does not compute/verify it.")
    lines.append("  - Rational vectors are normalized via `Lattice.ratvecNormalize`.")
    lines.append("*)")
    lines.append("")
    lines.append(f"structure {struct_name} = struct")
    lines.append("  type ratvec = Lattice.ratvec")
    lines.append("  type entry = int * ratvec * ratvec")
    lines.append("")
    lines.append("  fun rv (den: int, nums: int list) : ratvec =")
    lines.append("    Lattice.ratvecNormalize {den = den, nums = nums}")
    lines.append("")
    lines.append("  val data : entry list =")
    if not entries:
        lines.append("    []")
    else:
        x, lam_den, lam_nums, nu_den, nu_nums = entries[0]
        lines.append(f"    [ ({x}, rv ({lam_den}, {sml_list(lam_nums)}), rv ({nu_den}, {sml_list(nu_nums)}))")
        for x, lam_den, lam_nums, nu_den, nu_nums in entries[1:]:
            lines.append(
                f"    , ({x}, rv ({lam_den}, {sml_list(lam_nums)}), rv ({nu_den}, {sml_list(nu_nums)}))"
            )
        lines.append("    ]")
    lines.append("end")
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("input_at", type=pathlib.Path)
    ap.add_argument("output_sml", type=pathlib.Path)
    args = ap.parse_args()

    text = args.input_at.read_text()
    entries = parse_entries(text)
    struct_name = args.output_sml.stem
    out = emit_sml(struct_name, args.input_at.name, entries)
    args.output_sml.parent.mkdir(parents=True, exist_ok=True)
    args.output_sml.write_text(out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

