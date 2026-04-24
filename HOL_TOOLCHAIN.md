# HOL4 + CakeML toolchain (local install)

This repo uses a local HOL4/CakeML checkout under `hol/` (ignored by git) to
support the staged formalization plan in `VERIFY_ESTIMATE.md`.

**Pinned commits**
- HOL4: `77fad203c275899bfee21faf6d2d3a66e749d7e1`
- CakeML: `f46cb7a02ef250d89bbcdfde119a8095616c6c4f`

**Local paths**
- Poly/ML (built from source): `hol/polyml/bin/poly`
- HOL4: `hol/HOL/` (with `hol/HOL/bin/hol`, `hol/HOL/bin/Holmake`)
- CakeML: `hol/cakeml/`

**Environment**
When using the locally built Poly/ML, set:
- `PATH=hol/polyml/bin:hol/HOL/bin:$PATH`
- `LD_LIBRARY_PATH=hol/polyml/lib:$LD_LIBRARY_PATH`
- `HOLDIR=$(pwd)/hol/HOL`

**Quick sanity checks**
- HOL REPL: `(cd hol/HOL && bin/hol)`
- CakeML build (resumes incremental): `(cd hol/cakeml && $HOLDIR/bin/Holmake)`

