# HOL4 formalization (VERIFY_ESTIMATE Stage A)

This directory contains the first HOL4 artifacts for the plan in `VERIFY_ESTIMATE.md`.

**Build**
```sh
export HOLDIR=/project/hol/HOL
export PATH=/project/hol/polyml/bin:$HOLDIR/bin:$PATH
export LD_LIBRARY_PATH=/project/hol/polyml/lib:${LD_LIBRARY_PATH:-}

cd formal/hol4
$HOLDIR/bin/Holmake
```

**Theories**
- `F4FPPVerifySpec` (`F4FPPVerifySpecScript.sml`): abstract set semantics (`U_slow`, `complete_rel`, `missing_witness`, and an abstract `D_slow`).
- `F4FPPVerifyAlg` (`F4FPPVerifyAlgScript.sml`): pure list model of the slow checker (`check_domain_fun`) and lemmas connecting `check_domain_fun = 0` to `complete_rel`.

