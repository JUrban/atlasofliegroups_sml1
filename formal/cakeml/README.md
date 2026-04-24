## CakeML translator playground for F4/FPP verification

This directory is the start of the CakeML side of the plan in `VERIFY_ESTIMATE.md`:
translate small, *pure* list-model checkers into CakeML and prove their basic
correctness theorems (in HOL), before moving on to stateful/hash-table models.

### Build

This assumes you have already built the HOL4 + CakeML toolchain under
`/project/hol/` as described in `HOL_TOOLCHAIN.md`.

```sh
export HOLDIR=/project/hol/HOL
export PATH=/project/hol/polyml/bin:$HOLDIR/bin:$PATH
export LD_LIBRARY_PATH=/project/hol/polyml/lib:${LD_LIBRARY_PATH:-}

cd /project/formal/cakeml
$HOLDIR/bin/Holmake
```

### What is here so far?

- `F4FPPCheckDomainProgScript.sml`: a minimal pure checker (`check_domain_fun`)
  that counts “missing witnesses” in a domain list; translated with the
  (non-monadic) CakeML translator.
- `ParamHashProgScript.sml`: an initial monadic (stateful) model of a
  ParamHash-like bucketed set, translated with the monadic translator.
