# Coupling Tests

The tests included in this folder are designed to evaluate the coupled behavior of multiple subroutines through more complex cases. For example, the subroutine LEVEL_SET_PROPAGATION in elmfire_level_set.f90 invokes numerous other subroutines and functions, making simple input–output verification infeasible. Instead, synthetic scenarios with known solutions will be constructed. Given the complexity of these functions, verification cannot be achieved through a single case; rather, a suite of cases will be developed to assess their behavior under varying conditions. Although it is not possible to anticipate all error-inducing situations, this category of test cases will remain adaptive, with new cases added whenever problems are identified and resolved. If addressing such problems requires introducing new independent subroutines or functions, those components will also be subjected to functionality verification through independent unit tests.

All active coupling cases use the global identifiers registered in
`../CASE_REGISTRY.md`. `CASE01_BET` through `CASE14_SRS` are the Eulerian
spotting-model suite. `CASE20_PIG` through `CASE29_SUP` are repository-native
reformulations of the simplified end-to-end scenarios in Section 3.2 of the
ELMFIRE Guide. Their original harness and reference material are retained under
`__legacy__/` and are not active cases.

`CASE30_MIT` through `CASE33_SDO` are portable, executable-level successors
to the distinct black-box scenarios in Adam Laird's test collection. The
source-to-case mapping, including internal routines that cannot be observed
through the normal ELMFIRE executable, is documented in
`../ADAM_TEST_MIGRATION.md`. The active successors contain no Fortran and do
not depend on the retained `../build_adam/` source tree at runtime.

Use `../skills/elmfire-verification-case/SKILL.md` when creating or reviewing a
general verification case. The historical spotting-specific skill remains
under `__legacy__/spotting_model/` as domain reference material.

Each active case is runnable from a standalone V&V-suite checkout. Runtime
scripts resolve files from their own case root and use only the configured
`ELMFIRE_BIN` executable; they do not inspect or copy files from an ELMFIRE
source checkout. Spotting cases that launch ELMFIRE therefore retain their
reviewed fuel-model tables under their own `data/misc/` directory. Prospective
features are enabled only by explicit, reviewed case metadata, never by a
runtime source-code search.

`CASE40_WSD` verifies vector addition and level-set ellipse construction when
Rothermel wind and slope forcing compete. `CASE42_WAF` verifies the upstream
coupling from 20-ft wind and canopy inputs through ELMFIRE's wind-adjustment
factor into the Rothermel response. Their scalar reference equations are
case-local, but their decisions intentionally cover more than one runtime
component, so they remain coupling tests.

`CASE43_WER` through `CASE49_WTC` form the deterministic WU-E Ellipse--Heat
verification suite. They cover ellipse and material response, design-fire heat
release, the heat-to-spread-rate mapping, both wildland--urban transition
directions, isolated receivers, and uniform-community space/time convergence.
The cases disable firebrands and other unrelated mechanisms, distinguish the
current executable behavior from publication-level model intent, and report an
unavailable or nonselectable pathway as `NOT EVALUABLE` rather than treating a
missing capability as evidence of success. The shared source/document map is
recorded in `../WUE_TRACEABILITY.md`; every case remains independently runnable
and carries the equations and evidence needed for its own decision.
