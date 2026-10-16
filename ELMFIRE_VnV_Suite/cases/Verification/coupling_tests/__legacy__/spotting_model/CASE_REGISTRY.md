# Spotting verification case registry

This registry is the authoritative map between stable suite identifiers, verification purposes, and supporting source coverage. Case identifiers and paths remain independent of publication numbering; source material is cited only to document provenance.

| ID | Purpose abbreviation | Verification purpose | Principal source coverage |
| --- | --- | --- | --- |
| `CASE01_BET` | BET | Biomass emission-time resolution independence | Qin dissertation, Eq. 3.11 and Fig. 3.3 |
| `CASE02_FGR` | FGR | Finite-duration firebrand generation | Qin dissertation, Ch. 3 generation-time formulation |
| `CASE03_ETC` | ETC | Eulerian transport convergence | Qin dissertation, Fig. 4.11 |
| `CASE04_STI` | STI | Steady transport and immediate ignition | Qin dissertation, Tests 4-1 and 5-1 |
| `CASE05_TDW` | TDW | Firebrand transport in time-dependent wind | Qin dissertation, Sec. 4.4.2 |
| `CASE06_F2D` | F2D | Two-dimensional firebrand-driven spread | Qin dissertation, Sec. 4.4.2 and Fig. 4.17 |
| `CASE07_IPC` | IPC | Ignition probability and SFT convergence | Qin dissertation, Test 5-2 and Figs. 5.13--5.14 |
| `CASE08_FBC` | FBC | Deposited firebrand consumption | Qin dissertation, Sec. 5.5.2 |
| `CASE09_MMD` | MMD | Mixed-mode ignition-delay effects | Qin dissertation, Tests 5-3 and 5-4 |
| `CASE10_WSR` | WSR | WUI structural resolution sensitivity | Qin dissertation, Tests 6-1, 6-2, 6-3, 6-5, and 6-6 |
| `CASE11_WSC` | WSC | WUI surface-coupling synchronization | Qin dissertation, Test 6-4 |
| `CASE12_TRS` | TRS | WUI temporal-resolution sensitivity | Qin dissertation, Sec. 6.4 |
| `CASE13_PRM` | PRM | WUI parametric response maps | Qin dissertation, Sec. 6.5; Qin et al. (2026), Sec. 3.3 |
| `CASE14_SRS` | SRS | Two-dimensional spatial-resolution sensitivity | Qin et al. (2026), Secs. 3.4 and 4; Qin dissertation, Ch. 6 |

## Canonical report titles

Each PDF title is generated from `report/case_metadata.tex` as `Case ##: <purpose>`. The machine-readable ID remains `CASE##_<PURPOSE>`. Variant and figure names describe the controlled condition or observable and do not encode publication test or figure numbers.

## Execution note

Every active case owns its preprocessor, runner, postprocessor, metrics, figures, and six-file LaTeX project. `CASE13_PRM` is capability-gated and intentionally omits an ELMFIRE stage. `CASE14_SRS` is a long 15-variant workflow and should be launched only when the required compute time is available.
