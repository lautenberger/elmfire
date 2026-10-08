# Chapter 6 spotting-verification coverage

This index maps the Chapter 6 thesis studies and the associated journal paper to standalone cases in this suite.

Source basis:

- Yiren Qin, *Development and Implementation of a Physical Firebrand Model for Landscape-Scale Fire Spread Simulations*, Chapter 6, Sections 6.1--6.5.
- Y. Qin et al., *Simulations of firebrand-driven fire spread in landscape-scale Wildland-Urban-Interface (WUI) and urban conflagration models*, Fire Safety Journal 162 (2026) 104686, DOI 10.1016/j.firesaf.2026.104686.

| Reference study | Verification case | Coverage | Current generated status |
| --- | --- | --- | --- |
| Thesis Tests 6-1, 6-2, 6-3 | `CASE10_WSR` | Baseline 10 m map and native fine/coarse structural discretization | Native outputs exist, but the combined five-variant decision is **NOT EVALUATED** because Tests 6-5/6-6 are capability-gated |
| Thesis Test 6-4 / Fig. 6.12 | `CASE11_WSC` | Fine-grid paired no-surface/surface-coupled synchronization test | **PASS**, 2/2 current-fingerprint variants |
| Thesis Tests 6-5 and 6-6 / Figs. 6.13--6.14 | `CASE10_WSR` | Aligned and misaligned resolution-independent single-structure treatment | **NOT EVALUATED**; current source lacks `USE_RESOLUTION_INDEPENDENT_STRUCTURES` |
| Thesis Section 6.4 / Figs. 6.15--6.16 | `CASE12_TRS` | Four CFL levels and accumulation/ignition sensitivity | **FAIL**, 4/4 current-fingerprint variants; accumulation-history shapes pass but absolute-load and tight ignition-time metrics fail |
| Thesis Section 6.5 / Figs. 6.17--6.18 | `CASE13_PRM` | Complete 120-point generation--separation and 150-point generation--wind designs, explicit metrics, figure and report | **NOT EVALUATED**; 270/270 design points prepared, exact single-structure source capability absent |
| Journal Sections 3.4 and 4 / Figs. 7--9 | `CASE14_SRS` | Full published 15-resolution, 1200 m by 400 m ELMFIRE map/ROS sweep | **NOT RUN**; 15/15 input variants prepared and all five geometry checks pass |

## Execution notes

`CASE14_SRS` is an ordinary executable 15-variant workflow with a maximum simulated time of 150,000 s. Configure `ELMFIRE_BIN` and launch `./run_case.sh` only when the required compute time is available. Its runner contains no hidden prepare-only or long-run guard.

`CASE13_PRM` is technically inadmissible with the current source because the resolution-independent structure source/collector and matching input adapter are absent. Its documented runner therefore performs preprocessing, postprocessing, and report compilation but does not substitute the native multiple-discrete-cell model.

## Evidence policy

Each case is standalone: it owns its contract, commented preprocessing and postprocessing scripts, metrics JSON, figure, and report source/PDF. Reference or expected curves are labeled as such. Only outputs carrying the current input fingerprint contribute to an executable case decision, and missing source capability is reported as **NOT EVALUATED**, never as a pass.
