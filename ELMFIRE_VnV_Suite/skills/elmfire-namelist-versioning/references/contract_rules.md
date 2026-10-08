# Contract selection rules

An invariant belongs in `case.yaml` only when changing it would alter the
experiment or make a required metric unavailable.

Include, when applicable:

- algorithm and model selectors;
- enable/disable flags for the process under test and confounding processes;
- controlled duration, timestep, CFL target, mesh-related namelist values, and
  output cadence;
- ensemble size, seed, and randomization policy;
- ignition count, mode, and controlled timing;
- required output flags used by postprocessing; and
- variant-specific values that distinguish required runs.

Exclude:

- purpose, equations, derivations, expected trends, and assumptions;
- metric definitions, selection rationale, tolerance rationale, and conclusions;
- prose copied from the report;
- host paths, executable paths, scratch paths, and GDAL discovery settings;
- ordinary input filenames unless selecting a different file changes the test;
- values that a generator intentionally varies, unless each generated variant
  is separately contracted; and
- every namelist value merely for completeness.

Prefer explicit settings for critical behavior. If a critical behavior relies
on a source default, either make it explicit in the canonical deck without
changing its value or document the default dependency during review and treat a
default change as blocking.
