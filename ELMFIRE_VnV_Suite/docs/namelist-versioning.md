# ELMFIRE namelist versioning

The suite treats a case namelist as version-specific executable configuration,
not as the scientific definition of the test. Scientific intent, derivations,
metric rationale, tolerances, and interpretation remain in the case report.
`case.yaml` records only the namelist values that must not change without
invalidating the test.

No `scientific_intent.yaml` is used. Add a separate scientific contract only if
the machine-readable contract becomes too large for `case.yaml` or must be
reused independently of the case metadata.

## Controlled workflow

1. Check out the ELMFIRE revision to be tested and record its commit and dirty
   state. A dirty source tree is permitted for development, but its source-file
   SHA-256 is the authoritative identity.
2. Extract the accepted groups, keys, and defaults directly from
   `build/source/elmfire_namelists.f90`:

   ```bash
   python3 tools/extract_namelist_schema.py \
     --source-root /path/to/elmfire \
     --output /tmp/elmfire-schema.json
   ```

3. Compare that schema with the last reviewed schema. Removed variables cause a
   nonzero exit; moves and changed defaults always require review:

   ```bash
   python3 tools/compare_namelist_schemas.py \
     schemas/elmfire/reviewed.json /tmp/elmfire-schema.json
   ```

4. Validate a case against the new schema and its invariants:

   ```bash
   python3 tools/validate_case_namelist.py CASE_DIR \
     --schema /tmp/elmfire-schema.json
   ```

5. If necessary, generate a candidate outside the case tree. The migration tool
   never overwrites canonical inputs and blocks on every change absent from the
   reviewed catalog:

   ```bash
   python3 tools/migrate_case_namelist.py CASE_DIR \
     --target-schema /tmp/elmfire-schema.json \
     --rules migrations/elmfire/semantic_migrations.yaml \
     --output-dir /tmp/elmfire-namelist-review
   ```

6. Review the source diff and candidate against the report. Add a migration rule
   only when the old and new implementation paths are demonstrably equivalent.
   Many-to-one switches, enum changes, unit changes, changed defaults, and model
   substitutions require case-owner review.
7. Validate the candidate, run the case with the intended executable in a clean
   results area, and compare the established metrics and acceptance criteria.
   Parsing successfully is not verification.
8. Adopt the candidate only after code review. Record the tested ELMFIRE commit,
   schema hash, and migration report with the run evidence. Do not weaken a
   tolerance or change a scientific parameter merely to recover a pass.

## `case.yaml` contract

The contract is intentionally narrow:

```yaml
namelist_contract:
  schema_version: 1
  report: "report/case_body.tex"
  files:
    - path: "elmfire.data.in"
      invariants:
        TIME_CONTROL.SIMULATION_TSTOP: 360.0
        OUTPUTS.DUMP_TIME_OF_ARRIVAL: true
        SIMULATOR.NUM_IGNITIONS: 1
```

Include only values whose alteration changes the experiment or prevents a
required metric from being evaluated: model selectors, enabled/disabled coupled
processes, duration and timestep when scientifically controlled, ensemble size
and seed policy, ignition mode, and required outputs. Do not duplicate the
derivation, purpose, metric explanation, tolerance rationale, or report prose.

If a case intentionally varies a value, list each tracked variant separately or
omit that value from the invariant set and constrain the variant-generating
script. Generated namelists are not the source of truth when a case-local script
recreates them from a canonical template.

Set `template: true` on a file entry only when its canonical namelist contains
standalone `@TOKEN@` directives expanded by the deterministic case-local
preprocessor. The validator checks every literal assignment and invariant in
the template while allowing those explicit directives; normal namelists reject
them. Variant-specific values should be checked in separate file entries only
when they are essential to the experiment.

A capability-only case that intentionally does not execute ELMFIRE records the
exception explicitly instead of inventing a dummy namelist:

```yaml
namelist_contract:
  schema_version: 1
  applicability: "not_applicable"
  reason: "The required source capability and adapter are unavailable."
  report: "report/case_body.tex"
  files: []
```

The validator reports `NOT APPLICABLE` for this reviewed condition. Omitting a
contract, a reason, or a required namelist remains an error.

## Review gates

- **Schema gate:** every explicit key exists in the target source group.
- **Invariant gate:** every value listed in `case.yaml` is present and equal.
- **Migration gate:** every removed, renamed, moved, unit-changed, or enum-changed
  setting has a reviewed rule; unknowns block.
- **Execution gate:** the intended binary completes and produces all required
  artifacts.
- **Scientific gate:** established metrics pass without changing their rationale
  or acceptance criteria.

CASE01--CASE14 follow the same controlled workflow and edit rules as every other
verification case. Their stable case IDs, scientific purposes, and established
acceptance criteria remain authoritative, but their files and metadata are not
subject to a separate immutability policy. Use review candidates and the normal
schema, invariant, execution, and scientific gates before adopting changes.
