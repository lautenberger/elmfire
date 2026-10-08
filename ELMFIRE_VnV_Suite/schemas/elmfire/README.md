# Extracted ELMFIRE schemas

These JSON files are machine-generated inventories of accepted namelist groups,
keys, and source defaults. They are review evidence, not a replacement for the
ELMFIRE source. Each file records the source commit, the exact namelist-reader
SHA-256, and whether the surrounding checkout was dirty.

`efa0b8fa-working-tree.json` was extracted from the local `elmfire-nick`
checkout at commit `efa0b8fa3935ea4cd30a2bd054c7e9b83db81163`. The checkout
contained unrelated working-tree changes, so the snapshot is deliberately
labelled `working-tree`; the recorded source-file hash identifies the reader
used for extraction.

For a release gate, generate a schema from a clean tagged checkout and name it
for the tag or full commit. Review the diff before replacing or adding a
baseline.
