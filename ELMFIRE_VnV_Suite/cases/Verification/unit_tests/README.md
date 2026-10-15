# Functionality Test (unit test)

The independent functionality verification tests evaluate the designed behavior of all fire model–related subroutines/functions in ELMFIRE (unit tests), as well as subroutines not directly related to the fire model but modified from the original release. These tests aim to verify that individual subroutines or functions produce consistent outputs when executed in isolation. Each test is conducted under conditions with a known response, where inputs are provided to the subroutine/function and the outputs are assessed based on (1) accuracy and (2) computational cost (the latter may also be assessed through global profiling). The tested subroutines/functions are designed to be independent of other components, allowing their corresponding test cases to be implemented efficiently and, if desired, verified after installation.

CASE34--CASE39 and CASE41 are executable-level known-response tests of the
Rothermel surface-spread implementation. They use homogeneous planar fronts
and independent, case-local analytical calculations to isolate standard fuel
models, wind, slope, dead and live moisture, dynamic curing, and public custom
fuel-table parameters. Although they invoke the installed ELMFIRE executable,
their acceptance targets one deterministic model response rather than a
multi-component landscape interaction.
