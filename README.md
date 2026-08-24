# ELMFIRE — Eulerian Level set Model of FIRE spread

ELMFIRE is an operational wildland fire spread model used by fire
agencies, researchers, and engineers to model how wildfires grow across
real landscapes. It couples the Rothermel (and CFFDRS) surface spread
formulations with a level-set front-tracking method, and runs efficiently
in parallel from a laptop up to a large compute cluster.

As part of the [Pyrecast project](https://pyrecast.org), ELMFIRE forecasts
the spread of most large fires in the Continental US.

## What ELMFIRE can do

* **Real-time forecasting** — predict where an active fire will spread.
* **Historical reconstruction** — reconstruct the spread of past fires.
* **Fire behavior potential** — quantify landscape-scale spread, fireline
  intensity, flame length, and crown fire potential.
* **Risk assessment** — estimate annual burn probability and fire severity
  through Monte Carlo simulation.
* **Smoke, spotting & WUI** — model ember (firebrand) transport, smoke
  emissions for HYSPLIT, and structure-to-structure fire spread in the
  wildland–urban interface.

ELMFIRE ingests standard gridded inputs (fuels, topography, weather, and
moisture as GeoTIFFs) and produces georeferenced raster outputs such as
time of arrival, fireline intensity, spread rate, and flame length.

## Documentation

ELMFIRE ships with a complete documentation guide covering installation,
tutorials, the input reference, and verification/validation results.
**Start there** — it is the authoritative source for day-to-day use.

* Project site and guide: [elmfire.io](https://elmfire.io/)
* In-repo docs: [`docs/`](docs/) (getting started, tutorials, technical
  and user reference)
* What's new in each release: [`CHANGELOG.md`](CHANGELOG.md)

## Quick start (Linux)

Tested on a clean Ubuntu Server 24.04 install. See the
[getting started guide](docs/getting_started.rst) for the full procedure,
including the CloudFire data microservices used for real fuel and weather.

```bash
# 1. Install build prerequisites
sudo apt-get update && sudo apt-get install -y \
    bc csvkit gdal-bin gfortran git jq libopenmpi-dev \
    openmpi-bin pigz python3 python3-pip unzip wget zip

# 2. Clone the repository
git clone https://github.com/lautenberger/elmfire.git

# 3. Set environment variables (add these to ~/.bashrc)
export ELMFIRE_BASE_DIR=/path/to/elmfire
export ELMFIRE_SCRATCH_BASE=/path/to/scratch
export ELMFIRE_INSTALL_DIR=$ELMFIRE_BASE_DIR/build/linux/bin
export CLOUDFIRE_SERVER=worldgen.cloudfire.io
export PATH=$PATH:$ELMFIRE_INSTALL_DIR:$ELMFIRE_BASE_DIR/cloudfire

# 4. Build the executables
cd $ELMFIRE_BASE_DIR/build/linux
./make_gnu.sh
```

A [Docker image](Dockerfile) is also provided if you prefer a
self-contained environment (`docker compose up`).

### Running your first case

The fastest way to learn ELMFIRE is to run it. Work through the
[tutorials](tutorials/), which progress from a constant-wind idealized
case to full simulations with real fuels and weather. After the tutorials,
the [verification cases](verification/) confirm your build reproduces
known reference solutions.

## How a run is configured

A simulation is driven by a single plain-text input file built from
Fortran namelists (`&INPUTS`, `&SIMULATOR`, `&OUTPUTS`, `&MONTE_CARLO`,
`&WUI`, …). Each namelist groups related settings — input rasters, run
control, requested outputs, Monte Carlo perturbations, and so on. Every
parameter is described in the user guide. Worked examples live in
[`examples/`](examples/).

## Background and citation

The mathematical formulation of ELMFIRE is described in its
[original journal article](https://doi.org/10.1016/j.firesaf.2013.08.014):

> Lautenberger, C. (2013). Wildland fire modeling with an Eulerian level
> set method and automated calibration. *Fire Safety Journal*, 62, 289–298.

## License

ELMFIRE is open-source software released under the Eclipse Public
License 2.0 (EPLv2). See [`LICENSE.md`](LICENSE.md).

## Support

Questions, bug reports, and feature requests are welcome as
[GitHub issues](https://github.com/lautenberger/elmfire/issues). You can
also contact Chris Lautenberger at
[chris@cloudfire.com](mailto:chris@cloudfire.com).
