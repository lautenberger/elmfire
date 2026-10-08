.PHONY: new run run-all run-verification run-validation build-all \
	report-inputs verification-report validation-report reports main clean \
	prepare-run configure

SUITE ?= all
ROOT_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# Find all ELMFIRE config files
ELMFIRE_CONFIGS := $(shell find cases -type f -name 'elmfire.data.in')

# Create a new case: make new CASE=case_id
new:
	@./tools/new_case.sh "$(CASE)"

# Run a single case locally: make run CASE=case_id
run:
	@./cases/"$(CASE)"/run_case.sh

# Run selected cases sequentially (or with Slurm if SLURM=1).
# Examples: make run-all SUITE=verification; make run-all SUITE=validation
run-all:
ifeq ($(SLURM),1)
	@python3 ./tools/run_all.py --suite "$(SUITE)" --slurm
else
	@python3 ./tools/run_all.py --suite "$(SUITE)"
endif

run-verification:
	@python3 ./tools/run_all.py --suite verification $(if $(filter 1,$(SLURM)),--slurm,)

run-validation:
	@python3 ./tools/run_all.py --suite validation $(if $(filter 1,$(SLURM)),--slurm,)

# Rebuild all utilities
build-all:
	@./tools/build_all.sh

# Generate aggregate include lists and scientific decision tables.
report-inputs:
	@python3 ./tools/generate_summary_reports.py

verification-report: report-inputs
	@cd main_report && latexmk -lualatex -silent verification_report.tex

validation-report: report-inputs
	@cd main_report && latexmk -lualatex -silent validation_report.tex

reports: verification-report validation-report

# Backward-compatible aggregate-report target.
main: reports

# Remove simulation rasters, logs, verification variants, Slurm files, LaTeX
# auxiliaries, and ELMFIRE scratch artifacts. Preserve compiled reports and all
# report-build inputs, including figures, result JSON, and generated TeX.
clean:
	@python3 ./tools/clean_artifacts.py --apply

# Prepare a clean evaluation boundary. In addition to the normal cleanup,
# invalidate prior metrics, figures, generated report inputs, and compiled PDFs.
prepare-run:
	@python3 ./tools/clean_artifacts.py --apply --prepare-run

# Optionally update GDAL paths and always repair executable permissions that
# may be lost when the suite is copied to an HPC filesystem.
configure:
	@if [ -n "$(PATH_TO_GDAL)" ]; then \
		for cfg in $(ELMFIRE_CONFIGS); do \
			echo "Updating $$cfg"; \
			python3 ./tools/refresh_gdal_path.py "$$cfg" "$(PATH_TO_GDAL)"; \
		done; \
	else \
		echo "[INFO] PATH_TO_GDAL not supplied; leaving namelists unchanged."; \
	fi
	@echo "[INFO] Restoring user-executable permission on suite shell scripts..."
	@find "$(ROOT_DIR)" -path "$(ROOT_DIR)/.git" -prune -o \
		-type f -name "*.sh" -exec chmod u+x {} +
