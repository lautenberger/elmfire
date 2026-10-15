#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# ----------------------- Imports & paths -----------------------
from pathlib import Path
import json
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl

# Resolve case directory assuming this file is .../cases/<case>/scripts/postprocess.py
CASE_DIR = Path(__file__).resolve().parents[1]
FIG_DIR = CASE_DIR / "figures"
REP_DIR = CASE_DIR / "report"
OUT_DIR = CASE_DIR / "outputs"
FIG_DIR.mkdir(parents=True, exist_ok=True)
REP_DIR.mkdir(parents=True, exist_ok=True)
OUT_DIR.mkdir(parents=True, exist_ok=True)

# ----------------------- Add customized postprocessing code below -----------------------


# ----------------------- Output Metrics for report -----------------------
# Modify the following name-value pairs, that prepares metric values to be used for report

metrics = {
    "Var1": Var1,
    "Var2": Var2,
    "Var3": Var3,
#    Add more if necesssary
}
(OUT_DIR / "metrics.json").write_text(json.dumps(metrics, indent=2), encoding="utf-8")


print("[OK] Postprocess complete.")
print(f"  - Figures: {FIG_DIR}")
print(f"  - Report snippets: {REP_DIR/'figures.tex'}, {REP_DIR/'metrics.tex'}")
print(f"  - Metrics JSON: {OUT_DIR/'metrics.json'}")
