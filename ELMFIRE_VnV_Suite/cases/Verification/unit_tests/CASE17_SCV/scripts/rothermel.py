"""
Rothermel Surface Fire Spread Rate Model Implementation

This module implements the Rothermel surface fire spread model for computing
the rate of spread under no wind and no slope conditions, based on the basic
fire spread equations and fuel model parameters.

The Rothermel model calculates surface fire spread rate as:

    R = I_R * ξ / (ρ_b * E_ig)  [ft/min]

Where:
    I_R  = Reaction intensity (depends on fuel properties and moisture)
    ξ    = Propagating flux ratio
    ρ_b  = Ovendry bulk density
    E_ig = Effective heating number

References:
- Rothermel, R. C. (1972). A mathematical model for predicting fire spread in
  wildland fuels. USDA Forest Service Research Paper INT-115, Intermountain
  Forest and Range Experiment Station, Ogden, UT.
- Albini, F. A., & Randerson, D. (1975). Fire spread through a discontinuous
  fuel bed. Western Fire Ecology Center, Missoula, MT.
- Thomas, P. H., Simmons, A. E., & Law, M. E. (1999). Behaviour of multi-storey
  fire test facility. Fire and Materials, 23(5), 207-214.
- ELMFIRE implementation: https://github.com/lautenberger/elmfire

Usage Example:
    from pathlib import Path
    from rothermel import load_fuel_models, RothermelInput, calculate_rothermel_no_wind_no_slope
    
    # Load fuel models from CSV
    csv_path = Path("fuel_models.csv")
    fuel_models = load_fuel_models(csv_path)
    
    # Select a fuel model and set conditions
    fuel_model = fuel_models[2]  # FBFM02
    inputs = RothermelInput(
        fuel_model_id=2,
        m1=5.0,    # 1-hr dead fuel moisture (%)
        m10=7.0,   # 10-hr dead fuel moisture (%)
        m100=9.0,  # 100-hr dead fuel moisture (%)
        mlh=30.0,  # Live herb moisture (%)
        mlw=60.0,  # Live woody moisture (%)
        s_e=0.01,  # Effective mineral content
    )
    
    # Calculate rate of spread (no wind, no slope)
    output = calculate_rothermel_no_wind_no_slope(fuel_model, inputs)
    
    print(f"Rate of Spread: {output.rate_of_spread:.2f} ft/min")
    print(f"Reaction Intensity: {output.reaction_intensity:.1f} Btu/(ft²·min)")
    print(f"Flame Length: {output.flame_length:.1f} ft")
"""

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


FUEL_MODEL_COLUMNS = [
    "id",
    "name",
    "is_grass",
    "w_dead_1h",
    "w_dead_10h",
    "w_dead_100h",
    "w_live_herb",
    "w_live_woody",
    "sigma_dead_1h",
    "sigma_live_herb",
    "sigma_live_woody",
    "depth",
    "moisture_of_extinction_pct",
    "heat_of_combustion",
]

@dataclass
class FuelModelParams:
    """Stores fuel model parameters from CSV."""
    
    id: int                        # Fuel model ID
    name: str                      # Fuel model name
    is_grass: bool                 # Whether this is a grass fuel model
    w_dead_1h: float              # 1-hr dead fuel loading, lb/ft²
    w_dead_10h: float             # 10-hr dead fuel loading, lb/ft²
    w_dead_100h: float            # 100-hr dead fuel loading, lb/ft²
    w_live_herb: float            # Live herb loading, lb/ft²
    w_live_woody: float           # Live woody loading, lb/ft²
    sigma_dead_1h: float          # 1-hr dead fuel surface-area-to-volume ratio, 1/ft
    sigma_dead_10h: float         # 10-hr dead fuel surface-area-to-volume ratio, 1/ft (ELMFIRE constant)
    sigma_dead_100h: float        # 100-hr dead fuel surface-area-to-volume ratio, 1/ft (ELMFIRE constant)
    sigma_live_herb: float        # Live herb surface-area-to-volume ratio, 1/ft
    sigma_live_woody: float       # Live woody surface-area-to-volume ratio, 1/ft
    depth: float                  # Fuel bed depth, ft
    moisture_of_extinction: float # Moisture of extinction, fraction
    heat_of_combustion: float     # Heat of combustion, Btu/lb


@dataclass
class FuelModelTableEntry:
    """ELMFIRE-style precomputed fuel model quantities."""

    id: int
    name: str
    dynamic: bool
    w0: np.ndarray
    sig: np.ndarray
    delta: float
    mex_dead: float
    hoc: float
    rhop: float = 32.0
    st: float = 0.055
    se: float = 0.01
    eps: np.ndarray | None = None
    f: np.ndarray | None = None
    fmex: np.ndarray | None = None
    fw0: np.ndarray | None = None
    fsig: np.ndarray | None = None
    wprime_numer: np.ndarray | None = None
    wprime_denom: np.ndarray | None = None
    mprime_denom: np.ndarray | None = None
    mprime_denom_14_sum: float = 0.0
    r_mprime_denom_14_sum_mex_dead: float = 0.0
    f_dead: float = 0.0
    f_live: float = 0.0
    w0_dead: float = 0.0
    w0_live: float = 0.0
    wn_dead: float = 0.0
    wn_live: float = 0.0
    sig_dead: float = 0.0
    sig_live: float = 0.0
    sig_overall: float = 0.0
    beta: float = 0.0
    beta_op: float = 0.0
    rho_b: float = 0.0
    xi: float = 0.0
    a_coeff: float = 0.0
    b_coeff: float = 0.0
    c_coeff: float = 0.0
    e_coeff: float = 0.0
    gamma_prime_peak: float = 0.0
    gamma_prime: float = 0.0
    tr: float = 0.0
    gp_wnd_emd_es_hoc: float = 0.0
    gp_wnl_eml_es_hoc: float = 0.0
    phisterm: float = 0.0
    phiwterm: float = 0.0
    mex_live: float = 100.0
    unsheltered_waf: float = 0.0


@dataclass
class RothermelInput:
    """Input parameters for Rothermel model calculation."""
    
    fuel_model_id: int
    # Dead fuel moisture contents (% of ovendry weight)
    m1: float = 5.0               # 1-hr dead fuel moisture
    m10: float = 7.0              # 10-hr dead fuel moisture
    m100: float = 9.0             # 100-hr dead fuel moisture
    mlh: float = 60.0             # Live herb moisture
    mlw: float = 60.0             # Live woody moisture
    # Mineral content (fraction)
    s_e: float = 0.01             # Effective mineral content (silica-free)


@dataclass
class RothermelOutput:
    """Output from Rothermel model calculation."""
    
    rate_of_spread: float         # ft/min
    reaction_intensity: float     # Btu/(ft²·min)
    flame_length: float           # ft
    hpua: float                   # Heat per unit area, Btu/ft²


def load_fuel_models(csv_path: Path) -> dict[int, FuelModelParams]:
    """
    Load fuel model parameters from CSV file.
    
    Expected CSV format (no header row, 14 columns):
    ID, Name, IsGrass, w1, w2, w3, w4, w5, sigma1, sigma2, sigma3, depth, param13, h_c
    
    Column mapping:
    0:   Fuel model ID
    1:   Fuel model name
    2:   IsGrass (TRUE/FALSE)
    3-7: Fuel loadings (w_o1 through w_o5), lb/ft²
    8:   Dead 1-hr sigma, 1/ft
    9:   Live herb sigma, 1/ft
    10:  Live woody sigma, 1/ft
    11:  Fuel bed depth (δ), ft
    12:  Moisture of extinction, % in source CSV
    13:  Heat of combustion (h), Btu/lb
    
    Args:
        csv_path: Path to fuel_models.csv file
        
    Returns:
        Dictionary mapping fuel model IDs to FuelModelParams
    """
    fuel_models = {}

    fuel_df = pd.read_csv(csv_path, names=FUEL_MODEL_COLUMNS, header=None)

    for row in fuel_df.itertuples(index=False):
        try:
            fuel_id = int(row.id)
            name = str(row.name).strip()
            bool_token = str(row.is_grass).strip().upper().strip('.')
            is_grass = bool_token in {'TRUE', 'T', '1', 'YES', 'Y'}

            # ELMFIRE convention:
            # - Dead 10-hr and 100-hr classes use fixed constants (109, 30)
            # - CSV columns sigma_live_herb/sigma_live_woody map to live classes
            sigma_dead_10h = 109.0
            sigma_dead_100h = 30.0

            fuel_models[fuel_id] = FuelModelParams(
                id=fuel_id,
                name=name,
                is_grass=is_grass,
                w_dead_1h=float(row.w_dead_1h),
                w_dead_10h=float(row.w_dead_10h),
                w_dead_100h=float(row.w_dead_100h),
                w_live_herb=float(row.w_live_herb),
                w_live_woody=float(row.w_live_woody),
                sigma_dead_1h=float(row.sigma_dead_1h),
                sigma_dead_10h=sigma_dead_10h,
                sigma_dead_100h=sigma_dead_100h,
                sigma_live_herb=float(row.sigma_live_herb),
                sigma_live_woody=float(row.sigma_live_woody),
                depth=float(row.depth),
                moisture_of_extinction=float(row.moisture_of_extinction_pct) / 100.0,
                heat_of_combustion=float(row.heat_of_combustion),
            )
        except (ValueError, TypeError):
            continue  # Skip rows with parsing errors
    
    return fuel_models


def build_fuel_model_table_entry(fuel_model: FuelModelParams, live_herb_moisture: float = 60.0) -> FuelModelTableEntry:
    """Replicate the ELMFIRE fuel-model preprocessing for a single fuel model."""

    w0 = np.zeros(7)
    sig = np.full(7, 9999.0)

    w0[1] = fuel_model.w_dead_1h
    w0[2] = fuel_model.w_dead_10h
    w0[3] = fuel_model.w_dead_100h
    w0[5] = fuel_model.w_live_herb
    w0[6] = fuel_model.w_live_woody

    sig[1] = fuel_model.sigma_dead_1h
    sig[2] = fuel_model.sigma_dead_10h
    sig[3] = fuel_model.sigma_dead_100h
    sig[5] = fuel_model.sigma_live_herb
    sig[6] = fuel_model.sigma_live_woody

    entry = FuelModelTableEntry(
        id=fuel_model.id,
        name=fuel_model.name,
        dynamic=fuel_model.is_grass,
        w0=w0,
        sig=sig,
        delta=fuel_model.depth,
        mex_dead=fuel_model.moisture_of_extinction,
        hoc=fuel_model.heat_of_combustion,
    )

    if entry.dynamic:
        live_frac = min(max((live_herb_moisture - 30.0) / (120.0 - 30.0), 0.0), 1.0)
        dead_frac = 1.0 - live_frac
        w0[4] = dead_frac * w0[5]
        w0[5] = live_frac * w0[5]
        sig[4] = sig[5]
        numerator = (sig[1] * sig[1] * w0[1]) + (sig[4] * sig[4] * w0[4])
        denominator = (sig[1] * w0[1]) + (sig[4] * w0[4])
        if denominator > 0.0:
            sig[1] = numerator / denominator
        w0[1] += w0[4]
        w0[4] = 0.0
        sig[4] = 9999.0
        entry.w0 = w0
        entry.sig = sig
    else:
        w0[4] = 0.0
        sig[4] = 9999.0
        entry.w0 = w0
        entry.sig = sig
    
    # Compute eps for indices 1-6
    sig_array = entry.sig
    eps_array = np.where(sig_array > 0, np.exp(-138.0 / sig_array), 0.0)

    a_values = np.zeros(7)
    w0_array = entry.w0
    sig_array = entry.sig
    for idx in range(1, 7):
        a_values[idx] = sig_array[idx] * w0_array[idx] / entry.rhop

    a_dead = max(float(np.sum(a_values[1:5])), 1e-9)
    a_live = max(float(np.sum(a_values[5:7])), 1e-9)
    a_overall = a_dead + a_live
    entry.f_dead = a_dead / a_overall
    entry.f_live = a_live / a_overall

    f_array = np.zeros(7)
    fmex_array = np.zeros(7)
    fw0_array = np.zeros(7)
    fsig_array = np.zeros(7)
    wprime_numer_array = np.zeros(7)
    wprime_denom_array = np.zeros(7)
    mprime_denom_array = np.zeros(7)
    
    for idx in range(1, 5):
        f_array[idx] = a_values[idx] / a_dead
        fmex_array[idx] = f_array[idx] * entry.mex_dead
        fw0_array[idx] = f_array[idx] * w0_array[idx]
        fsig_array[idx] = f_array[idx] * sig_array[idx]
        wprime_numer_array[idx] = w0_array[idx] * eps_array[idx]
        mprime_denom_array[idx] = w0_array[idx] * eps_array[idx]
    for idx in range(5, 7):
        f_array[idx] = a_values[idx] / a_live
        fw0_array[idx] = f_array[idx] * w0_array[idx]
        fsig_array[idx] = f_array[idx] * sig_array[idx]
        wprime_denom_array[idx] = w0_array[idx] * (np.exp(-500.0 / sig_array[idx]) if sig_array[idx] > 0 else 0.0)

    entry.f = f_array
    entry.fmex = fmex_array
    entry.fw0 = fw0_array
    entry.fsig = fsig_array
    entry.wprime_numer = wprime_numer_array
    entry.wprime_denom = wprime_denom_array
    entry.mprime_denom = mprime_denom_array
    entry.eps = eps_array

    entry.w0_dead = float(np.sum(entry.fw0[1:5]))
    entry.w0_live = float(np.sum(entry.fw0[5:7]))
    entry.wn_dead = entry.w0_dead * (1.0 - entry.st)
    entry.wn_live = entry.w0_live * (1.0 - entry.st)
    entry.sig_dead = float(np.sum(entry.fsig[1:5]))
    entry.sig_live = float(np.sum(entry.fsig[5:7]))
    entry.sig_overall = (a_dead / a_overall) * entry.sig_dead + (a_live / a_overall) * entry.sig_live
    entry.beta = float(np.sum(entry.w0[1:7])) / (entry.delta * entry.rhop)
    entry.beta_op = 3.348 / (entry.sig_overall ** 0.8189) if entry.sig_overall > 0 else 0.0
    entry.rho_b = float(np.sum(entry.w0[1:7])) / max(entry.delta, 1e-9)
    entry.xi = np.exp((0.792 + 0.681 * np.sqrt(entry.sig_overall)) * (0.1 + entry.beta)) / (192.0 + 0.2595 * entry.sig_overall)
    entry.a_coeff = 133.0 / (entry.sig_overall ** 0.7913) if entry.sig_overall > 0 else 0.0
    entry.b_coeff = 0.02526 * entry.sig_overall ** 0.54 if entry.sig_overall > 0 else 0.0
    entry.c_coeff = 7.47 * np.exp(-0.133 * entry.sig_overall ** 0.55) if entry.sig_overall > 0 else 0.0
    entry.e_coeff = 0.715 * np.exp(-0.000359 * entry.sig_overall)
    entry.gamma_prime_peak = entry.sig_overall ** 1.5 / (495.0 + 0.0594 * entry.sig_overall ** 1.5) if entry.sig_overall > 0 else 0.0
    if entry.beta_op > 0.0:
        entry.gamma_prime = entry.gamma_prime_peak * (entry.beta / entry.beta_op) ** entry.a_coeff * np.exp(entry.a_coeff * (1.0 - entry.beta / entry.beta_op))
    else:
        entry.gamma_prime = 0.0
    entry.tr = 384.0 / entry.sig_overall if entry.sig_overall > 0 else 0.0
    entry.gp_wnd_emd_es_hoc = entry.gamma_prime * entry.wn_dead * (0.174 / (entry.se ** 0.19)) * entry.hoc
    entry.gp_wnl_eml_es_hoc = entry.gamma_prime * entry.wn_live * (0.174 / (entry.se ** 0.19)) * entry.hoc
    entry.phisterm = 5.275 * entry.beta ** (-0.3) if entry.beta > 0.0 else 0.0
    entry.phiwterm = entry.c_coeff * (entry.beta / entry.beta_op) ** (-entry.e_coeff) if entry.beta > 0.0 and entry.beta_op > 0.0 else 0.0
    wprime_denom_56_sum = float(np.sum(entry.wprime_denom[5:7]))
    wprime_numer_14_sum = float(np.sum(entry.wprime_numer[1:5]))
    entry.mprime_denom_14_sum = float(np.sum(entry.mprime_denom[1:5]))
    if entry.mprime_denom_14_sum > 1e-9 and entry.mex_dead > 1e-9:
        entry.r_mprime_denom_14_sum_mex_dead = 1.0 / (entry.mprime_denom_14_sum * entry.mex_dead)
    else:
        entry.r_mprime_denom_14_sum_mex_dead = 0.0
    entry.mex_live = 100.0 if wprime_denom_56_sum <= 1e-6 else 2.9 * wprime_numer_14_sum / wprime_denom_56_sum
    entry.unsheltered_waf = 0.0 if entry.delta <= 0.0 else (0.555 / np.sqrt(entry.delta)) if entry.delta > 1e-9 else 0.0
    return entry


def print_fuel_model_table_entry(entry: FuelModelTableEntry) -> None:
    """Print the full ELMFIRE-style fuel table state for one fuel model."""

    print("Fuel Table State:")
    print("  sources: CSV = loaded from fuel_models.csv, ASSUMPTION = loader default, DERIVED = ELMFIRE-style computation")
    print(f"  id:               {entry.id}")
    print(f"  name:             {entry.name}")
    print(f"  dynamic:          {entry.dynamic}  [CSV boolean flag]")
    print(f"  delta:            {entry.delta:.6f} ft  [CSV]")
    print(f"  mex_dead:         {entry.mex_dead:.6f} fraction  [CSV]")
    print(f"  mex_live:         {entry.mex_live:.6f} fraction  [DERIVED from CSV + ELMFIRE formula]")
    print(f"  hoc:              {entry.hoc:.6f} Btu/lb  [CSV]")
    print(f"  rhop:             {entry.rhop:.6f} lb/ft^3  [ELMFIRE assumption]")
    print(f"  st:               {entry.st:.6f} lb/lb  [ELMFIRE assumption]")
    print(f"  se:               {entry.se:.6f} lb/lb  [ELMFIRE assumption]")
    print(f"  w0:               {[round(float(value), 6) for value in entry.w0[1:7]]} lb/ft^2  [CSV]")
    print(f"  sig:              {[round(float(value), 6) for value in entry.sig[1:7]]} 1/ft  [dead-1h CSV, dead-10h/100h ELMFIRE constants, live CSV]")
    print(f"  eps:              {[round(float(value), 6) for value in entry.eps[1:7]] if entry.eps is not None else []} 1  [DERIVED]")
    print(f"  f:                {[round(float(value), 6) for value in entry.f[1:7]] if entry.f is not None else []} 1  [DERIVED]")
    print(f"  fmex:             {[round(float(value), 6) for value in entry.fmex[1:7]] if entry.fmex is not None else []} 1  [DERIVED]")
    print(f"  fw0:              {[round(float(value), 6) for value in entry.fw0[1:7]] if entry.fw0 is not None else []} lb/ft^2  [DERIVED]")
    print(f"  fsig:             {[round(float(value), 6) for value in entry.fsig[1:7]] if entry.fsig is not None else []} 1/ft  [DERIVED]")
    print(f"  w0_dead:          {entry.w0_dead:.6f} lb/ft^2  [DERIVED]")
    print(f"  w0_live:          {entry.w0_live:.6f} lb/ft^2  [DERIVED]")
    print(f"  wn_dead:          {entry.wn_dead:.6f} lb/ft^2  [DERIVED]")
    print(f"  wn_live:          {entry.wn_live:.6f} lb/ft^2  [DERIVED]")
    print(f"  sig_dead:         {entry.sig_dead:.6f} 1/ft  [DERIVED]")
    print(f"  sig_live:         {entry.sig_live:.6f} 1/ft  [DERIVED / assumption-based]")
    print(f"  sig_overall:      {entry.sig_overall:.6f} 1/ft  [DERIVED]")
    print(f"  f_dead:           {entry.f_dead:.6f} 1  [DERIVED]")
    print(f"  f_live:           {entry.f_live:.6f} 1  [DERIVED]")
    print(f"  beta:             {entry.beta:.6f} 1  [DERIVED]")
    print(f"  beta_op:          {entry.beta_op:.6f} 1  [DERIVED]")
    print(f"  rho_b:            {entry.rho_b:.6f} lb/ft^3  [DERIVED]")
    print(f"  xi:               {entry.xi:.6f} 1  [DERIVED]")
    print(f"  a_coeff:          {entry.a_coeff:.6f} 1  [DERIVED]")
    print(f"  b_coeff:          {entry.b_coeff:.6f} 1  [DERIVED]")
    print(f"  c_coeff:          {entry.c_coeff:.6f} 1  [DERIVED]")
    print(f"  e_coeff:          {entry.e_coeff:.6f} 1  [DERIVED]")
    print(f"  gamma_prime_peak: {entry.gamma_prime_peak:.6f} 1/min  [DERIVED]")
    print(f"  gamma_prime:      {entry.gamma_prime:.6f} 1/min  [DERIVED]")
    print(f"  tr:               {entry.tr:.6f} min  [DERIVED]")
    print(f"  gp_wnd_emd_es_hoc:{entry.gp_wnd_emd_es_hoc:.6f} Btu/(ft^2·min)  [DERIVED]")
    print(f"  gp_wnl_eml_es_hoc:{entry.gp_wnl_eml_es_hoc:.6f} Btu/(ft^2·min)  [DERIVED]")
    print(f"  phisterm:         {entry.phisterm:.6f} 1  [DERIVED]")
    print(f"  phiwterm:         {entry.phiwterm:.6f} 1  [DERIVED]")
    print(f"  unsheltered_waf:  {entry.unsheltered_waf:.6f} 1  [DERIVED]")


def calculate_rothermel_no_wind_no_slope(
    fuel_model: FuelModelParams,
    inputs: RothermelInput,
) -> RothermelOutput:
    """
    Calculate Rothermel rate of spread for no wind and no slope conditions.
    
    This implements the basic Rothermel fire spread equations as documented in:
    Rothermel, R. C. (1972). A mathematical model for predicting fire spread in
    wildland fuels. USDA Forest Service Research Paper INT-115, Intermountain
    Forest and Range Experiment Station, Ogden, UT.
    
    And the ELMFIRE FORTRAN implementation:
    https://github.com/lautenberger/elmfire/blob/main/build/source/elmfire_spread_rate.f90
    
    Basic Fire Spread Equation (equations 52, 27, and 42 from Rothermel 1972):
    
        R = I_R(1 + φ_w + φ_s) / (ρ_b * E_ig)  [ft/min]  (equation 52)
        
    For no wind and no slope conditions (φ_w = 0, φ_s = 0):
    
        R = I_R * ξ / (ρ_b * E_ig)  [ft/min]
    
    Where:
        I_R  = Γ' * w_n * h_n * η_M * η_s     Reaction intensity (eq. 27)
        ξ    = Propagating flux ratio (eq. 42)
        ρ_b  = w_o / δ                         Ovendry bulk density
        E_ig = ε * Q_ig                        Effective heating number (eq. 14)
        Γ'   = Optimum reaction velocity
        w_n  = Net fuel loading
        h_n  = Net heat of combustion
        η_M  = Moisture damping coefficient (eq. 29)
        η_s  = Mineral damping coefficient (eq. 30)
        ε    = Effective heating number exponent
        Q_ig = Heat of preignition
    
    Args:
        fuel_model: Fuel model parameters from CSV file
        inputs: Input conditions (moisture contents, mineral content, etc.)
        
    Returns:
        RothermelOutput with rate of spread (ft/min) and derived quantities
    """

    # Legacy implementation note:
    # The current equation path is kept in place so it can be compared against
    # the table-driven ELMFIRE-aligned version during the next iteration.
    # If this function is replaced later, keep a commented copy of the prior
    # logic here or immediately below this block for easy rollback.
    
    table = build_fuel_model_table_entry(fuel_model, inputs.mlh)

    # Constants
    S_T = 0.0555  # Total mineral content (lb. minerals / lb. ovendry wood)
    
    # === Fuel loading aggregation (lb/ft²) ===
    # From fuel model CSV: w_o values (equation 24 in Rothermel 1972)
    w_dead_1 = table.w0[1]
    w_dead_2 = table.w0[2]
    w_dead_3 = table.w0[3]
    w_dead_4 = table.w0[4]
    w_live_5 = table.w0[5]
    w_live_6 = table.w0[6]
    
    w_dead_total = w_dead_1 + w_dead_2 + w_dead_3 + w_dead_4
    w_live_total = w_live_5 + w_live_6
    w_total = w_dead_total + w_live_total
    
    # === Ovendry bulk density (lb/ft³) ===
    # ρ_b = w_o / δ  (equation 40 in Rothermel 1972)
    delta = table.delta  # Fuel bed depth (ft)
    rho_b = table.rho_b
    
    # === Packing ratio (dimensionless) ===
    # β = ρ_b / ρ_p  (equation 31 in Rothermel 1972)
    beta = table.beta
    
    # === Optimal packing ratio (equation 37) ===
    # β_op = 3.348 * σ^(-0.8189)
    # Using median σ value for calculation
    sigma_avg = table.sig_overall
    beta_op = table.beta_op
    
    # === Moisture content and moisture ratio ===
    # M = moisture content as a fraction in this Python port.
    # ELMFIRE interpolation enforces moisture floors before spread calculations.
    m_1 = max(inputs.m1, 1.0) / 100.0
    m_2 = max(inputs.m10, 1.0) / 100.0
    m_3 = max(inputs.m100, 1.0) / 100.0
    m_4 = m_1
    m_5 = max(inputs.mlh, 30.0) / 100.0
    m_6 = max(inputs.mlw, 60.0) / 100.0
    
    # === Calculate moisture of extinction (dimensionless) ===
    # ELMFIRE computes a fuel-specific value; keep the legacy default here as a
    # fallback reference.
    m_x = table.mex_dead
    
    # === Calculate moisture damping coefficient (η_M) ===
    # η_M = 1 - 2.59(M_f/M_x) + 5.11(M_f/M_x)² - 3.52(M_f/M_x)³  (equation 29)
    # Use weighted average of dead fuel moisture
    m_dead_avg = (w_dead_1 * m_1 + w_dead_2 * m_2 + 
                  w_dead_3 * m_3 + w_dead_4 * m_4) / max(w_dead_total, 1e-6)
    
    # For live fuels (if present)
    m_live_avg = (w_live_5 * m_5 + w_live_6 * m_6) / max(w_live_total, 1e-6) if w_live_total > 0 else 0
    
    # Overall moisture - use dead fuel as primary driver for ROS
    m_f = m_dead_avg
    m_ratio = m_f / m_x if m_x > 0 else 0
    m_ratio = min(m_ratio, 1.0)  # Clamp to [0,1] - don't exceed M_x
    
    # Moisture damping coefficient (equation 29)
    eta_m_dead = 1.0 - 2.59 * m_ratio + 5.11 * m_ratio**2 - 3.52 * m_ratio**3
    eta_m_dead = max(0.0, min(1.0, eta_m_dead))

    # ELMFIRE-style live moisture of extinction update
    m_array = np.array([0.0, m_1, m_2, m_3, m_4, m_5, m_6])
    wprime_numer = table.wprime_numer
    mprime_numer = wprime_numer * m_array
    sum_mprime_numer = float(np.sum(mprime_numer[1:5]))
    live_extinction = table.mex_live * (1.0 - table.r_mprime_denom_14_sum_mex_dead * sum_mprime_numer) - 0.226
    live_extinction = max(live_extinction, table.mex_dead)

    live_ratio = min((m_live_avg / live_extinction) if live_extinction > 0 else 0.0, 1.0)
    eta_m_live = 1.0 - 2.59 * live_ratio + 5.11 * live_ratio**2 - 3.52 * live_ratio**3
    eta_m_live = max(0.0, min(1.0, eta_m_live))
    eta_m = eta_m_dead
    
    # === Mineral damping coefficient (η_s) ===
    # η_s = 0.174 * S_e^(-0.19)  (equation 30)
    # Effective mineral content (silica-free minerals)
    s_e = inputs.s_e
    s_t = S_T
    
    eta_s = (0.174 / (table.se ** 0.19)) if table.se > 0 else 0.174
    eta_s = max(0.0, min(1.0, eta_s))
    
    # === Heat content ===
    # h_n = net heat of combustion (Btu/lb)
    h = table.hoc  # Btu/lb
    
    # === Weighted surface-area-to-volume ratios ===
    # Calculate weighted average σ for dead and live fuels
    sigma_dead = table.sig_dead
    sigma_live = table.sig_live
    sigma = table.sig_overall
    
    # === Reaction intensity (I_R) ===
    # Net fuel loading (lb/ft²) - equation 24
    w_n = w_total
    
    # Net heat of combustion (Btu/lb)
    h_n = h
    
    # Optimum reaction velocity (Γ'_max) - equation (36)
    # Γ'_max = 0.1 * σ^1.5 * (495 + 0.0594 * σ^1.5)^(-1)
    gamma_prime_max = table.gamma_prime_peak

    # Equation (39) - A coefficient
    a = table.a_coeff
    
    # Optimum reaction velocity at current packing ratio
    # Γ' = Γ'_max * (β/β_op)^A * exp[A(1 - β/β_op)]  (equation 38)
    packing_ratio_term = beta / max(beta_op, 1e-6)
    gamma_prime = gamma_prime_max * (packing_ratio_term ** a) * np.exp(a * (1.0 - packing_ratio_term))
    
    # Propagating flux ratio (ξ) - equation (42)
    # ξ = (192 + 0.2595*σ)^(-1) * exp[(0.792 + 0.681*σ^0.5)(β + 0.1)]
    zeta = table.xi
    
    # Reaction intensity (Btu/(ft²·min)) - equation 27
    # ELMFIRE keeps dead and live contributions separate.
    I_R_dead = table.gp_wnd_emd_es_hoc * eta_m_dead
    I_R_live = table.gp_wnl_eml_es_hoc * eta_m_live
    I_R = I_R_dead + I_R_live
    
    # === Effective heating number (E_ig) ===
    # ELMFIRE-style class-weighted denominator terms
    m_array = np.array([0.0, m_1, m_2, m_3, m_4, m_5, m_6])
    f_array = table.f
    eps_array = table.eps
    
    # Vectorized computation for qig
    qig = 250.0 + 1116.0 * m_array
    
    # Compute fepsqig for classes 1-6
    fepsqig = f_array * eps_array * qig

    rhob_eps_qig_dead = table.rho_b * float(np.sum(fepsqig[1:5]))
    rhob_eps_qig_live = table.rho_b * float(np.sum(fepsqig[5:7]))
    rho_b_eps_q_ig = table.f_dead * rhob_eps_qig_dead + table.f_live * rhob_eps_qig_live
    
    # === Rate of spread (no wind, no slope) ===
    # R = I_R * ξ / (ρ_b * E_ig)  [ft/min]  (equation 52 with φ_w = φ_s = 0)
    rate_of_spread = (I_R * zeta) / rho_b_eps_q_ig
    
    # === Heat per unit area (HPUA) ===
    # HPUA = I_R * τ  [Btu/ft²]
    # τ = residence time (minutes)
    # τ = 0.208 * exp(-0.05393 * σ)  (Albini and Randerson, 1975)
    tr = table.tr
    hpua = I_R * tr
    
    # === Flame length ===
    # Flame length (ft) = 0.45 * (I_R)^0.46  (Thomas et al. 1999)
    flame_length = 0.45 * (I_R ** 0.46)
    
    return RothermelOutput(
        rate_of_spread=rate_of_spread,
        reaction_intensity=I_R,
        flame_length=flame_length,
        hpua=hpua,
    )


def main():
    """Example usage of the Rothermel model."""
    
    # Path to fuel models CSV
    csv_path = Path(__file__).parent.parent / "data" / "inputs" / "fuel_models.csv"
    
    if not csv_path.exists():
        print(f"Error: Fuel models file not found at {csv_path}")
        return

    # Example fuel model used in this script output.
    test_fuel_id = 102

    # Read and print only the input row associated with test_fuel_id.
    fuel_df = pd.read_csv(csv_path, names=FUEL_MODEL_COLUMNS, header=None)
    fuel_row = fuel_df.loc[fuel_df["id"] == test_fuel_id]
    print(f"Fuel model table row for id={test_fuel_id}:")
    if fuel_row.empty:
        print("No matching row found in fuel model table.")
    else:
        print(fuel_row.head(1).to_string(index=False))
    print()
    
    # Load fuel models
    fuel_models = load_fuel_models(csv_path)
    print(f"Loaded {len(fuel_models)} fuel models\n")
    
    # Example 1: Compute ROS for fuel model 102 with default conditions
    if test_fuel_id in fuel_models:
        fuel_model = fuel_models[test_fuel_id]
        table_entry = build_fuel_model_table_entry(fuel_model)
        print(f"='* Fuel Model (ID {test_fuel_id})")
        print(f"Name: {fuel_model.name}")
        print(f"Total Dead Fuel: {fuel_model.w_dead_1h + fuel_model.w_dead_10h + fuel_model.w_dead_100h:.4f} lb/ft²")
        print(f"Total Live Fuel: {fuel_model.w_live_herb + fuel_model.w_live_woody:.4f} lb/ft²")
        print_fuel_model_table_entry(table_entry)
        print()

        # Use default moisture conditions
        default_m1 = 0.0
        default_m10 = 0.0
        default_m100 = 0.0

        print("Rate of Spread (No Wind, No Slope):")
        print(f"  Using fuel model id: {test_fuel_id}")
        print(f"  m1={default_m1}%, m10={default_m10}%, m100={default_m100}%")

        inputs = RothermelInput(
            fuel_model_id=test_fuel_id,
            m1=default_m1,
            m10=default_m10,
            m100=default_m100,
            s_e=0.01,
        )

        output = calculate_rothermel_no_wind_no_slope(fuel_model, inputs)
        print(f"  Rate of Spread:      {output.rate_of_spread:.4f} ft/min , {output.rate_of_spread * 0.00508:.4f} m/s")
        print(f"  Reaction Intensity:  {output.reaction_intensity:.2f} Btu/(ft²·min)")
    
    # Example 2: Compare multiple fuel models with standard conditions
    # Sample a few representative fuel models
    # sample_ids = [1, 2, 3, 4, 102, 161, 181]
    sample_ids = []
    
    if len(sample_ids) > 0:
        print("\n" + "=" * 80)
        print("Rate of Spread Comparison for Various Fuel Models")
        print("(No Wind, No Slope, Moisture: m1=5%, m10=7%, m100=9%)")
        print("=" * 80)
        print(f"{'ID':<6} {'Name':<15} {'Dead (lb/ft²)':<15} {'Live (lb/ft²)':<15} {'ROS (ft/min)':<15}")
        print("-" * 80)
        
        for fuel_id in sample_ids:
            if fuel_id in fuel_models:
                fuel = fuel_models[fuel_id]
                inputs = RothermelInput(fuel_id, m1=5, m10=7, m100=9)
                output = calculate_rothermel_no_wind_no_slope(fuel, inputs)
                
                w_dead = fuel.w_dead_1h + fuel.w_dead_10h + fuel.w_dead_100h
                w_live = fuel.w_live_herb + fuel.w_live_woody
                
                print(f"{fuel_id:<6} {fuel.name:<15} {w_dead:<15.4f} {w_live:<15.4f} {output.rate_of_spread:<15.4f}")


if __name__ == "__main__":
    main()
