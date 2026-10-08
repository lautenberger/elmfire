import numpy as np

# ----------------------- WU-E Model Functions -----------------------
def hrr_transient(burning_time, early_time, developed_time, decay_time, hrr_peak):
    """Piecewise transient HRRPUA function (kW/m^2)."""
    if burning_time <= early_time:
        hrr = (hrr_peak / early_time) * burning_time
    elif burning_time <= developed_time:
        hrr = hrr_peak
    elif burning_time > decay_time:
        hrr = 0.0
    else:
        # Linear decay from developed_time -> decay_time down to 0 at decay_time
        hrr = (hrr_peak / (developed_time - decay_time)) * (burning_time - decay_time)
    return max(0.0, hrr)


def ellipse_ucb(ws20_now_mph, hamada_a, hamada_d, wind_prop):
    """Ellipse dimension regression based on wind speed & Hamada params.
    Returns: [ELLIPSE_MAJOR, ELLIPSE_MINOR, ELLIPSE_ECCENTRICITY, DIST_DOWNWIND] (meters)
    """
    V_MPS = ws20_now_mph * 0.447  # mph -> m/s

    if V_MPS < 10.0:  # HAZUS correction (low wind)
        D1 = 1.679463256 - 0.123901243*hamada_a + 0.307612446*hamada_d
        D2 = 78.62957398 + 1.536189561*hamada_a - 0.5662073*hamada_d

        S1 = -2.922896622 - 0.05550541*hamada_a + 0.017291361*hamada_d
        S2 = 39.31478699 + 0.768094781*hamada_a - 0.28310365*hamada_d

        U1 = -6.297892493 - 0.119654483*hamada_a + 0.037754535*hamada_d
        U2 = 78.62957398 + 1.536189561*hamada_a - 0.5662073*hamada_d

        dist_downwind = wind_prop * (D1*V_MPS + D2)
        dist_upwind   = wind_prop * (U1*V_MPS + U2)
        dist_sidewind = wind_prop * (S1*V_MPS + S2)

    elif V_MPS > 17.3:  # high wind
        D1 = -7.159031537 - 0.043555289*hamada_a - 0.14894238*hamada_d
        D2 = 394.4930697 + 0.720929023*hamada_a + 11.42149084*hamada_d

        S1 = -0.577270631 - 0.015285438*hamada_a + 0.012786629*hamada_d
        S2 = 38.11784939 + 0.800599307*hamada_a - 0.412476476*hamada_d

        U1 = -1.092711783 - 0.025390239*hamada_a + 0.016740663*hamada_d
        U2 = 52.39584604 + 1.104793131*hamada_a - 0.57241037*hamada_d

        dist_downwind = wind_prop * (D1*V_MPS + D2)
        dist_upwind   = wind_prop * (U1*V_MPS + U2)
        dist_sidewind = wind_prop * (S1*V_MPS + S2)

    else:  # mid wind (quadratic fits)
        D1 = 4.099488028 - 0.000767118*hamada_a + 0.134372426*hamada_d
        D2 = -94.26651508 - 0.000694022*hamada_a - 3.053034015*hamada_d
        D3 = 615.192675 + 0.300438559*hamada_a + 19.34120221*hamada_d

        S1 = 0.437844987 + 0.008280661*hamada_a - 0.002833081*hamada_d
        S2 = -10.13978982 - 0.192922421*hamada_a + 0.067862023*hamada_d
        S3 = 66.32382799 + 1.282260348*hamada_a - 0.484673257*hamada_d

        U1 = 0.525004045 + 0.01046073*hamada_a - 0.004473105*hamada_d
        U2 = -12.4091466 - 0.249326233*hamada_a + 0.109759448*hamada_d
        U3 = 84.64808209 + 1.727651884*hamada_a - 0.801945211*hamada_d

        dist_downwind = wind_prop * (D1*V_MPS**2 + D2*V_MPS + D3)
        dist_upwind   = wind_prop * (U1*V_MPS**2 + U2*V_MPS + U3)
        dist_sidewind = wind_prop * (S1*V_MPS**2 + S2*V_MPS + S3)

    ellipse_major = (dist_downwind + dist_upwind) / 2.0
    ellipse_ecc   = min(ellipse_major/2.0, ellipse_major - dist_upwind)

    EB2 = 1.0 - (ellipse_ecc / max(ellipse_major, 1e-12))**2
    if EB2 > 0.0:
        ellipse_minor = dist_sidewind / np.sqrt(EB2)
    else:
        ellipse_minor = 0.0

    return np.array([ellipse_major, ellipse_minor, ellipse_ecc, dist_downwind], dtype=float)


def heat_flux_calc(
    HRR_TRANSIENT,
    NONBURNABLE_FRAC,
    ABSORPTIVITY,
    RAD_DIST,
    ellipse_dimensions,
    hrr_ellipse_adj,
    idx, idy, ANALYSIS_CELLSIZE,
    WD20_NOW
):
    """Returns (DFC_HEAT_RECEIVED, RAD_HEAT_RECEIVED) in kW/m^2 for a target cell."""
    # Resolution derivatives
    RANALYSIS_CELLSIZE = 1.0 / ANALYSIS_CELLSIZE
    ANALYSIS_CELLSIZE_SQUARED = ANALYSIS_CELLSIZE * ANALYSIS_CELLSIZE
    HALF_ANALYSIS_CELLSIZE = 0.5 * ANALYSIS_CELLSIZE

    # Only burnable fraction receives conductive/convective (DFC) heat
    DFC_COEFF = 1.0 - NONBURNABLE_FRAC
    RAD_COEFF = ABSORPTIVITY

    # Ellipse geometry
    ELLIPSE_MAJOR, ELLIPSE_MINOR, ELLIPSE_ECCENTRICITY, DIST_DOWNWIND = ellipse_dimensions
    ELLIPSE_MINOR_SQUARED = ELLIPSE_MINOR**2

    # Relative target distance
    TARGET_R = np.hypot(idx, idy)
    TARGET_R_METERS = TARGET_R * ANALYSIS_CELLSIZE

    # Relative angle: ellipse major axis vs. vector to target
    TARGET_THETA = np.arctan2(idy, idx)
    WIND_THETA = np.deg2rad(270.0 - WD20_NOW)
    TARGET_THETA_F = TARGET_THETA - WIND_THETA

    # Distance from center to ellipse boundary along theta
    MAX_ELLIPSE_DIST = 0.3 * DIST_DOWNWIND * (ELLIPSE_MAJOR - ELLIPSE_ECCENTRICITY) / max(ELLIPSE_MINOR_SQUARED, 1e-12)
    denom = ELLIPSE_MAJOR - ELLIPSE_ECCENTRICITY * np.cos(TARGET_THETA_F)
    denom = denom if abs(denom) > 1e-12 else np.sign(denom) * 1e-12  # safety
    ELLIPSE_DIST_THETA = MAX_ELLIPSE_DIST * ELLIPSE_MINOR_SQUARED / denom

    # Fraction of target cell covered by ellipse
    DFC_CHECKER = RANALYSIS_CELLSIZE * (ELLIPSE_DIST_THETA + HALF_ANALYSIS_CELLSIZE - TARGET_R_METERS)
    DFC_FACTOR = np.clip(DFC_CHECKER, 0.0, 1.0)

    # Normalize heat flux so sum over ellipse area equals HRR
    HRR_ADJUSTER = ANALYSIS_CELLSIZE_SQUARED / (
        np.pi
        * (hrr_ellipse_adj * ELLIPSE_MAJOR)
        * (hrr_ellipse_adj * ELLIPSE_MINOR)
        + 1e-12
    )

    # DFC heat at target
    DFC_HEAT_RECEIVED = DFC_COEFF * DFC_FACTOR * HRR_TRANSIENT * HRR_ADJUSTER

    # Radiation: within RAD_DIST beyond ellipse
    RAD_LIMIT_THETA = ELLIPSE_DIST_THETA + RAD_DIST
    RAD_CHECKER = RANALYSIS_CELLSIZE * (RAD_LIMIT_THETA + HALF_ANALYSIS_CELLSIZE - TARGET_R_METERS)

    DELTA_RAD = np.clip(RAD_CHECKER, 0.0, 1.0)
    RAD_FACTOR = DELTA_RAD - DELTA_RAD * DFC_FACTOR

    if (DFC_FACTOR < 1.0) and (DFC_FACTOR > 0.0):
        RAD_EFF_DIST = ANALYSIS_CELLSIZE - DFC_FACTOR * ANALYSIS_CELLSIZE
    else:
        RAD_EFF_DIST = TARGET_R_METERS - ELLIPSE_DIST_THETA

    RAD_EFF_DIST = max(RAD_EFF_DIST, 1e-6)  # prevent divide-by-zero
    RAD_HEAT_RECEIVED = HRR_ADJUSTER * (0.3 * DFC_COEFF * RAD_COEFF * RAD_FACTOR * HRR_TRANSIENT * ANALYSIS_CELLSIZE_SQUARED) / (4.0 * np.pi * RAD_EFF_DIST * RAD_EFF_DIST)

    return DFC_HEAT_RECEIVED, RAD_HEAT_RECEIVED
