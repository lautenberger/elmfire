#!/usr/bin/env python3
"""Plot Monte Carlo coefficient histograms against their analytical PDFs.

The file outputs/coeffs.csv stores the *unscaled* perturbation coefficients that
ELMFIRE draws per case (see elmfire_subs.f90: X_actual = X_input + COEFF).

Distributions (from monte_carlo.data &MONTE_CARLO):
  WS : UNIFORM,   lower=-5, upper=5
  WD : GAUSSIAN,  mean=0,   sigma=20
  M1 : LOGNORMAL, mean=1.0, sigma=0.3   -> stored coeff = LN - mean  (shifted)

For the lognormal, ELMFIRE matches the *arithmetic* mean/sigma of the lognormal
variable LN, then subtracts the mean so the perturbation is centered near 0:
  sigma_n^2 = ln(1 + (sigma/mean)^2)
  mu_n      = ln(mean) - 0.5*sigma_n^2
  coeff     = exp(mu_n + sigma_n*Z) - mean
"""

import os
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

CSV = os.path.join(os.path.dirname(__file__), "outputs", "coeffs.csv")

# --- load -------------------------------------------------------------------
data = np.genfromtxt(CSV, delimiter=",", skip_header=1, usecols=(1, 2, 3))
ws, wd, m1 = data[:, 0], data[:, 1], data[:, 2]

# --- analytical pdfs --------------------------------------------------------
def uniform_pdf(x, lo, hi):
    return stats.uniform(loc=lo, scale=hi - lo).pdf(x)

def gaussian_pdf(x, mean, sigma):
    return stats.norm(loc=mean, scale=sigma).pdf(x)

def shifted_lognormal_pdf(x, mean, sigma):
    # pdf of (LN - mean) where LN is lognormal with arithmetic mean & sigma
    sigma_n2 = np.log(1.0 + (sigma / mean) ** 2)
    sigma_n = np.sqrt(sigma_n2)
    mu_n = np.log(mean) - 0.5 * sigma_n2
    # scipy lognorm: shape=s=sigma_n, scale=exp(mu_n); shift support by -mean
    return stats.lognorm(s=sigma_n, scale=np.exp(mu_n)).pdf(x + mean)

# --- plot -------------------------------------------------------------------
fig, axes = plt.subplots(1, 3, figsize=(16, 5))

panels = [
    ("WS  (Uniform, -5 to 5)", ws, lambda x: uniform_pdf(x, -5.0, 5.0)),
    ("WD  (Gaussian, mean=0, sd=20)", wd, lambda x: gaussian_pdf(x, 0.0, 20.0)),
    ("M1  (Lognormal, mean=1.0, sd=0.3)", m1, lambda x: shifted_lognormal_pdf(x, 1.0, 0.3)),
]

for ax, (title, samples, pdf) in zip(axes, panels):
    ax.hist(samples, bins=30, density=True, alpha=0.6, color="steelblue",
            edgecolor="white", label="sampled (coeffs.csv)")
    xs = np.linspace(samples.min(), samples.max(), 400)
    ax.plot(xs, pdf(xs), "r-", lw=2, label="analytical pdf")
    ax.set_title(title)
    ax.set_xlabel("perturbation coefficient")
    ax.set_ylabel("density")
    ax.legend()

fig.suptitle(f"Monte Carlo perturbation coefficients  (N={len(ws)})", fontsize=13)
fig.tight_layout(rect=[0, 0, 1, 0.96])

out = os.path.join(os.path.dirname(__file__), "outputs", "coeffs_histograms.png")
fig.savefig(out, dpi=130)
print(f"Saved {out}")
plt.show()
