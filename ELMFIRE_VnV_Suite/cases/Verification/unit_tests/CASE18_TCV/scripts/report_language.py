"""Presentation vocabulary for reports and plots; never used to evaluate results.

Case-local copies keep independent report and figure generation possible.
Operational names remain in data records; only their displayed labels change.
"""
from __future__ import annotations

import re


TERMS = {
    'LIST_BURNED': 'burned-cell state',
    r'LIST\_BURNED': 'burned-cell state',
    'FTP_CRIT': 'critical material fire-thermal property',
    r'FTP\_CRIT': 'critical material fire-thermal property',
    'HRR_ELLIPSE_ADJ': 'heat-release ellipse scale factor',
    r'HRR\_ELLIPSE\_ADJ': 'heat-release ellipse scale factor',
    'wind_equal_35': '35 mph wind',
    r'wind\_equal\_35': '35 mph wind',
    'hand calculations': 'independent calculations',
    'hand-calculated': 'independently calculated',
    'metrics JSON': 'calculated metrics',
    'harness default': 'prescribed',
    'standalone wrapper renders': 'report states',
    'numerical artifact': 'discretization effect',
    'independent oracle': 'independent reference calculation',
    'no-propagation oracle': 'independent prediction of no propagation',
    'analytical oracle': 'analytical reference solution',
    'oracle': 'reference calculation',
    'test harness': 'verification procedure',
    'acceptance contract': 'acceptance criteria',
    'case contract': 'case specification',
    'predeclared contract': 'predeclared specification',
    'evaluation contract': 'evaluation definition',
    'implementation path': 'physical and numerical processes',
    'code path': 'numerical procedure',
    'runtime dependency': 'required software',
    'runtime': 'execution',
    'source tree': 'ELMFIRE implementation',
    'case-local': 'case-specific',
    'variant-local': 'simulation-specific',
    'workflow-selected': 'selected for this comparison',
    'workflow': 'analysis procedure',
    'pipeline': 'simulation and analysis procedure',
    'payload': 'source data',
    'artifacts': 'supporting results',
    'artifact': 'supporting result',
    'fixture': 'prescribed configuration',
    'schema': 'configuration definition',
    'registry': 'case index',
    'adapter': 'data conversion procedure',
    'machine-readable': 'recorded',
    'backend': 'numerical implementation',
    'mock': 'prescribed',
    'stub': 'incomplete calculation',
    'variants': 'simulation conditions',
    'variant': 'simulation condition',
    'manifest': 'simulation specification',
    'fingerprint': 'input identity record',
    'fingerprints': 'input identity records',
}

NAMES = {
    'metrics.json': 'calculated verification and validation metrics',
    'case.yaml': 'case specification', 'case.json': 'case specification',
    'manifest.json': 'prescribed simulation conditions',
    'expected.json': 'independently calculated expected responses',
    'input_statistics.json': 'input summary statistics',
    'geometry_summary.csv': 'structure geometry measurements',
    'parameter_design.csv': 'prescribed parameter combinations',
    'parametric_results.csv': 'parameter-sweep results',
    'source_manifest.json': 'input and observation provenance record',
    'archive_manifest.json': 'source-data integrity record',
    'source_selector_preflight.json': 'review of supported model settings',
    'run_fingerprints.json': 'input identity records for the completed simulations',
    'variant_ids.txt': 'list of prescribed simulation conditions',
    'source_audit.md': 'review of model compatibility and data provenance',
    'case_registry.md': 'ordered list of verification cases',
    'elmfire.data.in': 'ELMFIRE simulation configuration',
    'elmfire.data': 'ELMFIRE simulation configuration',
    'run_case.sh': 'simulation and analysis procedure',
    'compile_case.sh': 'report compilation procedure',
    'preprocess.py': 'input preparation procedure',
    'generate_inputs.py': 'input construction procedure',
    'postprocess.py': 'result analysis procedure',
    'rothermel_reference.py': 'independent Rothermel reference calculation',
    'metrics_macros.tex': 'reported metric values',
    'elmfire_level_set.f90': 'ELMFIRE level-set formulation',
    'elmfire_spread_rate.f90': 'ELMFIRE spread-rate calculation',
    'elmfire_io.f90': 'ELMFIRE output procedure',
    'elmfire_subs.f90': 'ELMFIRE initialization procedure',
    'elmfire.f90': 'ELMFIRE simulation procedure',
    'fire_size_stats.csv': 'fire-growth and containment statistics',
    'phi': 'level-set field', 'new_phi': 'initial level-set field',
    'vs': 'spread rate', 'flin': 'fireline intensity',
    'fbfm': 'fuel-model classification', 'fbfm40': 'fuel-model classification',
    'new_fbfm40': 'fuel-model classification', 'adj': 'spread-rate adjustment factor',
    'ws': 'wind speed', 'wd': 'wind direction', 'dem': 'elevation',
    'slp': 'terrain slope', 'asp': 'terrain aspect',
    'cc': 'canopy cover', 'ch': 'canopy height',
    'cbh': 'canopy base height', 'cbd': 'canopy bulk density',
    'm1': '1-h dead-fuel moisture', 'm10': '10-h dead-fuel moisture',
    'm100': '100-h dead-fuel moisture',
    'ember_flux': 'accumulated firebrand deposition',
    'ember_flux_transient': 'time-resolved firebrand deposition',
    'time_of_arrival': 'fire arrival time', 'ember_toa': 'firebrand arrival time',
    'hf_dfc_transient': 'time-resolved convective heat flux',
    'hf_rad_transient': 'time-resolved radiative heat flux',
    'hrr_transient': 'time-resolved heat-release rate',
    'dump_times': 'recorded output times',
    'transport_impulse': 'isolated transport',
    'wildland_no_delay': 'wildland ignition without delay',
    'surface_dominated': 'surface-fire-dominated transport',
    'mixed_mode': 'combined surface-fire and firebrand transport',
    'insufficient_output': 'insufficient simulation results',
    'inputs-orientation': 'orientation of the prescribed input fields',
    'numpy.rot90': 'a rotation through a multiple of 90 degrees',
    'completion_receipt': 'record of simulation completion',
    'selected_files': 'selected simulation results',
    'source_files': 'reviewed ELMFIRE implementation',
    'reviewed_file_sha256': 'identity of the reviewed ELMFIRE implementation',
}


def scientific_name(value: str) -> str:
    """Translate a known operational label, retaining numerical selections."""
    clean = value.replace(r'\_', '_').replace(r'\allowbreak ', '').strip()
    key = clean.lower()
    if key in NAMES:
        return NAMES[key]
    leaf = clean.split('/')[-1]
    if leaf.lower() in NAMES:
        return NAMES[leaf.lower()]
    for stem in sorted(NAMES, key=len, reverse=True):
        if leaf.lower().startswith(stem + '_') or leaf.lower() == stem + '.tif':
            if re.search(r'\.(?:tif|csv)$', leaf, re.I) or '*' in leaf:
                return NAMES[stem]
    if key in ('data/archive_payload/', 'variants/'):
        return 'the supplied input data' if key.startswith('data') else 'the prescribed simulation conditions'
    if key == 'figures/*.pdf':
        return 'input and result figures'
    # These labels encode physical grid spacing and timestep, not result values.
    if re.fullmatch(r'dx\d+(?:p\d+)?(?:_cfl\d+p\d+)?', key):
        parts = key.split('_')
        result = 'a ' + parts[0][2:].replace('p', '.') + ' m grid'
        if len(parts) == 2:
            result += ' and wind Courant number ' + parts[1][3:].replace('p', '.')
        return result
    if re.fullmatch(r'dt\d+p\d+', key):
        return 'a timestep of ' + key[2:].replace('p', '.') + ' s'
    return value


def scientific_text(value: object) -> str:
    """Translate displayed wording without modifying source metrics or keys."""
    text = scientific_name(str(value))
    # Display a temporal-resolution condition by its Courant number; the
    # accompanying table retains the exact generated timestep and stop time.
    text = re.sub(r'\bcfl(\d+(?:p\d+)?)(?:\\_|_)dt\d+p\d+\b',
                  lambda m: 'Wind Courant number ' + m[1].replace('p', '.'), text)
    for old, new in sorted(TERMS.items(), key=lambda item: -len(item[0])):
        text = re.sub(r'(?<![\w\\])' + re.escape(old) + r'(?!\w)',
                      lambda m: new[0].upper() + new[1:] if m[0][0].isupper() else new,
                      text, flags=re.I)
    return text


def report_text(text: str) -> str:
    """Format visible TeX prose while preserving equations and resource names.

    Scientific citations retain their published wording. Resource arguments,
    labels, macro keys and mathematical expressions remain literal.
    """
    # Match the guide's existing normalization for an overall missing-evidence
    # decision. Execution-state fields remain unchanged.
    text = text.replace(r'\def\OverallStatus{NOT RUN}',
                        r'\def\OverallStatus{NOT EVALUABLE}')
    # Normalize typography of a declared status, not its scientific meaning.
    text = re.sub(
        r'(metric@status\\endcsname\s*\{)(pass|fail)(\})',
        lambda m: m[1] + m[2].upper() + m[3], text, flags=re.I)
    protected = []
    def protect(match):
        protected.append(match[0])
        return f'@@PROTECTED{len(protected)-1}@@'
    pattern = (r'(?m)^%[^\n]*|\\begin\{thebibliography\}[\s\S]*?\\end\{thebibliography\}'
               r'|\\begin\{(?:equation\*?|align\*?|gather\*?)\}[\s\S]*?\\end\{(?:equation\*?|align\*?|gather\*?)\}'
               r'|\\\[[\s\S]*?\\\]|\\\([\s\S]*?\\\)|(?<!\\)\$[^$]*?(?<!\\)\$'
               r'|\\csname[^\\]*\\endcsname'
               r'|\\(?:input|include|includegraphics|EvidenceFigure|IfFileExists|label|ref|eqref|cite|Metric|InputMetric|DefineMetric|DefineInputMetric)\*?(?:\[[^\]]*\])?\{[^{}]*\}')
    text = re.sub(pattern, protect, text)
    text = re.sub(r'\\(?:texttt|path)\{([^{}]+)\}',
                  lambda m: scientific_name(m[1]) if scientific_name(m[1]) != m[1] else m[0], text)
    text = scientific_text(text)
    for index, original in enumerate(protected):
        text = text.replace(f'@@PROTECTED{index}@@', original)
    return text


def polish_figure(figure) -> None:
    """Edit Matplotlib text artists only; preserve all plotted data and limits."""
    from matplotlib.text import Text
    if hasattr(figure, 'gcf'):
        figure = figure.gcf()
    for artist in figure.findobj(match=Text):
        before = artist.get_text()
        after = scientific_text(before)
        if after != before:
            artist.set_text(after)
    # Matplotlib table cells do not expose their text through findobj on all
    # supported versions. Visit those labels explicitly without touching data.
    for axis in figure.axes:
        for table in axis.tables:
            for cell in table.get_celld().values():
                label = cell.get_text()
                label.set_text(scientific_text(label.get_text()))
