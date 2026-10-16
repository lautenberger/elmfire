import sys, pathlib, re
path = pathlib.Path(sys.argv[1])
target = sys.argv[2]
text = path.read_text()
pattern = re.compile(r"(PATH_TO_GDAL\s*=\s*)'[^']*'")
updated, count = pattern.subn(lambda m: f"{m.group(1)}'{target}'", text)
if not count:
    raise SystemExit(f"PATH_TO_GDAL line not found in {path}")
path.write_text(updated)