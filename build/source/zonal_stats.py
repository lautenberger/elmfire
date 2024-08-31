#!/usr/bin/env python3
import sys,os,csv
from rasterstats import zonal_stats

vectorFn = sys.argv[1]
rasterFn = sys.argv[2]
outFn = sys.argv[3]

#statistics = zonal_stats(vectorFn, rasterFn, stats=['min', 'max', 'median', 'mean', 'sum'])
#statistics = zonal_stats(vectorFn, rasterFn, stats=['mean'])

statistics = zonal_stats(vectorFn, rasterFn, stats=['min', 'max', 'median', 'mean'])
keys = statistics[0].keys()
with open(outFn, 'w') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
#    dict_writer.writeheader()
    dict_writer.writerows(statistics)
