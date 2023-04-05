#!/usr/bin/env python3

import sys,os

oldpaths = sys.path
sys.path.extend(oldpaths)

from osgeo import ogr

inputfn = sys.argv[1]
outputBufferfn = sys.argv[2]
bufferDist = float(sys.argv[3])

inputds = ogr.Open(inputfn)
inputlyr = inputds.GetLayer()

shpdriver = ogr.GetDriverByName('ESRI Shapefile')
if os.path.exists(outputBufferfn):
    shpdriver.DeleteDataSource(outputBufferfn)
outputBufferds = shpdriver.CreateDataSource(outputBufferfn)
bufferlyr = outputBufferds.CreateLayer(outputBufferfn, geom_type=ogr.wkbPolygon)
featureDefn = bufferlyr.GetLayerDefn()

i = 1
for feature in inputlyr:
    i += 1
    ingeom = feature.GetGeometryRef()
    geomBuffer = ingeom.Buffer(bufferDist)
    outFeature = ogr.Feature(featureDefn)
    outFeature.SetGeometry(geomBuffer)
    bufferlyr.CreateFeature(outFeature)


