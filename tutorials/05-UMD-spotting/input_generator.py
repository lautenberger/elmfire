def raw_raster_generator(CELLSIZE):
    # Create a template rasters
    from osgeo import gdal,osr
    import numpy as np
    import os

    OUTPUT_PATH = './inputs'
    if(not os.path.isdir(OUTPUT_PATH)):
        os.mkdir(OUTPUT_PATH)

    X_lowerleft = 0
    Y_lowerleft = 0

    DX = CELLSIZE # meters
    DY = CELLSIZE # meters

    X_dimension = 3000 # meters
    Y_dimension = 5000 # meters

    FLOAT_RASTER=['ws','wd','m1','m10','m100','adj','phi']
    FLOAT_VAL=[15.0,0,0,0,0,1,1]

    INT_RASTER=['slp','asp','dem','fbfm40','cc','ch','cbh','cbd']
    INT_VAL=[0,0,0,102,0,0,0,0]

    srs = osr.SpatialReference()
    srs.ImportFromEPSG(32610)

    # Process

    X_topleft = X_lowerleft + X_dimension
    Y_topleft = Y_lowerleft + Y_dimension

    shape = [int(Y_dimension/DY), int(X_dimension/DX)]
    
#     print("Raster size: {}\n".format(shape))
    
    empty_float_array = np.empty(shape, dtype = float, order = 'C')
    empty_int_array = np.empty(shape, dtype = int, order = 'C')

    driver = gdal.GetDriverByName("GTiff")
    driver.Register()

    for i in range(len(FLOAT_RASTER)):
        TEMP_ARRAY = empty_float_array
        TEMP_ARRAY[:][:] = FLOAT_VAL[i]
        outds = driver.Create(OUTPUT_PATH+"/{}.tif".format(FLOAT_RASTER[i]), xsize = shape[1],
                          ysize = shape[0], bands = 1,
                          eType = gdal.GDT_Float32)
        outds.SetGeoTransform([X_topleft, DX, 0.0, Y_topleft, 0.0, -DY])
        outds.SetProjection(srs.ExportToWkt())
        outband = outds.GetRasterBand(1)
        outband.WriteArray(TEMP_ARRAY)
        outband.SetNoDataValue(-9999)
        outband.FlushCache()

        # close your datasets and bands!!!
        outband = None
        outds = None

    for i in range(len(INT_RASTER)):
        TEMP_ARRAY = empty_int_array
        TEMP_ARRAY[:][:] = INT_VAL[i]
        outds = driver.Create(OUTPUT_PATH+"/{}.tif".format(INT_RASTER[i]), xsize = shape[1],
                          ysize = shape[0], bands = 1,
                          eType = gdal.GDT_Int16)
        outds.SetGeoTransform([X_topleft, DX, 0.0, Y_topleft, 0.0, -DY])
        outds.SetProjection(srs.ExportToWkt())
        outband = outds.GetRasterBand(1)
        outband.WriteArray(TEMP_ARRAY)
        outband.SetNoDataValue(-9999)
        outband.FlushCache()

        # close your datasets and bands!!!
        outband = None
        outds = None
        
def raster_manipulation(PATH):
    # Detail manipulation
    from osgeo import gdal
    import numpy as np
    
    directory=PATH.split('/')[0]+'/'+PATH.split('/')[1]
    filename =PATH.split('/')[2]
    # Read raster
    dataset = gdal.Open(PATH)
    gt = dataset.GetGeoTransform()
    proj = dataset.GetProjection()

    band = dataset.GetRasterBand(1)
    nodata_value = band.GetNoDataValue()
    datatype_value = band.DataType
    bandcount_value = dataset.RasterCount

    array = band.ReadAsArray()

    # Manipulate

    array_manipulated = array
    array_manipulated[2:4, 2:-2] = -1

    # Output raster
    driver = gdal.GetDriverByName("GTiff")
    driver.Register()
    
    outds = driver.Create(directory + "/new_{}".format(filename),
                          xsize = array_manipulated.shape[1], ysize = array_manipulated.shape[0],
                          bands = bandcount_value,
                          eType = band.DataType)
    outds.SetGeoTransform(gt)
    outds.SetProjection(proj)
    outband = outds.GetRasterBand(1)
    outband.WriteArray(array_manipulated)
    outband.SetNoDataValue(nodata_value)
    outband.FlushCache()

    # close your datasets and bands!!!
    outband = None
    outds = None

from osgeo import gdal
import sys

CELLSIZE_STR=sys.stdin.read()
CELLSIZE = float(CELLSIZE_STR)

raw_raster_generator(CELLSIZE)

PATH = './inputs/phi.tif'

raster_manipulation(PATH)
