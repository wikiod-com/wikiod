---
title: "Reading rasters with gdal"
slug: "reading-rasters-with-gdal"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Read subset of a global raster defined by a bounding box
Open a raster that covers the globe and extract a subset of the raster.

<!-- language-all: python -->
    import gdal

    # Path to a tiff file covering the globe
    # http://visibleearth.nasa.gov/view.php?id=57752
    tif_name = "/path_name/land_shallow_topo_21600.tif"
    
    # Open raster in read only mode
    ds = gdal.Open(tif_name, gdal.GA_ReadOnly)

    # Get the first raster band
    band = ds.GetRasterBand(1)

    # Compute x/y resolution in degrees
    resx = 360. / band.XSize
    resy = 180. / band.YSize

    # Define the geotransform used to convert x/y pixel to lon/lat degree
    # [lon_topleft, lon_resolution, lat_skew, lat_topleft, lon_skew, lat_resolution]
    geotransform = [-180, resx, 0.0,  90, 0.0, -1*resy]

    # The inverse geotransform is used to convert lon/lat degrees to x/y pixel index
    inv_geotransform = gdal.InvGeoTransform(geotransform)

    # Define a longitude/latitude bounding box in degrees
    # [lonmin, latmin, lonmax, latmax]
    bbox = [-5, 40, 10, 55]

    # Convert lon/lat degrees to x/y pixel for the dataset
    _x0, _y0 = gdal.ApplyGeoTransform(inv_geotransform, bbox[0], bbox[1])
    _x1, _y1 = gdal.ApplyGeoTransform(inv_geotransform, bbox[2], bbox[3])
    x0, y0 = min(_x0, _x1), min(_y0, _y1)
    x1, y1 = max(_x0, _x1), max(_y0, _y1)

    # Get subset of the raster as a numpy array
    data = band.ReadAsArray(int(x0), int(y0), int(x1-x0), int(y1-y0))

[![Result display][1]][1]


  [1]: https://i.stack.imgur.com/aMXnB.jpg

