---
title: "Read a netCDF file with gdal"
slug: "read-a-netcdf-file-with-gdal"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Read a netCDF file (.nc) with python gdal
How to read a netCDF file (.nc) with python gdal ?

<!-- language: python -->
    import gdal

    # Path of netCDF file
    netcdf_fname = "/filepath/PREVIMER_WW3-GLOBAL-30MIN.nc"

    # Specify the layer name to read
    layer_name = "hs"

    # Open netcdf file.nc with gdal
    ds = gdal.Open("NETCDF:{0}:{1}".format(netcdf_name, layer_name))

    # Read full data from netcdf
    data = ds.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
    data[data < 0] = 0

[![netCDF display][1]][1]


  [1]: https://i.stack.imgur.com/zlrAS.png

