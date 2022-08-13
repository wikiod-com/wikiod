---
title: "Getting started with gdal"
slug: "getting-started-with-gdal"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation on Linux
GDAL is available in the default repositories of most popular Linux distributions and can be installed in the same way that packages in a Linux distribution are usually installed.

`apt-get install libgdal-dev`

`CPLUS_INCLUDE_PATH` and `C_INCLUDE_PATH` are necessary in order to include these corresponding libraries.

`export CPLUS_INCLUDE_PATH=/usr/include/gdal`

`export C_INCLUDE_PATH=/usr/include/gdal`

GDAL can also be installed with Python's package manager `pip`.

`xe pip install gdal`

