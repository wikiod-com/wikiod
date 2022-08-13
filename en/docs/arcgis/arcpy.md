---
title: "ArcPy"
slug: "arcpy"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

ArcPy is a Python module that provides access to Esri's geographic data analysis, data conversion, and data management software.

While Esri software primarily uses C++ for its proprietary geoprocessing and mapping tools, the ArcPy library enables those tools to be executed as part of a Python script.

Organized in tools, functions, classes, and modules, ArcPy allows automation of Esri software products to build complex workflow

Unlike many Python modules, ArcPy is not freeware and requires a licensed version of ArcGIS Desktop or ArcGIS Pro to function properly.

## Importing ArcPy modules
ArcPy is composed of a number of modules:

* `arcpy` — the basic functions and geoprocessing tools
* `arcpy.mapping` — access to mapping and map document tools
* `arcpy.da` — a data access module for working with feature classes and tables
* `arcpy.sa` — the Spatial Analyst extension module
* `arcpy.na` — the Network Analyst extension module

*Note: Some of these modules require additional licensing from Esri.*

These can be imported all at once (`import arcpy`) or individually (`import arcpy.sa as sa`)

