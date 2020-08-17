# idf

<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/KeesVanImmerzeel/idf?branch=master&svg=true)](https://ci.appveyor.com/project/KeesVanImmerzeel/idf)
<!-- badges: end -->

Extending the 'raster' package in order to be able to read and write idf rasters.

IDF is a simple binary format used by the iMOD groundwater modelling software.
The format contains a header with grid and spatial extent information and
an array of floats.The array of floats is translated to a rectangular grid using
the ncol and nrow fields of the header.

## Installation

You can install the released version of menyanthes from with:

`install_github("KeesVanImmerzeel/idf")`

Then load the package with:

`library("idf")` 

## Functions in this package
- `read_raster()`: create a RasterLayer object from an idf-file.
- `write_raster()`: writes an Raster object to a file, using one of the many supported formats or the R-raster-package 
  or the idf file format as used in the imod modeling software.

## Get help

To get help on the functions in this package type a question mark before the function name, like `?read_raster()`


