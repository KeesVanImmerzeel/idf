# idf
<!-- badges: start -->
[![R-CMD-check](https://github.com/KeesVanImmerzeel/idf/workflows/R-CMD-check/badge.svg)](https://github.com/KeesVanImmerzeel/idf/actions)
<!-- badges: end -->

Extending the 'terra' package in order to be able to read and write idf rasters.

IDF is a simple binary format used by the iMOD groundwater modelling software.
The format contains a header with grid and spatial extent information and
an array of floats.The array of floats is translated to a rectangular grid using
the ncol and nrow fields of the header.

## Installation

You can install the released version of the idf-package with:

`install_github("KeesVanImmerzeel/idf")`

Then load the package with:

`library("idf")` 

## Functions in this package
- `read_raster()`: create a terra::SpatRaster object (single layer) from an idf-file.
- `write_raster()`: writes an terra::SpatRaster object (single layer) to a file, using one of the many supported formats or the terra package 
  or the idf file format as used in the imod modeling software.
- `create_funstr()`: Create a string ("funstr") to be used in functions read_raster() and write_raster().
- `funstr_ptrn()`: Create a string to be used as pattern in the function "create_funstr()".
- `idfname_to_date()`: Extract the date part of an idf filename with a date included, like in 'HEAD_20080402_l1.idf'.
- `filter_idfnames()`: Filter filenames of idf files with a date included, like "HEAD_20080402_l1.idf".
- `all_idf_filenames_contain_dates()`: FALSE if at least 1 filename specified does not contain a date; TRUE otherwise (all filenames contain a date).
- `create_statistic_raster()`: Summarize the values of multiple layers into one layer.

## Get help

To get help on the functions in this package type a question mark before the function name, like `?read_raster()`

## idf-format definition

![image](https://user-images.githubusercontent.com/16401251/155693136-579077c6-fd0c-4d18-8bf7-c68057f3692a.png)
![image](https://user-images.githubusercontent.com/16401251/155693361-57942b0b-b0ed-4b2b-93a2-311ceb3874d1.png)



