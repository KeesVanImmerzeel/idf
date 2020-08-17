#' idf: Extending the 'raster' package in order to be able to read and write idf rasters.
#'
#' IDF is a simple binary format used by the iMOD groundwater modelling software.
#' The format contains a header with grid and spatial extent information and
#' an array of floats.The array of floats is translated to a rectangular grid using
#' the ncol and nrow fields of the header.
#'
#' @section idf functions:
#' This package extends the 'raster' package with two functions:
#' \code{\link{read_raster}}
#' \code{\link{write_raster}}
#'
#' @docType package
#' @name idf
NULL
