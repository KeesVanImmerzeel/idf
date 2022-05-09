#' idf: Extending the 'terra' package in order to be able to read and write idf rasters.
#'
#' IDF is a simple binary format used by the iMOD groundwater modelling software.
#' The format contains a header with grid and spatial extent information and
#' an array of floats.The array of floats is translated to a rectangular grid using
#' the ncol and nrow fields of the header.
#'
#' @section idf functions:
#' This package extends the 'terra' package with the following functions:
#'
#' \code{\link{read_raster}}
#'
#' \code{\link{write_raster}}
#'
#' \code{\link{create_funstr}}
#'
#' \code{\link{funstr_ptrn}}
#'
#' \code{\link{idfname_to_date}}
#'
#' \code{\link{filter_idfnames}}
#'
#' @docType package
#' @name idf
#'
#' @importFrom fileutils get_filename_extension
#' @importFrom fileutils bare_filename
#' @importFrom terra rast
#' @importFrom terra NAflag
#' @importFrom terra crs
#' @importFrom terra ncol
#' @importFrom terra nrow
#' @importFrom terra xmin
#' @importFrom terra xmax
#' @importFrom terra ymin
#' @importFrom terra ymax
#' @importFrom terra minmax
#' @importFrom terra values
#' @importFrom terra crop
#' @importFrom terra writeRaster
#' @importFrom terra `values<-`
#' @importFrom terra `time<-`
#' @importFrom lubridate "ymd"
#' @importFrom lubridate "month"
#'
NULL
