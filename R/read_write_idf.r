# To do 2022-2-21:
# 1. add reading/writing double precision idf

# ----------------------------------------------------------------------------
#' Methods to create a terra::SpatRaster object with the ability to read idf files.

#' The function 'read_raster' is able to create a RasterLayer object from an
#' idf-file if an idf-filename is specified (exctension '.idf', like in 'foo.idf').
#' Otherwhise the function equals the function 'raster::raster()'.
#' In order to use the raster being read in your script, load the raster library first
#' i.e. library(raster)
#'
#' @param x (character) filename
#' @param EPSG Four-five digit number which represents a particular CRS definition.
#'    If an idf-file is specified and this argument is missing, EPSG=28992 is used.
#' @param ... Additional arguments as for 'terra::rast' function.
#'    For an idf-raster, only the optional 'EPSG' argument is used.
#' @return terra::SpatRaster
#' @examples
#'
#' f <- system.file("extdata", "test.idf", package="idf")
#' f
#' r <- read_raster(f)
#'
#' f <- system.file("extdata", "test.tif", package="idf")
#' f
#' r <- read_raster(f)
#'
#' @export
read_raster <- function(x, EPSG=NULL, ...) {
   if ((typeof(x) == "character") &
       (.is_idf_extension(fileutils::get_filename_extension(x)))) {
      if (missing(EPSG)) {
         EPSG=28992
      }
      reslt <- .read.idf(x, EPSG)
   } else {
      reslt <- suppressWarnings(terra::rast(x, ...))
      if (!missing(EPSG)) {  # Force crs if explicitly specified
         terra::crs(reslt) <- 28992
      }
   }
   return(reslt)
}

# ----------------------------------------------------------------------------

#' Write raster data to a file.
#'
#' \code{write_raster} writes an entire terra::SpatRaster object to a file, using one of
#' the many supported formats in the 'terra' package or an idf file format as used in the imod modeling software
#' (\url{https://oss.deltares.nl/web/imod}).
#'
#' To use the 'idf' output format, don't specify the 'format argument' but use
#' the'.idf' extension in the filename. Only terra::SpatRaster objects with depth=0 can be written
#' to an idf-file. Of the additional arguments
#' of \code{\link[terra]{writeRaster}}, only the 'overwrite' may be used in
#' case the idf-format is specified. In that case, all other additional
#' arguments of the function \code{write_raster} are ignored.
#' @param x terra::SpatRaster object
#' @param filename Output filename
#' @param filetype Character. File format expresses as GDAL driver names.
#'   If this argument is not provided, it is attempted to infer it from the filename extension.
#' @param e Extent object
#' @param constant multiplication factor (numeric)
#' @param ... Additional arguments as in 'terra::writeRaster'.
#' @return This function is used for the side-effect of writing values to a file.
#' @importFrom magrittr %<>%
#' @export
write_raster <- function(x, filename, filetype=NULL, e=NULL, constant=NULL, ...) {
   # Write raster layer to idf file.
   # @param x  terra::SpatRaster object
   # @param filename Output filename
   # @param overwrite (boolean) if true overwrite possible existing file.
   # @examples
   #
   # f <- system.file("extdata", "test.tif", package="idf")
   # f
   # r <- idf::read_raster(f)
   #
   # .write.idf(r, "test.idf", overwrite = TRUE)
   #

   .write.idf <- function(x, filename, overwrite = FALSE) {
      if ((overwrite == TRUE) |
          (overwrite == FALSE & (!file.exists(filename)))) {
         con = file(filename, "wb")
         ncols <- terra::ncol(x)
         nrows <- terra::nrow(x)
         xll <- terra::xmin(x)
         xur <- terra::xmax(x)
         xlr <- terra::ymin(x)
         yur <- terra::ymax(x)
         mindata <- terra::minmax(x)[1]
         maxdata <- terra::minmax(x)[2]
         NAflg <- terra::NAflag(x)
         writeBin(as.integer(c(1271, ncols, nrows)),
                  con,
                  size = 4,
                  endian = "little")
         writeBin(
            c(
               xll,
               xur,
               xlr,
               yur,
               mindata,
               maxdata,
               NAflg
            ),
            con,
            size = 4,
            endian = "little"
         )
         writeBin(as.integer(0),
                  con,
                  size = 4,
                  endian = "little")
         dx <- (xur - xll) / ncols
         dy <- (yur - xlr) / nrows
         writeBin(c(dx, dy), con, size = 4, endian = "little")
         writeBin(as.vector(terra::values(x)), con, size = 4, endian = "little")
         close(con)
      } else {
         stop("Error in .write.idf: file exists; use overwrite=TRUE")
      }
   }

   if (!is.null(e)) {
      x %<>% terra::crop(e)
   }
   if (!is.null(constant)) {
      terra::values(x) <- terra::values(x) #* constant
   }
   if ((is.null(filetype)) &
       (.is_idf_extension(fileutils::get_filename_extension(filename))) &
       class(x) == "SpatRaster") {
      print(".write.idf")
      .write.idf(x, filename, ...)
   } else {
      if (is.null(filetype)) {
         print("write using file extension to determine file type")
         suppressWarnings(terra::writeRaster(x, filename, ...))
      } else {
         print("write using specified file extension ")
         suppressWarnings(terra::writeRaster(x, filename, filetype, ...))
      }
   }
}

# ----------------------------------------------------------------------------

# Test if the Specified Filename Extension is idf-Filename Extension
#
# Returns TRUE or FALSE depending on the input filename extension.
#
# @param ext (character) Filename extension
# @return (logical) TRUE if 'ext' is a valid idf-filename extension; FALSE otherwhise.
# @examples
#
# .is_idf_extension(".idf")
# .is_idf_extension(".txt")
.is_idf_extension <- function( ext ) {
   toupper(ext) == ".IDF"
}


# ----------------------------------------------------------------------------
# Read raster layer from idf file.
#
# @param x Name of idf-file (*.idf)
# @param crs EPSG Four-five digit number which represents a particular CRS definition.
# @return terra::SpatRaster object
# @examples
#
# f <- system.file("extdata", "test.idf", package="idf")
# f
# r <- .read.idf(f)
#
.read.idf <- function(x, EPSG=28992) {
   con = file(x, "rb")
   vars = readBin(con,
                  integer(),
                  n = 3,
                  size = 4,
                  endian = "little")
   ncol = vars[2]
   nrow = vars[3]
   vars = readBin(con,
                  double(),
                  n = 7,
                  size = 4,
                  endian = "little")
   xll = vars[1]
   xur = vars[2]
   yll = vars[3]
   yur = vars[4]
   minval = vars[5]
   maxval = vars[6]
   nodata = vars[7]
   vars = readBin(con,
                  logical(),
                  n = 4,
                  size = 1,
                  endian = "little")
   ieq = vars[1]
   itb = vars[2]
   if (ieq |
       itb)
      stop("Non-equidistant IDF's or IDF's with tops and bottoms not supported yet...")
   vars = readBin(con,
                  double(),
                  n = 2,
                  size = 4,
                  endian = "little")
   dx = vars[1]
   dy = vars[2]
   data = readBin(con,
                  double(),
                  n = ncol * nrow,
                  size = 4,
                  endian = "little")
   data = matrix(data,
                 nrow = nrow,
                 ncol = ncol,
                 byrow = TRUE)
   close(con)

   layer <-
      terra::rast(
         nrows = nrow,
         ncols = ncol,
         xmin = xll,
         xmax = xur,
         ymin = yll,
         ymax = yur,
         vals=data
      )
   terra::NAflag(layer)<-nodata

   names(layer) <- fileutils::bare_filename(x)

   return(layer)
}

# ----------------------------------------------------------------------------


