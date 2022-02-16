# In development: to install package to the default library path (.libPaths()): devtools::install()
# To create a binary package file: devtools::build(binary=TRUE). This creates a single zip-file.

# ----------------------------------------------------------------------------
#' Methods to create a RasterLayer object with the ability to read idf files.
#'
#' The function 'read_raster' is able to create a RasterLayer object from an
#' idf-file if an idf-filename is specified (exctension '.idf', like in 'foo.idf').
#' Otherwhise the function equals the function 'raster::raster()'.
#' In order to use the raster being read in your script, load the raster library first
#' i.e. library(raster)
#'
#' @param x (character) filename
#' @param crs (CRS) Coordinate Reference System. Optional.
#'    If an idf-file is specified and this argument is missing,
#'    the sp::CRS("+init=epsg:28992") is used.
#'    For other raster filetypes: the CRS specified is used;
#'       if it is not specified, the CRS is read from file (if existant).
#' @param ... Additional arguments as for 'raster' function.
#'    For an idf-raster, only the optional 'crs' argument is used.
#' @return RasterLayer
#'    N.B.: For your own files, omit the 'system.file' and 'package="raster"' bits
#'    these are just to get the path to files installed with the package.
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
read_raster <- function(x, crs=NULL, ...) {
   if ((typeof(x) == "character") &
       (.is_idf_extension(fnamer::get_filename_extension(x)))) {
      if (missing(crs)) {
         crs <- sp::CRS("+init=epsg:28992")
      }
      reslt <- .read.idf(x, crs)
   } else {
      reslt <- suppressWarnings(raster::raster(x, ...))
      if (!missing(crs)) {  # Force crs if explicitly specified
         raster::crs(reslt) <- sp::CRS("+init=epsg:28992")
      }
   }
   return(reslt)
}

# ----------------------------------------------------------------------------

#' Write raster data to a file.
#'
#' \code{write_raster} writes an entire Raster* object to a file, using one of
#' the many supported formats or the \code{\link[raster]{raster-package}} or the
#' idf file format as used in the imod modeling software
#' (\url{https://oss.deltares.nl/web/imod}).
#'
#' To use the 'idf' output format, don't specify the 'format argument' but use
#' the'.idf' extension in the filename. Only RasterLayer objects can be written
#' to an idf-file (no RasterStack or RasterBrick). Of the additional arguments
#' of \code{\link[raster]{writeRaster}}, only the 'overwrite' may be used in
#' case the idf-format is specified. In that case, all other additional
#' arguments of the function \code{write_raster} are ignored.
#' @param x RasterLayer Raster* object
#' @param filename Output filename
#' @param format Character. Output file type. See
#'   \code{\link[raster]{writeFormats}}. If this argument is not provided, it is
#'   attempted to infer it from the filename extension. If that fails, the
#'   default format is used. The default format is 'raster', but this can be
#'   changed using \code{\link[raster]{rasterOptions}}.
#' @param ... Additional arguments as in \code{\link[raster]{writeRaster}}.
#' @return This function is used for the side-effect of writing values to a file.
#'
#' @export
write_raster <- function(x, filename, format, ...) {
   # Write raster layer to idf file.
   #

   # @param x RasterLayer Raster* object
   # @param filename Output (idf) filename
   # @param overwrite (boolean) if true overwrite possible existing idf file.
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
         ncols <- x@ncols
         nrows <- x@nrows
         xll <- x@extent[1]
         xur <- x@extent[2]
         xlr <- x@extent[3]
         yur <- x@extent[4]
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
               x@data@min,
               x@data@max,
               x@file@nodatavalue
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
         writeBin(x[], con, size = 4, endian = "little")
         close(con)
      } else {
         stop("Error in .write.idf: file exists; use overwrite=TRUE")
      }
   }


   if ((missing(format)) &
       (.is_idf_extension(fnamer::get_filename_extension(filename))) &
       class(x) == "RasterLayer") {
      .write.idf(x, filename, ...)
   } else {
      if (!missing(format)) {
         suppressWarnings(raster::writeRaster(x, filename, prj=TRUE, ...))
      } else {
         suppressWarnings(raster::writeRaster(x, filename, format, prj=TRUE, ...))
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

# All possible output types of the 'raster::writeRaster' function.
# @return (character) output types that can be specified for the 'raster::writeRaster' function.
.writeFormats <- function() {
   x <- raster::writeFormats()
   return(x[,1])
}

# ----------------------------------------------------------------------------
# Read raster layer from idf file.
#
# @param x Name of idf-file (*.idf)
# @param crs PROJ.4 string to set the CRS. Default is epsg projection 28992 - amersfoort.
# @return RasterLayer
#    N.B.: For your own files, omit the 'system.file' and 'package="raster"' bits
#    these are just to get the path to files installed with the package.
# @examples
#
# f <- system.file("extdata", "test.idf", package="idf")
# f
# r <- .read.idf(f)
#
.read.idf <- function(x, crs=sp::CRS("+init=epsg:28992")) {
      con = file(x, "rb")
      vars = readBin(con,
                     integer(),
                     n = 3,
                     size = 4,
                     endian = "little")
      lrl_id = vars[1] # Lahey Record Length Identification; 1271 is a single precision IDF, 2296 a double precision.
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
            raster::raster(
                  nrows = nrow,
                  ncols = ncol,
                  xmn = xll,
                  xmx = xur,
                  ymn = yll,
                  ymx = yur,
                  crs = crs
            )
      raster::NAvalue(layer) <- nodata
      layer[] <- data

      names(layer) <- fnamer::bare_filename(x)

      return(layer)
}

# ----------------------------------------------------------------------------


