# ----------------------------------------------------------------------------
#' Methods to create a terra::SpatRaster object with the ability to read idf files.
#'
#' The function 'read_raster' is able to create a terra::SpatRaster object from an
#' idf-file if an idf-filename is specified (exctension '.idf', like in 'foo.idf').
#' Otherwhise the function equals the function 'terra::rast()'.
#'
#' @param x filename (character)
#' @param EPSG coordinate reference system like "EPSG:4326" (character)
#' @param ... Additional arguments as for 'terra::rast' function. For an idf-raster, only the optional 'EPSG' argument is used.
#' @return terra::SpatRaster
#' @examples
#' f <- system.file("extdata", "test.idf", package="idf")
#' r <- read_raster(f)
#'
#' f <- system.file("extdata", "test.tif", package="idf")
#' r <- read_raster(f)
#' @export
read_raster <- function(x, EPSG = "EPSG:28992", ...) {
   # Read binary with som defaults
   .read_bin <- function(con,
                         what = integer(),
                         n = 1L,
                         size = 4L) {
      readBin(con,
              what,
              n = n,
              size = size,
              endian = "little")
   }

   # ----------------------------------------------------------------------------
   # Read raster layer from idf file.
   #
   # @param x Name of idf-file (character)
   # @return terra::SpatRaster object (single layer)
   .read.idf <- function(x) {
      con <- file(x, "rb")

      size <- .read_bin(con) %>% .get_size()
      double_precision <-  size > 4

      # Header is fully doubled in size in case of double precision ...
      # This means integers are also turned into 8 bytes
      # and requires padding with some additional bytes
      if (double_precision) {
         void <- .read_bin(con)
      }
      ncol <- .read_bin(con)
      if (double_precision) {
         void <- .read_bin(con)
      }
      nrow <- .read_bin(con)
      if (double_precision) {
         void <- .read_bin(con)
      }
      vars <- .read_bin(con,
                       what = double(),
                       n = 7,
                       size = size)
      xll <- vars[1]
      xur <- vars[2]
      yll <- vars[3]
      yur <- vars[4]
      minval <- vars[5]
      maxval <- vars[6]
      nodata <- vars[7]
      vars <- .read_bin(con,
                       what = logical(),
                       n = 4,
                       size = 1)
      if (vars[1] | vars[2]) # ieq | itb
         stop("Non-equidistant IDF's or IDF's with tops and bottoms not supported.")
      if (double_precision) {
         void <- .read_bin(con)
      }
      vars <- .read_bin(con,
                        what = double(),
                        n = 2,
                        size = size)
      dx <- vars[1]
      dy <- vars[2]
      data <-
         .read_bin(con,
                   what = double(),
                   n = ncol * nrow,
                   size = size) %>%
         matrix(nrow = nrow,
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
            vals = data
         )
      terra::NAflag(layer) <- nodata
      names(layer) <- fileutils::bare_filename(x)
      return(layer)
   }

   if ((typeof(x) == "character") &
       (.is_idf_extension(fileutils::get_filename_extension(x)))) {
      reslt <- .read.idf(x)
   } else {
      reslt <- suppressWarnings(terra::rast(x, ...))
   }
   terra::crs(reslt) <- EPSG
   return(reslt)
}

# ----------------------------------------------------------------------------
#
#  Get size of floats (single or double precision IDF)
#
# @param Lahey Record Length Identification; 1271 is a single precision IDF,
#              2296 a double precision (integer)
# @return size of floats (integer)
.get_size <- function (lahey) {
      if ((lahey == 2295)|(lahey == 2296)) {
            return(8)
      } else {
            return(4)
      }
}

# ----------------------------------------------------------------------------

# Test if the Specified Filename Extension is idf-Filename Extension
# Returns TRUE or FALSE depending on the input filename extension.
#
# @param ext Filename extension (character)
# @return TRUE if 'ext' is a valid idf-filename extension; FALSE otherwhise (logical)
# @examples
# .is_idf_extension(".idf")
# .is_idf_extension(".txt")
.is_idf_extension <- function( ext ) {
      toupper(ext) == ".IDF"
}

# ----------------------------------------------------------------------------

#' Write raster data to a file.
#'
#' Writes a terra::SpatRaster object to a file, using one of
#' the supported formats in the 'terra' package.
#' or an idf file format (ref. \url{https://oss.deltares.nl/web/imod}).
#' In that case, use the'.idf' extension in the filename.
#' @param x terra::SpatRaster object
#' @param filename Output filename (character)
#' @param e Terra extent object
#' @param constant multiplication factor (numeric)
#' @param double_precision Write double precision idf. Only meaningfull for writing idf-files (logical)
#' @param ... Additional arguments as in 'terra::writeRaster'.
#' @return This function is used for the side-effect of writing values to a file.
#' @importFrom magrittr %<>%
#' @export
write_raster <- function(x,
                         filename,
                         e = NULL,
                         constant = NULL,
                         double_precision = FALSE,
                         ...) {
      .write.idf <-
            function(x,
                     filename,
                     double_precision = FALSE,
                     overwrite = FALSE) {
                  if ((overwrite == TRUE) |
                      (overwrite == FALSE &
                       (!file.exists(filename)))) {
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
                        if (double_precision) {
                              size <- 8
                              lahey <- 2295
                        } else {
                              size <- 4
                              lahey <- 1271
                        }
                        writeBin(as.integer(c(lahey, ncols, nrows)),
                                 con,
                                 size = 4,
                                 endian = "little")
                        writeBin(
                              c(xll,
                                xur,
                                xlr,
                                yur,
                                mindata,
                                maxdata,
                                NAflg),
                              con,
                              size = size,
                              endian = "little"
                        )
                        writeBin(as.integer(0),
                                 con,
                                 size = 4,
                                 endian = "little")
                        if (double_precision) {
                              writeBin(as.integer(0),
                                       con,
                                       size = 4,
                                       endian = "little")
                        }
                        dx <- (xur - xll) / ncols
                        dy <- (yur - xlr) / nrows
                        writeBin(c(dx, dy),
                                 con,
                                 size = size,
                                 endian = "little")
                        writeBin(
                              as.vector(terra::values(x)),
                              con,
                              size = size,
                              endian = "little"
                        )
                        close(con)
                  } else {
                        stop("Error in .write.idf: file exists; use overwrite=TRUE")
                  }
            }

      if (!is.null(e)) {
            x %<>% terra::crop(e)
      }
      if (!is.null(constant)) {
            terra::values(x) <- terra::values(x) * constant
      }
      if (.is_idf_extension(fileutils::get_filename_extension(filename))) {
            .write.idf(x, filename, double_precision, ...)
      } else {
            suppressWarnings(terra::writeRaster(x, filename, ...))
      }
}

# ----------------------------------------------------------------------------


