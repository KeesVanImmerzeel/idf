# ----------------------------------------------------------------------------
#' Methods to create a terra::SpatRaster object with the ability to read idf files.
#'
#' The function 'read_raster' is able to create a terra::SpatRaster object from an
#' idf-file if an idf-filename is specified (exctension '.idf', like in 'foo.idf').
#' Otherwhise the function equals the function 'terra::rast()'.
#'
#' @param x filename (character)
#' @param EPSG coordinate reference system like "EPSG:4326" (character)
#' @param e Terra extent object
#' @param funstr Optional function to modify raster values. Denote the raster with 'x' (or character between square brackets) (character)
#' @param ... Additional arguments as for 'terra::rast' function. For an idf-raster, only the optional 'EPSG' argument is used.
#' @return terra::SpatRaster
#' @examples
#' f <- system.file("extdata", "test.idf", package="idf")
#' r <- read_raster(f)
#'
#' f <- system.file("extdata", "test.tif", package="idf")
#' r <- read_raster(f)
#'
#' f <- system.file("extdata", "heads.tif", package="idf")
#' r <- read_raster(f)
#' names(r)
#'
#' f1 <- system.file("extdata", "HEAD_20080401_l1.idf", package="idf")
#' f2 <- system.file("extdata", "HEAD_20080501_l1.idf", package="idf")
#' f <- c(f1, f2)
#' r <- read_raster(f)
#'
#' r <- read_raster(f, funstr="x*100")
#' @export
read_raster <- function(x, EPSG = "EPSG:28992", e=NULL, funstr=NULL, ...) {
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
         #print("double precision")
      } else {
         #print("single precision")
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

      e <- terra::ext(xll, xur, yll, yur)
      d <- suppressWarnings(idf::idfname_to_date(x))
      if (is.na(d)) {
            d <- as.Date("1900-01-01")
            nm <- fileutils::bare_filename(x)
      } else {
            nm <- d %>% as.character()
      }
      layer <-
         terra::rast(
            nrows = nrow,
            ncols = ncol,
            extent = e,
            crs=EPSG,
            resolution=dx,
            time=d,
            names=nm,
            vals = data
         )
      terra::NAflag(layer) <- nodata
      return(layer)
   }

   is_idf_type <- (typeof(x) == "character") &
         (.is_idf_extension(fileutils::get_filename_extension(x)))

   if (is_idf_type) {
      x <- x %>% mapply(FUN=.read.idf, USE.NAMES=FALSE)

   } else {
      x <- x %>% mapply(FUN=terra::rast, USE.NAMES=FALSE)
   }

   if (!is.null(e)) {
      x <- x |> lapply(FUN=terra::crop, y=e)
   }
   x <- suppressWarnings(terra::rast(x, ...))

   if ((terra::crs(x)=="")||(is_idf_type)) {
      terra::crs(x) <- EPSG
   }


   if (!is.null(funstr)) {
      x <- funstr %>% create_funstr() %>% str2lang() %>% eval()
   }
   return(x)
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

#' Write raster data to a file.
#'
#' Writes a terra::SpatRaster object to a file, using one of
#' the supported formats in the 'terra' package.
#' or an idf file format (ref. \url{https://oss.deltares.nl/web/imod}).
#' In that case, use the'.idf' extension in the filename.
#' @inheritParams read_raster
#' @param x terra::SpatRaster object.
#' @param filename Output filename(s). In case "x" contains multiple layers and idf-output files are needed, specify a character vector of filenames (character).
#' @param double_precision Write double precision idf. Only meaningfull for writing idf-files (logical).
#' @param ... Additional arguments as in 'terra::writeRaster'.
#' @return This function is used for the side-effect of writing values to a file.
#' @importFrom magrittr %<>%
#' @export
write_raster <- function(x,
                         filename,
                         e = NULL,
                         funstr=NULL,
                         double_precision = FALSE,
                         ...) {
   # Write binary with som defaults
   .write_bin <- function(object = 0L,
                          con,
                          size = 4L) {
      writeBin(
         object = object,
         con = con,
         size = size,
         endian = "little"
      )
   }

   .write.idf <-
      function(x,
               filename,
               double_precision = FALSE,
               overwrite = FALSE) {
         if ((overwrite == TRUE) |
             (overwrite == FALSE &
              (!file.exists(filename)))) {
            con = file(filename, "wb")
            if (double_precision) {
               size <- 8
               lahey <- 2295
            } else {
               size <- 4
               lahey <- 1271
            }
            .write_bin(as.integer(lahey), con = con)
            if (double_precision) {
               .write_bin(con = con)
            }
            x %>% terra::ncol() %>% as.integer() %>% .write_bin(con = con)
            if (double_precision) {
               .write_bin(con = con)
            }
            x %>% terra::nrow() %>% as.integer() %>% .write_bin(con = con)
            if (double_precision) {
               .write_bin(con = con)
            }
            writeBin(
               c(
                  terra::xmin(x),
                  terra::xmax(x),
                  terra::ymin(x),
                  terra::ymax(x),
                  terra::minmax(x)[1],
                  terra::minmax(x)[2],
                  terra::NAflag(x)
               ),
               con,
               size = size,
               endian = "little"
            )
            .write_bin(con = con)
            if (double_precision) {
               .write_bin(con = con)
            }
            x %>% terra::res() %>% .write_bin(con = con, size = size)
            x %>% terra::values() %>% as.vector() %>% .write_bin(con = con, size = size)
            close(con)
         } else {
            stop("Error in .write.idf: file exists; use overwrite=TRUE")
         }
      }

   if (!is.null(e)) {
      x %<>% terra::crop(e)
   }

   if (!is.null(funstr)) {
      x <- funstr %>% create_funstr() %>% str2lang() %>% eval()
   }
   if (.is_idf_extension(fileutils::get_filename_extension(filename))) {
      x %>% mapply(FUN=.write.idf, filename, MoreArgs=list(double_precision=double_precision, ...), USE.NAMES = FALSE)
   } else {
      tictoc::tic("--> terra::writeRaster <--")
      suppressWarnings(terra::writeRaster(x, filename, ...))
      tictoc::toc()
   }
}

# ----------------------------------------------------------------------------
