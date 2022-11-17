# ----------------------------------------------------------------------------

# Test if the Specified Filename Extension is idf-Filename Extension
# Returns TRUE or FALSE depending on the input filename extension.
#
# @param ext Filename extension (character)
# @return TRUE if 'ext' is a valid idf-filename extension; FALSE otherwhise (logical)
# @examples
# .is_idf_extension(".idf")
# .is_idf_extension(".txt")
# .is_idf_extension(c(".txt",".idf"))
# .is_idf_extension(c(".idf",".idf"))
.is_idf_extension <- function( ext ) {
      ( toupper(ext) == ".IDF") %>% all()
}

# ----------------------------------------------------------------------------

#' Create a string to be used as pattern in the function "create_funstr()".
#'
#' @return  string to be used as pattern in the function "create_funstr()" (character)
#' @export
funstr_ptrn <- function() {
      paste0("\\[[",LETTERS,"\\]]")
}

# ----------------------------------------------------------------------------

#' Create a string ("funstr") to be used in functions read_raster() and write_raster().
#'
#' @param s Blueprint of the string to be created, with the variable between square brackets.
#' @param pattern character string containing a regular expression.
#' @param replacement Symbol to replace the variable with in "s"
#' @return String ("funstr") to be used in functions read_raster() and write_raster()
#' @examples
#' replacement <- c("y","x")
#' create_funstr("[A]*100+[B]", replacement=c("y","x"))
#' @export
create_funstr <- function(s, pattern=funstr_ptrn(), replacement="x") {
      . = NULL
      s %<>% gsub(pattern[1], replacement[1],.) %>% gsub(" ", "", ., fixed=TRUE)
      n <- length(replacement)
      if (n>1) {
            s %>% gsub(pattern[2:n], replacement[2:n],.)
      }  else {
            s
      }
}

# ----------------------------------------------------------------------------

#' Extract the date part of an idf filename with a date included, like in 'HEAD_20080402_l1.idf'.
#' @param idfname idf-filename (character).
##' @return Date
#' @examples
#' idfname_to_date(c("HEAD_20080401_l1.idf","HEAD_20080501_l1.idf"))
#' @export
idfname_to_date <- function(idfname) {
      idfname %>% fileutils::bare_filename() %>% strsplit("_") %>% sapply(function(x) {
            x[2]
      }) %>% lubridate::ymd()
}

# ----------------------------------------------------------------------------

#' Filter filenames of idf- files with a date included, like 'HEAD_20080402_l1.idf'
#' @inheritParams idfname_to_date
#' @param fltr filter to be applied. In this filter a dot indicates the date field (character).
#' @return Selected names of idf-files.
#' @examples
#' idfname <- c("HEAD_20080401_l1.idf","HEAD_20080501_l1")
#' fltr <- "month(.)==4"
#' filter_idfnames(idfname, fltr)
#' @export
filter_idfnames <- function(idfname, fltr = NULL) {
      . <- NULL
      year <- NULL
      day <- NULL
      if (!is.null(fltr)) {
            dates <- idfname %>% idfname_to_date()

            #fltr %<>% gsub("\\.", "dates", .)

            df <-
                  data.frame(
                        date = dates,
                        year = lubridate::year(dates),
                        month = lubridate::month(dates),
                        day = lubridate::day(dates)
                  )
            colnames(df) <-
                  c("date",
                    "year",
                    "month",
                    "day")
            df %<>% dplyr::mutate(hydro_year = ifelse(month %in% 10:12, year + 1, year))
            df %<>% dplyr::mutate(season = ifelse(
                  month %in% 9:11,
                  "herfst",
                  ifelse(
                        month %in% c(12, 1, 2),
                        "Winter",
                        ifelse(month %in% 3:5, "voorjaar",
                               "zomer")
                  )
            ))
            df %<>% dplyr::mutate(hydr_season = ifelse(month %in% c(10:12, 1:3),
                                                "hydr_wintr",
                                                "hydr_summr"))

            df %<>% dplyr::mutate(apr1okt1 = ifelse(
                  month == 4 & day == 1,
                  "apr_1",
                  ifelse(month == 10 & day == 1,
                         "okt_1", "other")
            ))

            sel <- str2lang(paste("df$date %>% ", fltr)) %>% eval()
            return(idfname[sel])
      } else
            return(idfname)
}

# ----------------------------------------------------------------------------

#' Summarize the values of multiple layers into one layer.

#' @inheritParams read_raster
#' @param statistic Statistic to apply to selected rasters: mean, min, max, sd, median (character)
#' @param keys a one row data.frame (or tibble) with no column, consistently with dplyr::group_keys() (tibble)
#' @return terra::SpatRaster
#' @examples
#' statistic <- "max"
#' keys <- data.frame(day=1)
#' f1 <- system.file("extdata", "HEAD_20080401_l1.idf", package="idf")
#' f2 <- system.file("extdata", "HEAD_20080501_l1.idf", package="idf")
#' x <- c(f1, f2)
#' create_statistic_raster(x, statistic=statistic, keys=keys)
#' @export
create_statistic_raster <-
      function(x,
               EPSG = "EPSG:28992",
               e = NULL,
               funstr = NULL,
               statistic = "mean",
               keys) {
            r <- x %>% idf::read_raster(
                  EPSG=EPSG,
                  e=e,
                  funstr=funstr
            )
            print("raster is read")
            r <- paste0("terra::app(r, fun=", statistic,")") %>% str2lang() %>% eval()
            names(r) <-
                  paste0(statistic, "_", colnames(keys), "_", as.character(keys[1,]))
            return(r)
      }
