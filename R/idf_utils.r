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
#' idfnames <- c("HEAD_20080401_l1.idf","HEAD_20080501_l1")
#' fltr <- "month(.)==4"
#' filter_idfnames(idfnames, fltr)
#' @export
filter_idfnames <- function(idfname, fltr = NULL) {
      . = NULL
      if (!is.null(fltr)) {
            dates <- idfname %>% idfname_to_date()
            fltr %<>% gsub("\\.", "dates", .)
            sel <- fltr %>%  str2lang() %>% eval()
            return(idfname[sel])
      } else
            return(idfname)
}
