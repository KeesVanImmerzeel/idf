#' Load setup idf (\href{https://oss.deltares.nl/web/imod/online-imod-user-manual}{iMOD})
#'
#' @param file character string, filename.
#' @importFrom fs file_exists path_file
#' @importFrom stringr str_c
#' @export get_head_idf
#' @examples
#' # extract setup of idf-file
#' file <- system.file("extdata/test.idf", package = "idfR")
#' get_head_idf(file = file)
get_head_idf <- function(file) {

  # ---- initial part of procedure ----

  # check if file exists
  if (!file_exists(file)) stop(str_c("file '", path_file(file), "' does not exists"))

  # create empty list
  head_idf <- list()

  # ---- main part of procedure ----

  # open connection
  con <- file(description = file, open = "rb")

  # load kind of precision
  type <- readBin(con = con, what = integer(), n = 1, size = 4, endian = "little")
  if (!type %in% c(1271, 2296)) stop("unknown precision of idf")
  head_idf$precision <- ifelse(type == 1271, "single", "double")
  size <- ifelse(head_idf$precision == "single", 4, 8)

  # load number of columns and rows
  head_idf$ncol <- readBin(con = con, what = integer(), n = 1, size = size, endian = "little")
  head_idf$nrow <- readBin(con = con, what = integer(), n = 1, size = size, endian = "little")

  # load extent
  head_idf$xmin <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  head_idf$xmax <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  head_idf$ymin <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  head_idf$ymax <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")

  # load minimum, maximum and nodata-value
  head_idf$dmin <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  head_idf$dmax <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  head_idf$nodata <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")

  # check non-equidistants
  head_idf$ieq <- readBin(con = con, what = integer(), n = 1, size = 1, endian = "little")

  # check usage of TOP and BOT values
  itb <- readBin(con = con, what = integer(), n = 1, size = 1, endian = "little")
  if (itb == 1) stop("Usage of TOP and BOT values not supported yet...")

  # skip two lines
  seek(con = con, where = 2, origin = "current")

  # load dx and dy
  if (head_idf$ieq == 0) {
    head_idf$dx <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
    head_idf$dy <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
  }
  if (head_idf$ieq == 1) {
    head_idf$dx <- readBin(con = con, what = double(), n = head_idf$ncol, size = size, endian = "little")
    head_idf$dy <- readBin(con = con, what = double(), n = head_idf$nrow, size = size, endian = "little")
  }

  # close connection
  close(con)

  # save alternative index in case of non-equidistant idf
  if (head_idf$ieq == 1) {

    # add col index based on minimum dx
    head_idf$dx_min <- min(head_idf$dx)
    head_idf$ix <- cumsum(ifelse(is.na(match(x = seq(from = 0, to = (sum(head_idf$dx) - head_idf$dx_min), by = head_idf$dx_min), table = cumsum(head_idf$dx) - head_idf$dx)),0,1))

    # add row index based on minimum dy
    head_idf$dy_min <- min(head_idf$dy)
    head_idf$iy <- cumsum(ifelse(is.na(match(x = seq(from = 0, to = (sum(head_idf$dy) - head_idf$dy_min), by = head_idf$dy_min), table = cumsum(head_idf$dy) - head_idf$dy)),0,1))
  }

  # ---- return of procedure ----

  return(head_idf)
}

#' Load data idf (\href{https://oss.deltares.nl/web/imod/online-imod-user-manual}{iMOD})
#'
#' @param file character string, filename
#' @param col numeric vector, column to extract
#' @param row numeric vector, row to extract
#' @param x_crd numeric vector, x-coordinate to extract
#' @param y_crd numeric vector, y-coordinate to extract
#' @importFrom fs file_exists path_file
#' @importFrom stringr str_c
#' @details by default all data is extracted, vector starts from upper left corner
#' @export get_data_idf
#' @examples
#' # extract data of idf-file
#' file <- system.file("extdata/test.idf", package = "idfR")
#' get_data_idf(file = file)
#' get_data_idf(file = file, col = c(1,3), row = c(1,1))
#' get_data_idf(file = file, x_crd = c(219001,219050), y_crd = c(501100, 501099))
get_data_idf <- function(file, col = NULL, row = NULL, x_crd = NULL, y_crd = NULL) {

  # ---- initial part of procedure ----

  if (!file_exists(file)) stop(str_c("file '", path_file(file), "' does not exists"))

  # ---- main part of procedure ----

  # set extraction modus
  modus <- "all"
  if ((!is.null(col) & !is.null(row)) | (!is.null(x_crd) & !is.null(y_crd))) modus <- "vector"

  # get idf setup
  head_idf <- get_head_idf(file = file)

  # set precision
  size <- ifelse(head_idf$precision == "single", 4, 8)

  # set columns and rows to extract
  if (!is.null(x_crd) & !is.null(y_crd)) {
    if (head_idf$ieq == 0) {
      col <- ifelse(x_crd == head_idf$xmax, head_idf$ncol, floor((x_crd - head_idf$xmin) / head_idf$dx) + 1)
      row <- ifelse(y_crd == head_idf$ymin, head_idf$nrow, floor((head_idf$ymax - y_crd) / head_idf$dy) + 1)
    }
    if (head_idf$ieq == 1) {
      col_tmp <- ifelse(x_crd == head_idf$xmax, length(head_idf$ix), floor((x_crd - head_idf$xmin) / head_idf$dx_min) + 1)
      if (!all(col_tmp >= 1) | !all(col_tmp <= length(head_idf$ix))) stop("point(s) selected outside domain")
      col <- head_idf$ix[col_tmp]
      row_tmp <- ifelse(y_crd == head_idf$ymin, length(head_idf$iy), floor((head_idf$ymax - y_crd) / head_idf$dy_min) + 1)
      if (!all(row_tmp >= 1) | !all(row_tmp <= length(head_idf$iy))) stop("point(s) selected outside domain")
      row <- head_idf$iy[row_tmp]
    }
  }

  # check if selected point is in domain
  if (modus == "vector") {
    if (length(col) != length(row)) stop("size of vector 'col' and 'row' should be equal")
    if (!all(col >= 1) | !all(col <= head_idf$ncol)) stop("point(s) selected outside domain")
    if (!all(row >= 1) | !all(row <= head_idf$nrow)) stop("point(s) selected outside domain")

    skip_bytes <- ((head_idf$ncol * (row - 1)) + (col - 1)) * size
    order_extract <- order(skip_bytes)
  }

  # open connection
  con <- file(description = file, open = "rb")

  # skip header
  if (head_idf$ieq == 0) {
    where <- 16 + 9 * size
  } else {
    where <- 16 + (7 + head_idf$ncol + head_idf$nrow) * size
  }
  seek(con = con, where = where, origin = "start")

  # load data
  if (modus == "all") {
    data <- readBin(con = con, what = double(), n = head_idf$ncol*head_idf$nrow, size = size, endian = "little")
  }
  if (modus == "vector") {
    nrec <- length(col)
    data <- rep(x = head_idf$nodata, times = nrec)
    bytes_skipped <- 0
    for (rec in 1:nrec) {
      position <- skip_bytes[order_extract[rec]]
      where <- position - bytes_skipped
      if (where < 0) {
        data[order_extract[rec]] <- value
      } else {
        if (where > 0) seek(con = con, where = where, origin = "current")
        value <- readBin(con = con, what = double(), n = 1, size = size, endian = "little")
        bytes_skipped <- position + size
        data[order_extract[rec]] <- value
      }
    }
  }

  # close connection
  close(con)

  # ---- return of procedure ----

  return(data)
}

#' Read idf (\href{https://oss.deltares.nl/web/imod/online-imod-user-manual}{iMOD})
#'
#' @param file character string, filename
#' @param col numeric vector, column to extract
#' @param row numeric vector, row to extract
#' @param x_crd numeric vector, x-coordinate to extract
#' @param y_crd numeric vector, y-coordinate to extract
#' @importFrom fs file_exists path_file
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @details by default all data is extracted, vector starts from upper left corner
#' @export read_idf
#' @examples
#' # read idf-file
#' file <- system.file("extdata/test.idf", package = "idfR")
#' read_idf(file = file)
read_idf <- function(file, col = NULL, row = NULL, x_crd = NULL, y_crd = NULL) {

  # ---- initial part of procedure ----

  if (!file_exists(file)) stop(str_c("file '", path_file(file), "' does not exists"))


  # ---- main part of procedure ----

  # get idf setup
  idf <- get_head_idf(file = file)

  # set columns and rows to extract
  if (!is.null(x_crd) & !is.null(y_crd)) {
    if (idf$ieq == 0) {
      col <- ifelse(x_crd == idf$xmax, idf$ncol, floor((x_crd - idf$xmin) / idf$dx) + 1)
      row <- ifelse(y_crd == idf$ymin, idf$nrow, floor((idf$ymax - y_crd) / idf$dy) + 1)
    }
    if (idf$ieq == 1) {
      col_tmp <- ifelse(x_crd == idf$xmax, length(idf$ix), floor((x_crd - idf$xmin) / idf$dx_min) + 1)
      if (col_tmp < 1 | col_tmp > length(idf$ix)) stop("point(s) selected outside domain")
      col <- idf$ix[col_tmp]
      row_tmp <- ifelse(y_crd == idf$ymin, length(idf$iy), floor((idf$ymax - y_crd) / idf$dy_min) + 1)
      if (row_tmp < 1 | row_tmp > length(idf$iy)) stop("point(s) selected outside domain")
      row <- idf$iy[row_tmp]
    }
  }

  # get data idf
  value <- get_data_idf(file = file, col = col, row = row)

  # set columns and rows (if needed)
  if (is.null(col) & is.null(row)) {
    col <- rep(x = 1:idf$ncol, times = idf$nrow)
    row <- sort(rep(x = 1:idf$nrow, times = idf$ncol))
  }

  # set idf data
  idf$data <- tibble(col = col, row = row, value = value)


  # ---- return of procedure ----

  return(idf)
}

#' Write idf (\href{https://oss.deltares.nl/web/imod/online-imod-user-manual}{iMOD})
#'
#' @param file character string, filename.
#' @param idf list, setup and data of idf.
#' @importFrom fs file_exists file_delete path_file
#' @importFrom stringr str_c
#' @export write_idf
write_idf <- function(file, idf) {

  # ---- initial part of procedure ----

  # check if file exists
  if (file_exists(file)) file_delete(path = file)

  # ---- main part of procedure ----

  # update dmin and dmax
  limit <- range(idf$data$value[idf$data$value != idf$nodata], na.rm = TRUE)
  idf$dmin <- limit[1]
  idf$dmax <- limit[2]

  # open connection
  con <- file(description = file, open = "wb")

  # set kind of precision
  if (!idf$precision %in% c("single", "double")) stop("unknown precision of idf")
  size <- ifelse(idf$precision == "single", 4, 8)
  type <- ifelse(idf$precision == "single", 1271, 2296)
  writeBin(object = as.integer(type), con = con, size = 4, endian = "little")

  # set number of columns and rows
  writeBin(object = as.integer(idf$ncol), con = con, size = 4, endian = "little")
  writeBin(object = as.integer(idf$nrow), con = con, size = 4, endian = "little")

  # set extent
  writeBin(object = idf$xmin, con = con, size = size, endian = "little")
  writeBin(object = idf$xmax, con = con, size = size, endian = "little")
  writeBin(object = idf$ymin, con = con, size = size, endian = "little")
  writeBin(object = idf$ymax, con = con, size = size, endian = "little")

  # set minimum, maximum and nodata-value
  writeBin(object = idf$dmin, con = con, size = size, endian = "little")
  writeBin(object = idf$dmax, con = con, size = size, endian = "little")
  writeBin(object = idf$nodata, con = con, size = size, endian = "little")

  # set non-equidistants
  writeBin(object = as.integer(idf$ieq), con = con, size = 1, endian = "little")

  # set TOP and BOT values
  writeBin(object = as.integer(0), con = con, size = 1, endian = "little")

  # skip lines with blanks
  writeBin(object = as.integer(c(0,0)), con = con, size = 1, endian = "little")

  # set dx and dy
  writeBin(object = idf$dx, con = con, size = size, endian = "little")
  writeBin(object = idf$dy, con = con, size = size, endian = "little")

  # set data
  nrec <- idf$ncol * idf$nrow
  data <- idf$data
  if (nrow(data) != nrec) {
    value <- rep(x = idf$nodata, times = nrec)
    value[(idf$ncol * (data$row - 1)) + data$col] <- data$value
  } else {
    value <- data$value[order(data$row, data$col)]
  }
  writeBin(object = value, con = con, size = size, endian = "little")

  # close connection
  close(con)
}

#' Get column number
#'
#' @param file character string, filename
#' @param x_crd numeric vector, x-coordinate
#' @importFrom fs file_exists path_file
#' @importFrom stringr str_c
#' @export get_col_idf
#' @examples
#' # get column number from idf-file based on x-coordinate(s)
#' file <- system.file("extdata/test.idf", package = "idfR")
#' get_col_idf(file = file, x_crd = c(219001,219050))
get_col_idf <- function(file, x_crd) {

  # ---- initial part of procedure ----

  if (!file_exists(file)) stop(str_c("file '", path_file(file), "' does not exists"))

  # ---- main part of procedure ----

  # get idf setup
  idf <- get_head_idf(file = file)

  # set columns and rows to extract
  if (idf$ieq == 0) {
      col <- ifelse(x_crd == idf$xmax, idf$ncol, floor((x_crd - idf$xmin) / idf$dx) + 1)
  }
  if (idf$ieq == 1) {
    col_tmp <- ifelse(x_crd == idf$xmax, length(idf$ix), floor((x_crd - idf$xmin) / idf$dx_min) + 1)
    if (col_tmp < 1 | col_tmp > length(idf$ix)) stop("point(s) selected outside domain")
    col <- idf$ix[col_tmp]
  }

  # ---- return of procedure ----

  return(col)
}

#' Get row number
#'
#' @param file character string, filename
#' @param y_crd numeric vector, y-coordinate
#' @importFrom fs file_exists path_file
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @export get_row_idf
#' @examples
#' # get column number from idf-file based on y-coordinate(s)
#' file <- system.file("extdata/test.idf", package = "idfR")
#' get_row_idf(file = file, y_crd = c(219001,219050))
get_row_idf <- function(file, y_crd) {

  # ---- initial part of procedure ----

  if (!file_exists(file)) stop(str_c("file '", path_file(file), "' does not exists"))


  # ---- main part of procedure ----

  # get idf setup
  idf <- get_head_idf(file = file)

  # set columns and rows to extract
  if (idf$ieq == 0) {
    row <- ifelse(y_crd == idf$ymin, idf$nrow, floor((idf$ymax - y_crd) / idf$dy) + 1)
  }
  if (idf$ieq == 1) {
    row_tmp <- ifelse(y_crd == idf$ymin, length(idf$iy), floor((idf$ymax - y_crd) / idf$dy_min) + 1)
    if (row_tmp < 1 | row_tmp > length(idf$iy)) stop("point(s) selected outside domain")
    row <- idf$iy[row_tmp]
  }

  # ---- return of procedure ----

  return(row)
}

#' Add coordinates to data of idf-layer
#'
#' @param idf list, setup and data of idf-layer.
#' @importFrom dplyr %>% mutate
#' @export xy_crd_idf
#' @examples
#' # add coordinates to idf-layer
#' file <- system.file("extdata/test.idf", package = "idfR")
#' idf <- read_idf(file = file)
#' xy_crd_idf(idf = idf)
xy_crd_idf <- function(idf) {

  # ---- main part of procedure ----

  # clip body
  idf$data <- idf$data %>%
    mutate(
      x_crd = idf$xmin + (0.5 * idf$dx) + (col - 1) * idf$dx,
      y_crd = idf$ymax - (0.5 * idf$dy) - (row - 1) * idf$dy,
    )

  # ---- return of procedure ----

  return(idf)
}

#' Clip idf-layer based on extent
#'
#' @param idf list, setup and data of idf-layer.
#' @param extent list, new extent of layer
#' @importFrom dplyr %>% mutate filter select if_else
#' @export clip_idf
#' @examples
#' # create extent
#' extent <- list(xmin = 219000, xmax = 219050, ymin = 501000, ymax = 501050)
#'
#' # clip idf-layer based on extent
#' file <- system.file("extdata/test.idf", package = "idfR")
#' idf <- read_idf(file = file)
#' clip_idf(idf = idf, extent = extent)
clip_idf <- function(idf, extent) {

  x_crd <- y_crd <- value <- NULL

  # ---- main part of procedure ----

  # clip body
  db <- idf$data %>%
    mutate(
      x_crd = idf$xmin + (0.5 * idf$dx) + (col - 1) * idf$dx,
      y_crd = idf$ymax - (0.5 * idf$dy) - (row - 1) * idf$dy,
      col = col - (extent$xmin - idf$xmin) / idf$dx,
      row = row - (idf$ymax - extent$ymax) / idf$dy
    ) %>%
    filter(x_crd > extent$xmin & x_crd < extent$xmax & y_crd > extent$ymin & y_crd < extent$ymax) %>%
    select(col, row, value)

  # update layer information
  idf$ncol <- max(db$col)
  idf$nrow <- max(db$row)
  idf$xmin <- extent$xmin
  idf$xmax <- extent$xmax
  idf$ymin <- extent$ymin
  idf$ymax <- extent$ymax
  idf$data <- db

  # replace nodata with NA
  db <- db %>%
    mutate(value = if_else(value == idf$nodata, NA_real_, value))

  # update dmin and dmax
  limit <- range(db$value, na.rm = TRUE)
  idf$dmin <- limit[1]
  idf$dmax <- limit[2]

  # ---- return of procedure ----

  return(idf)
}
