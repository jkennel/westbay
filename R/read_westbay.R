#===============================================================================
#' read_westbay
#'
#' @param x name of the file
#' @param ... other arguments
#'
#' @importFrom data.table data.table
#' @importFrom data.table fread
#' @importFrom data.table setnames
#' @importFrom data.table setkey
#' @importFrom data.table melt
#' @importFrom data.table setDT
#' @return data.table
#' @export
#'
#===============================================================================
read_westbay <- function(x,
                         transducer_depth = NULL,
                         well_elevation   = NULL,
                         ...) {
  UseMethod("read_westbay", x)
}

#===============================================================================
#' read_westbay.character
#'
#' @param x name of the file
#' @param tranducer_depth depth
#' @param well_elevation elevation reference
#' @param ...other arguments
#'
#' @return data.table
#' @export
#'
#===============================================================================
read_westbay.character <- function(x,
                                   transducer_depth = NULL,
                                   well_elevation   = NULL,
                                   ...) {

  # .check_files(x)


  h      <- .get_westbay_header(x)
  elev   <- .get_westbay_elevation(h)
  serial <- .get_westbay_probe_serial(h)
  d      <- .get_westbay_data(x)
  dat    <- .parse_westbay_data(d)
  # dat    <- dat[, .split_parameters_westbay(data), by = list(probe_id)]
  depths <- .get_westbay_transducer_depths(d)

  dat    <- dat[depths, on = c("probe_id", "port"), nomatch = 0]
  dat    <- dat[serial, on = "probe_id"]

  # dat[, parameter := channel]
  dat[, model := NA_character_]
  dat[, version := NA_character_]
  dat[, id := probe_id]
  dat[, file := x]
  # dat[, dt := unique(diff(as.numeric(data[[1]]$datetime[1:2]))), by = list(channel, port)]
  dat[, well_elevation := .get_westbay_elevation(h)[["elev"]]]

  # if (!is.null(transducer_depth)) {
  #   dat[, transducer_depth := transducer_depth]
  # }

  # if (!is.null(well_elevation)) {
  #   dat[, well_elevation := well_elevation]
  # }

  dat[, units := .get_westbay_elevation(h)[["units"]]]
  # dat[, calibration := list(data.table(coef = character(), value = numeric()))]

  # setcolorder(dat, c("file", "channel", "data", "id", "calibration",
  #                    "parameter", "units", "version", "serial", "model", "dt",
  #                    "transducer_depth", "well_elevation"))

  dat
}


#===============================================================================
.split_parameters_westbay <- function(x) {

  x <- split(x[[1]], x[[1]]$variable)
  data.table::data.table(data = x, channel = names(x))

}

#===============================================================================
.get_westbay_header <- function(x) {

  h <- readLines(x, warn = FALSE)

  wh <- which(grepl("[Data]", h, fixed = TRUE))

  h <- h[1:wh]

  return(h)

}


#===============================================================================
.get_westbay_data <- function(x) {

  ind  <- readLines(x, warn = FALSE)
  wh   <- grep("[Data]", ind, fixed = TRUE)

  dat <- data.table::fread(x, skip = wh + 1,
                           na.strings = c("", "NA", "N A", "N / A", "N/A", "N/ A",
                                          "Not Available", "NOt available",
                                          '"n/a"', 'n/a'))
  data.table::setDT(dat)
  data.table::setnames(dat, c("name", "datetime", "pressure", "temperature",
                              "probe_id", "probe_status", "probe_description",
                              "port", "port_description", "depth", "comments"))


  dat[, pressure := as.numeric(pressure)]
  dat[, temperature := as.numeric(temperature)]
  dat[, transducer_depth := as.numeric(depth)]
  dat[, comments := as.character(comments)]
  dat[comments == "", comments := NA_character_]
  dat[, probe_description := as.character(probe_description)]
  dat[probe_description == "", probe_description := probe_description]
  dat[, datetime := as.POSIXct(datetime, format = "%Y/%m/%d %H:%M:%S", tz = "EST")]

  data.table::setkey(dat, datetime)

  dat

}

#===============================================================================
.parse_westbay_data <- function(x) {

  x <- x[, data.table::melt(.SD, id.vars = c("datetime", "probe_status", "port")),
         by = probe_id,
         .SDcols = c("datetime", "pressure", "temperature", "probe_status", "port")]
  data.table::setkey(x, probe_id, port)
#
#   x <- split(x, list(x$probe_id, x$port))
#
#   x <- lapply(x, function(x) {
#     x[, probe_id := NULL]
#     x[, port := NULL]
#     })
#
#   x <- data.table(data = x,
#                   probe_id = (names(x)))
#   x
    x

}


#===============================================================================
.get_westbay_probe_serial <- function(x) {

  wh <- which(grepl("Logical Probe-", x, fixed = TRUE))
  matches <- regmatches(x[wh], gregexpr("[[:digit:]]+", x[wh],))

  probe_id     <- as.integer(sapply(matches, "[", 1))
  serial <- sapply(matches, "[", 2)

  data.table(probe_id, serial)

}


#===============================================================================
.get_westbay_transducer_depths <- function(x) {

  unique(x[, list(probe_id,
                  port,
                  probe_description,
                  port_description,
                  transducer_depth = depth)])


}


#===============================================================================
.parse_westbay_header <- function(x) {


  download_time <- .get_westbay_download_time(h)
  probe_info    <- .get_westbay_probe_info(h)
  port_info     <- .get_westbay_port_info(h)
  elevation     <- .get_westbay_elevation(h)


  h <- data.table::data.table(download_time,
                  probe_info = list(probe_info),
                  port_info  = list(port_info),
                  elev       = elevation$elev,
                  elev_unit  = elevation$units)


  invisible(h)
}


#===============================================================================
.get_westbay_download_time <- function(x) {

  wh <- which(grepl("Date of Export:", x, fixed = TRUE))

  tz <- regmatches(x[wh], gregexpr("(?<=\\().*?(?=\\))", x[wh], perl = TRUE))[[1]]
  tz <- gsub("\\b(\\pL)\\pL{2,}|.","\\U\\1", tz ,perl = TRUE)

  download_time <- as.POSIXct(gsub("[^0-9.]", "",  x[wh]), format = "%Y%m%d%H%M%OS", tz = tz)

}


#===============================================================================
.get_westbay_elevation <- function(x) {

  wh <- which(grepl("Elevation:", x, fixed = TRUE))
  units <- regmatches(x[wh], gregexpr("(?<=\\().*?(?=\\))", x[wh], perl = TRUE))[[1]]

  elev <- gsub("[^0-9.]", "",  x[wh])

  data.table(elev = as.numeric(elev), units)

}
