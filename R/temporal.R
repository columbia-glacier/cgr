#' Convert to datetime
#'
#' @param x Object to be converted.
#' @param origin Date-time object, or something which can be coerced by as.POSIXct(tz = "GMT") to such an object.
#' @param tz A time zone specification to be used for the conversion.
#' @param format Character string giving a date-time format as used by \code{\link[base]{strptime}}.
#' @export
#' @family temporal functions
#' @examples
#' x <- Sys.time()
#' x == as_time(as.numeric(x))
#' x <- 1:10
#' as_time(x)
#' x <- data.frame(t1 = 1:10, t2 = 1:10)
#' as_time(x)
as_time <- function(x, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = "UTC", format = NULL) {
  if (any(dim(x) > 1) && methods::is(x, "matrix")) {
    x %<>% as.data.frame()
  }
  if (is.atomic(x)) {
    x %>%
      as.POSIXct(tz = tz, origin = origin, format = format)
  } else {
    x %>%
      lapply(function(xi) {
        as.POSIXct(xi, tz = tz, origin = origin, format = format)
      }) %>%
      do.call(what = class(x)[1])
  }
}

#' Convert day-of-year to datetime
#'
#' @param year Year.
#' @param day Day of year.
#' @param utc_offset Offset (in hours) of local time relative to UTC.
#' @export
#' @family temporal functions
#' @examples
#' as_time_doy(1984, 214, -8) == as_time("1984-08-01 08:00:00", format = "%F %T")
as_time_doy <- function(year, day, utc_offset = 0) {
  paste(year, day) %>%
    as_time(format = "%Y %j") %>%
    subtract(60 * 60 * utc_offset)
}
