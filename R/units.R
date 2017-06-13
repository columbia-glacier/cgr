#' Parse unit
#'
#' Parses a character string as a unit of measure according to the following notations:
#' \enumerate{
#'   \item Symbolic (e.g. "m/s") via \code{\link[units]{ud_units}}
#'   \item Product power (e.g. "m s-1") via \code{\link[units]{parse_unit}}
#' }
#'
#' For a list of supported units and their notation, see the UDUNITS-2 \href{https://www.unidata.ucar.edu/software/udunits/udunits-2.2.25/doc/udunits/udunits2.html#Database}{Units Database}.
#'
#' @param x Character string.
#' @family Unit functions
#' @export
#' @examples
#' parse_unit("m/s")
#' parse_unit("m s-1")
#' parse_unit("kg * (m^2 / s^3)")
#' parse_unit("kg m2 s-3")
parse_unit <- function(x) {
  functions = list(
    function(x) {
      x %>%
        as.character() %>%
        parse(text = .) %>%
        eval(envir = units::ud_units, enclos = baseenv())
    },
    function(x) {
      x %>%
        as.character() %>%
        units::parse_unit()
    }
  )
  for(fun in functions) {
    if (is_units(x)) {
      break
    }
    try(x <- fun(x), silent = TRUE)
  }
  if (is_units(x)) {
    x
  } else {
    stop("Parsing failed")
  }
}

#' Coerce to units
#'
#' Converts an object to \code{units} using \code{\link[units]{as.units}}.
#'
#' @param x Object.
#' @param unit Units or character string (see \code{\link{parse_unit}}).
#' @family Unit functions
#' @export
#' @examples
#' as_units(1:5, "m/s")
#' as_units(1:5, parse_unit("m/s"))
as_units <- function(x, unit = units::unitless) {
  unit %<>% parse_unit()
  x %>%
    units::as.units(value = unit)
}

#' Test if units
#'
#' Tests whether an object is \code{units}.
#'
#' @param x Object.
#' @family Unit functions
#' @export
#' @examples
#' is_units(1:5)
#' is_units(as_units(1:5))
is_units <- function(x) {
  "units" %in% class(x)
}

#' Convert units
#'
#' Converts values between different units of measure.
#'
#' @param x Object coercible to \code{units}.
#' @param from Units or character string (see \code{\link{parse_unit}}).
#' @param to Units or character string (see \code{\link{parse_unit}}).
#' @family Unit functions
#' @export
#' @examples
#' convert_units(1, "km", "m")
#' x <- as_units(1, "km")
#' convert_units(x, to = "m")
convert_units <- function(x, from = x, to) {
  x %>%
    as_units(from) %>%
    as_units(to)
}
