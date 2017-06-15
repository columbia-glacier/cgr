#' Evaluate a function
#'
#' Evaluates a function by taking its argument values by name from the specified environment.
#'
#' @param fun Fuction to evaluate.
#' @param envir Environment in which \code{fun} is to be evaluated. Must contain objects of the same name as the arguments in \code{f}.
#' @param enclos Environment in which to look for objects not found in \code{envir}. Relevant only when \code{envir} is a (pair)list or data frame.
#' @param default Default value (or function) to return (or evaluate) if requirements are not met.
#' @export
#' @examples
#' f <- function(x) {x + 1}
#' feval(f, envir = list(x = 1))
#' feval(f, envir = list(y = 1))
#' \dontrun{
#' feval(f, envir = list(y = 1), default = function() stop("Arguments not found"))
#' }
feval <- function(fun, envir = parent.frame(), enclos = globalenv(), default = NULL) {
  if (is.list(envir)) {
    envir %<>% list2env(parent = enclos)
  }
  args <- fun %>% methods::formalArgs()
  if (all(args %in% names(envir))) {
    args %>%
      lapply(as.symbol) %>%
      do.call(fun, ., envir = envir)
  } else {
    if (is.function(default)) {
      default()
    } else {
      default
    }
  }
}

#' Concatenate Strings
#'
#' Extends \code{\link[base]{paste}} with the ability to remove missing values (\code{NA}).
#'
#' @param ... Objects converted to character vectors.
#' @param sep Character string to seperate the terms.
#' @param collapse Character string to separate the results.
#' @param na.rm Whether \code{NA} values should be skipped.
#' @export
#' @source \url{http://stackoverflow.com/a/31508774}
#' @examples
#' paste2(c("hello", "goodbye"), NA, NA, "world", NA)
#' paste2(c("hello", "goodbye"), NA, NA, "world", NA, na.rm = TRUE)
paste2 <- function(..., sep = " ", collapse = NULL, na.rm = FALSE) {
  if (na.rm) {
    paste.na <- function(x, sep) {
      x <- gsub("^\\s+|\\s+$", "", x)
      ret <- paste(stats::na.omit(x), collapse = sep)
      is.na(ret) <- ret == ""
      return(ret)
    }
    ret <- apply(cbind(...), 1, function(x) paste.na(x, sep))
    if (is.null(collapse)) {
      ret
    } else {
      paste.na(ret, sep = collapse)
    }
  } else {
    paste(..., sep = sep, collapse = collapse)
  }
}

#' GitHub Raw URL from Repository Address
#'
#' Get the URL to the raw GitHub user content corresponding to a GitHub repository address.
#'
#' @param repo Repository address in the format \code{username/repo[/subdir][@ref]}, where \code{ref} can be a commit, tag, or branch name. Defaults to "master".
#' @export
#' @examples
#' github_raw_url("username/repo")
#' github_raw_url("username/repo/subdir")
#' github_raw_url("username/repo/subdir@ref")
github_raw_url <- function(repo) {
  repo %>%
    stringr::str_match("^/*([^/]+)/([^/@#]+)/*([^/@#]+)*/*(?:@(.*))*/*$") %>%
    extract(-1) %>%
    ifelse(is.na(.), c(NA, NA, NA, "master"), .) %>%
    {paste2("https://raw.githubusercontent.com", .[1], .[2], .[4], .[3], sep = "/", na.rm = TRUE)}
}

#' Convert to datetime
#'
#' @param x Object to be converted.
#' @param origin Date-time object, or something which can be coerced by as.POSIXct(tz = "GMT") to such an object.
#' @param tz A time zone specification to be used for the conversion.
#' @param format Character string giving a date-time format as used by \code{\link[base]{strptime}}.
#' @export
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

#' Apply default values
#'
#' @param x Vector.
#' @param default Vector of default values, matched by position.
#' @export
#' @examples
#' default <- c(1, 2, 3, 4)
#' x <- c(10)
#' apply_default(x, default)
#' x <- c(10, NA, 30)
#' apply_default(x, default)
apply_default <- function(x, default) {
  ind <- which(is.na(x))
  if (length(x) < length(default)) {
    ind %<>% append((length(x) + 1):length(default))
  }
  x[ind] <- default[ind]
  return(x)
}
