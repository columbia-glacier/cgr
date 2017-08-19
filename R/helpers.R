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

#' Trim list
#'
#' Removes \code{NULL} and empty lists from a list recursively.
#'
#' @param x List.
#' @export
#' @examples
#' x <- list(
#'   NULL,
#'   list(),
#'   list(list(TRUE, list())),
#'   list(NULL, NULL, list()),
#'   1,
#'   list(2, NULL),
#'   df = data.frame(a = 1, b = 2),
#'   integer(),
#'   list(integer())
#' )
#' str(trim_list(x))
trim_list <- function(x) {
  .rm_null <- function(x) {
    if (rlang::is_bare_list(x)) {
      x %<>%
        subset(!sapply(., is.null)) %>%
        lapply(.rm_null)
    }
    x
  }
  .is_not_empty <- function(x) {
    if (rlang::is_bare_list(x)) {
      any(sapply(x, .is_not_empty))
    } else {
      length(x) > 0
    }
  }
  .rm_empty <- function(x) {
    if (rlang::is_bare_list(x)) {
      x %<>%
        subset(sapply(., .is_not_empty)) %>%
        lapply(.rm_empty)
    }
    x
  }
  x %>%
    .rm_null() %>%
    .rm_empty()
}
