#' Create a parser
#'
#' Creates an unevaluated expression with an attribute for required object names.
#'
#' @param expr Any syntactically valid \code{R} expression.
#' @param required Character vector of required object names.
#' @family Parser functions
#' @export
#' @examples
#' parser(x + 10, required = "x")
#' parser(x + y, required = c("x", "y"))
parser <- function(expr, required = NULL) {
  match.call() %>%
    as.list() %$% expr %>%
    structure(required = required)
}

#' Evaluate a parser
#'
#' Evaluates an unevaluated expression after first checking that the required object names are present in the specified environment.
#'
#' @param parser Object to be evaluated.
#' @param envir Environment in which \code{parser} is to be evaluated. Must contain objects whose names are in \code{(attr(parser, "required")}.
#' @param enclos Environment in which to look for objects not found in \code{envir}.
#' @param default Default value (or function) to return (or evaluate) if requirements are not met.
#' @family Parser functions
#' @export
#' @examples
#' p <- parser(x + 10, required = "x")
#' eval_parser(p, envir = list(x = 1))
#' eval_parser(p, envir = list(y = 1))
#' \dontrun{
#' eval_parser(p, envir = list(y = 1), default = function() stop("Requirements not met"))
#' }
eval_parser <- function(parser, envir = parent.frame(), enclos = globalenv(), default = NULL) {
  if (all(attr(parser, "required") %in% names(envir))) {
    eval(parser, envir = envir, enclos = enclos)
  } else {
    if (is.function(default)) {
      default()
    } else {
      default
    }
  }
}
