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
