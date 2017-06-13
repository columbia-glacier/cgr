#' @examples
#' identical(searchenv(), pos.to.env(2))
#' identical(searchenv(1), globalenv())
#' identical(searchenv(length(search())), baseenv())
searchenv <- function(x = 2) {
  pos.to.env(x)
}

#' @examples
#' f <- mean
#' evalq(f, fnenv())
#' \dontrun{
#' x <- 1
#' evalq(x, fnenv())
#' }
fnenv <- function(envir = globalenv(), parent = parent.env(envir)) {
  e <- new.env(parent = parent)
  functions <- lsf.str(envir = envir, all.names = TRUE)
  for(fun in functions) {
    assign(fun, get(fun, envir), e)
  }
  return(e)
}
