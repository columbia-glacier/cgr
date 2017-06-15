#' Length of consecutive TRUE
#'
#' @param x Logical vector.
#' @export
#' @examples
#' x <- c(TRUE, TRUE, FALSE, TRUE)
#' length_true(x)
length_true <- function(x) {
  template <- numeric(length(x))
  x <- as.logical(x)
  template[x] <- x %>%
    rle() %>%
    {.$lengths[.$values]} %>%
    rep(., .)
  return(template)
}

#' Group number of consecutive TRUE
#'
#' @param x Logical vector.
#' @export
#' @examples
#' x <- c(TRUE, TRUE, FALSE, TRUE)
#' group_true(x)
group_true <- function(x) {
  template <- NA
  x <- as.logical(x)
  template[x] <- x %>%
    rle() %>%
    {.$lengths[.$values]} %>%
    {rep(1:length(.), .)}
  return(template)
}

#' Cumulative count of consecutive TRUE
#'
#' @param x Logical vector.
#' @export
#' @examples
#' x <- c(TRUE, TRUE, FALSE, TRUE)
#' count_true(x)
count_true <- function(x) {
  cumsum(x) %>%
  {. - cummax((!x) * .)}
}
