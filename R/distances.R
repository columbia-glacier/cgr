#' Distance to nearest element
#'
#' @param x Numeric vector.
#' @export
#' @examples
#' x <- c(0, 1, 5, 9)
#' nearest_distance(x)
nearest_distance <- function(x) {
  reorder <- order(x)
  revorder <- (1:length(x))[reorder]
  x %>%
    extract(reorder) %>%
    diff() %>%
    abs() %>%
    {c(.[1], pmin(.[-length(.)], .[-1]), .[length(.)])} %>%
    extract(revorder)
}

#' Width of bounding midpoints
#'
#' @param x Numeric vector.
#' @export
#' @examples
#' x <- c(0, 1, 5, 9)
#' nearest_width(x)
nearest_width <- function(x) {
  reorder <- order(x)
  revorder <- (1:length(x))[reorder]
  x %>%
    extract(reorder) %>%
    diff() %>%
    abs() %>%
    divide_by(2) %>%
    {c(.[1], rowSums(cbind(.[-length(.)], .[-1])), .[length(.)])} %>%
    extract(revorder)
}
