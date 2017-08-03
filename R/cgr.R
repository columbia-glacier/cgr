#' cgr: R Functions for Columbia Glacier Research
#'
#' @docType package
#' @name cgr
#' @import magrittr
#' @import data.table
NULL

# Quiets concerns of R CMD CHECK: no visible binding for global variable '.'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
  utils::globalVariables(c(".N"))
  utils::globalVariables(c("..bin.."))
}
