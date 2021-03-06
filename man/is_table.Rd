% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{is_table}
\alias{is_table}
\title{Test if table}
\usage{
is_table(x)
}
\arguments{
\item{x}{Object.}
}
\description{
Tests whether an object is a table.
}
\examples{
is_table(data.frame())
is_table(list())
\dontrun{
is_table(data.table::data.table())
is_table(dtplyr::tbl_dt(data.table::data.table()))
is_table(tibble::tibble())
}
}
\seealso{
Other Table functions: \code{\link{as_table}},
  \code{\link{create_table}},
  \code{\link{parse_table_type}},
  \code{\link{parse_table}}, \code{\link{rbind_tables}},
  \code{\link{remove_empty_dimensions}},
  \code{\link{write_table}}
}
