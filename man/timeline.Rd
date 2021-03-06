% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{timeline}
\alias{timeline}
\title{Plot a compact timeline}
\usage{
timeline(ranges, start = "from", end = "to", name, group = NULL,
  width = 2, color = "darkgrey")
}
\arguments{
\item{ranges}{(data.frame) See \code{\link{tblranges}}.}

\item{start, end}{(character) Name of columns containing temporal endpoints.}

\item{name}{(character) Name of column containing labels.}

\item{group}{(character) Name of column containing group names.}

\item{width}{(numeric) Line width.}

\item{color}{Line color.}
}
\description{
Plot a compact timeline
}
