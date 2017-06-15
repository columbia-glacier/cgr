# ---- Read data packages ----

#' Create column specification
#'
#' Return the \code{readr::\link[readr]{cols}} column specification for the Data Package Table Schema column type.
#'
#' @param type Column type.
#' @param format Format specification (e.g. see \code{\link[readr]{parse_datetime}}). If set to "", date times are parsed as ISO8601, and dates and times are parsed using the date and time formats specified in the \code{\link[readr]{locale}}.
#' @family Data package functions
#' @export
#' @examples
#' datapackage_col_type("string")
#' datapackage_col_type("number")
#' datapackage_col_type("datetime")
#' datapackage_col_type("boolean")
datapackage_col_type <- function(type, format = "") {
  switch(
    type,
    string = readr::col_character(),
    number = readr::col_number(),
    integer = readr::col_integer(),
    date = readr::col_date(format = format),
    time = readr::col_time(format = format),
    datetime = readr::col_datetime(format = format),
    boolean = readr::col_logical(),
    readr::col_guess()
  )
}

#' Read Data Package metadata
#'
#' @param path Character string.
#' @param simplifyVector Whether to simplify nested lists into vectors and data frames.
#' @param ... Arguments passed to \code{\link[jsonlite]{fromJSON}}.
#' @family Data package functions
#' @export
datapackage_info <- function(path = ".", simplifyVector = TRUE, ...) {
  path %>%
    file.path("datapackage.json") %>%
    jsonlite::read_json(simplifyVector = simplifyVector, ...)
}
#' @rdname datapackage_info
#' @param repo GitHub repository address (see \code{\link{github_raw_url}}).
#' @export
#' @examples
#' datapackage_info_github("columbia-glacier/noaa-coops")
datapackage_info_github <- function(repo, simplifyVector = TRUE, ...) {
  repo %>%
    github_raw_url %>%
    datapackage_info(simplifyVector = simplifyVector, ...)
}

#' Read Data Package data
#'
#' NOTE: Only Table Schema and CSV files are supported.
#'
#' @param path Character string.
#' @param resources Resources to read, by name or relative path.
#' @param simplify Whether to return single results unlisted.
#' @family Data package functions
#' @export
datapackage_data <- function(path = ".", resources = NULL, simplify = TRUE) {
  resource_info <- path %>%
    datapackage_info(simplifyVector = FALSE) %$%
    resources
  resource_names <- resource_info %>% sapply("[[", "name")
  resource_paths <- resource_info %>% sapply("[[", "path")
  if (is.null(resources)) {
    selected <- seq_along(resource_info)
  } else {
    selected <- resource_names %in% resources | resource_paths %in% resources
  }
  results <- lapply(resource_info[selected], function(resource) {
    col_names <- resource$schema$fields %>% sapply("[[", "name")
    col_types <- resource$schema$fields %>%
      sapply("[[", "type") %>%
      sapply(datapackage_col_type) %>%
      set_names(col_names) %>%
      do.call(readr::cols, .)
    resource$path %>%
      file.path(path, .) %>%
      readr::read_csv(col_types = col_types)
  }) %>%
    set_names(resource_names[selected])
  if (simplify && length(results) == 1) {
    results[[1]]
  } else {
    results
  }
}
#' @rdname datapackage_data
#' @param repo GitHub repository address (see \code{\link{github_raw_url}}).
#' @export
#' @examples
#' datapackage_data_github("columbia-glacier/optical-surveys-1985")
#' datapackage_data_github("columbia-glacier/noaa-coops", resources = "columbia-glacier")
datapackage_data_github <- function(repo, resources = NULL, simplify = TRUE) {
  repo %>%
    github_raw_url %>%
    datapackage_data(resources = resources, simplify = simplify)
}

# ---- Write data packages ----

