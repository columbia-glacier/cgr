# ---- Table objects ----

#' Parse table type
#'
#' Parses a table or character string into the corresponding table type.
#'
#' @param x Table or character string.
#' @family Table functions
#' @export
#' @examples
#' parse_table_type(data.frame())
#' parse_table_type("data.frame")
#' parse_table_type("data.table")
#' parse_table_type("tibble")
#' parse_table_type("tbl_df")
#' parse_table_type("tbl_dt")
#' \dontrun{
#' parse_table_type("fake.table")
#' }
#' \dontrun{
#' parse_table_type(data.table::data.table())
#' parse_table_type(tibble::tibble())
#' parse_table_type(dtplyr::tbl_dt(data.table::data.table()))
#' }
parse_table_type <- function(x) {
  type_tags <- list(
    tbl_dt = c("tbl_dt"),
    tbl_df = c("tbl_df", "tbl", "tibble"),
    data.table = c("data.table"),
    data.frame = c("data.frame")
  )
  if (!is.character(x)) {
    x %<>% class()
  }
  ind <- type_tags %>%
    sapply(intersect, x) %>%
    sapply(length) %>%
    as.logical() %>%
    which()
  if (length(ind) > 0) {
    names(type_tags)[ind[1]]
  } else {
    stop("Type not supported")
  }
}

#' Create a table
#'
#' Creates a table of the desired type.
#'
#' @param ... Arguments passed to the corresponding table creation function.
#' @param type Table type (passed to \code{\link{parse_table_type}}).
#' @param stringsAsFactors Whether to convert all character columns to factors.
#' @family Table functions
#' @export
#' @examples
#' create_table(a = 1, b = 2)
#' \dontrun{
#' create_table(a = 1, b = 2, type = "data.table")
#' create_table(a = 1, b = 2, type = "tibble")
#' create_table(a = 1, b = 2, type = "tbl_dt")
#' }
create_table <- function(..., type = data.frame(), stringsAsFactors = FALSE) {
  type %<>% parse_table_type()
  switch(
    type,
    data.frame = data.frame(..., stringsAsFactors = stringsAsFactors),
    data.table = data.table::data.table(..., stringsAsFactors = stringsAsFactors),
    tbl_df = tibble::tibble(...),
    tbl_dt = data.table::data.table(..., stringsAsFactors = stringsAsFactors) %>% dtplyr::tbl_dt(copy = FALSE),
    data.frame(..., stringsAsFactors = stringsAsFactors)
  )
}

#' Coerce to table
#'
#' Coerces an object to a table of the desired type.
#'
#' @param x Object passed to the corresponding table coercion function.
#' @param type Table type (passed to \code{\link{parse_table_type}}).
#' @param stringsAsFactors Whether to convert all character columns to factors.
#' @param ... Arguments passed to the corresponding table coercion function.
#' @family Table functions
#' @export
#' @examples
#' x <- list(a = 1, b = 1)
#' as_table(x)
#' \dontrun{
#' as_table(x, "data.table")
#' as_table(x, "tibble")
#' as_table(x, "tbl_dt")
#' }
as_table <- function(x, type = data.frame(), stringsAsFactors = FALSE, ...) {
  type %<>% parse_table_type()
  switch(
    type,
    data.frame = as.data.frame(x, stringsAsFactors = stringsAsFactors, ...),
    data.table = data.table::as.data.table(x, stringsAsFactors = stringsAsFactors, ...),
    tbl_df = tibble::as_tibble(x, ...),
    tbl_dt = data.table::as.data.table(x, stringsAsFactors = stringsAsFactors, ...) %>% dtplyr::tbl_dt(copy = FALSE),
    as.data.frame(x, stringsAsFactors = stringsAsFactors, ...)
  )
}

#' Test if table
#'
#' Tests whether an object is a table.
#'
#' @param x Object.
#' @family Table functions
#' @export
#' @examples
#' is_table(data.frame())
#' is_table(list())
#' \dontrun{
#' is_table(data.table::data.table())
#' is_table(dtplyr::tbl_dt(data.table::data.table()))
#' is_table(tibble::tibble())
#' }
is_table <- function(x) {
  (class(x) %in% c("data.frame", "data.table", "tbl_df", "tbl_dt")) %>%
    any()
}

# ---- Table operations ----

#' Parse a table
#'
#' Applies parsers to a table and coerces the result to a table of the same type.
#'
#' @param x Table.
#' @param functions Function or list thereof. If a list, any names are reused in the parsed results.
#' @param as Type of object to return. If "table", a table of the same type as \code{x}, otherwise a list.
#' @param enclos Environment in which to look for objects not found in \code{x}.
#' @param ... Arguments passed to \code{\link{feval}}.
#' @family Table functions
#' @export
#' @examples
#' df <- data.frame(x = 1:5, y = 6:10)
#' parsers <- list(
#'   xx = function(x) {x + 10},
#'   xy = function(x, y) {x + y}
#' )
#' parse_table(df, parsers)
#' parse_table(as.list(df), parsers)
#' parse_table(df, parsers, as = "list")
#' parsers$z = function(z) {z * 10}
#' parse_table(df, parsers)
#' parse_table(df, parsers, as = "list")
parse_table <- function(x, functions, as = c("table", "list"), enclos = globalenv(), ...) {
  if (!is.list(functions)) {
    functions %<>% list()
  }
  envir <- x %>% list2env(parent = enclos)
  parsed <- functions %>%
    lapply(feval, envir = envir, ...)
  as <- match.arg(as)
  if (as == "table" && is_table(x)) {
    parsed %>%
      subset(sapply(., length) > 0) %>%
      as_table(type = x)
  } else {
    parsed
  }
}

#' Bind tables by row
#'
#' Same as \code{\link[data.table]{rbindlist}}, but returns a table of the same type as the inputs.
#'
#' @param l List containing \code{data.table}, \code{data.frame} or \code{list} objects.
#' @param use.names Whether to bind tables by matching column names.
#' @param fill Whether to fill missing columns with \code{NA}.
#' @param ... Arguments passed to \code{\link[data.table]{rbindlist}}.
#' @family Table functions
#' @export
#' @examples
#' ldf <- list(
#'   data.frame(x = 1),
#'   data.frame(x = 2)
#' )
#' str(rbind_tables(ldf))
#' \dontrun{
#' ldt <- lapply(ldf, as_table, "data.table")
#' str(rbind_tables(ldt))
#' ltb <- lapply(ldf, as_table, "tibble")
#' str(rbind_tables(ltb))
#' }
rbind_tables <- function(l, use.names = fill, fill = TRUE, ...) {
  table_type <- l %>%
    sapply(parse_table_type) %>%
    unique()
  if (length(table_type) != 1) {
    table_type <- "data.frame"
  }
  data.table::rbindlist(l, use.names = use.names, fill = fill, ...) %>%
    as_table(type = table_type)
}

#' Remove empty table dimensions
#'
#' Removes empty rows and columns from a table.
#'
#' @param x Table.
#' @param ignore Vector of column names to ignore when checking for empty rows.
#' @family Table functions
#' @export
#' @examples
#' df <- data.frame(x = 1, y = NA)
#' remove_empty_dimensions(df)
#' remove_empty_dimensions(df, ignore = "x")
#' \dontrun{
#' dt <- data.table::data.table(x = 1, y = NA)
#' remove_empty_dimensions(dt)
#' remove_empty_dimensions(dt, ignore = "x")
#' tb <- tibble::tibble(x = 1, y = NA)
#' remove_empty_dimensions(tb)
#' remove_empty_dimensions(tb, ignore = "x")
#' }
remove_empty_dimensions <- function(x, ignore = NULL) {
  cols <- not(names(x) %in% ignore)
  na <- is.na(x)
  is_empty_row <- na[, cols, drop = FALSE] %>%
    rowSums() %>%
    equals(sum(cols))
  is_empty_col <- na %>%
    colSums() %>%
    equals(nrow(x))
  x %>%
    subset(!is_empty_row, !is_empty_col)
}

#' Write table to file
#'
#' Writes a table to a standard format.
#'
#' @param x Table.
#' @param file Output file name. "" indicates output to the console.
#' @family Table functions
#' @export
#' @examples
#' df <- data.frame(
#'   date = Sys.Date(),
#'   date_time = Sys.time(),
#'   string = "abc",
#'   integer = as.integer(123),
#'   number = 123.456,
#'   boolean = TRUE
#' )
#' write_table(df, "")
write_table <- function(x, file = "") {
  data.table::fwrite(
    x = x, file = file, na = "", dec = ".",
    sep = ",", sep2 = c("","|",""),
    quote = "auto", qmethod = c("double"),
    row.names = FALSE, col.names = TRUE,
    logicalAsInt = TRUE, dateTimeAs = "ISO"
  )
}
