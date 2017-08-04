#' Select elements within ranges
#'
#' @param x Position vector to evaluate against ranges.
#' @param ranges Ranges.
#' @export
#' @examples
#' x <- as_time(1:10)
#' ranges <- x[1:3]
#' range_select(x, ranges)
#' ranges <- as_time(data.frame(1, 2:4))
#' range_select(x, ranges)
range_select <- function(x, ranges) {
  if (is.atomic(ranges)) {
    ranges <- data.frame(ranges[1:(length(ranges) - 1)], ranges[2:length(ranges)])
  }
  mapply(ranges[, 1], ranges[, 2], FUN = function(ifrom, ito) {
    x >= ifrom & x <= ito
  }, USE.NAMES = FALSE, SIMPLIFY = TRUE)
}

#' Split object by ranges
#'
#' @param x Vector or data frame to be split into groups.
#' @param t Position vector to evaluate against ranges.
#' @param ranges Ranges.
#' @export
#' @examples
#' x <- 1:10
#' t <- as_time(1:10)
#' ranges <- t[1:3]
#' range_split(x, t, ranges)
#' ranges <- as_time(data.frame(1, 2:4))
#' range_split(x, t, ranges)
#' x <- data.frame(V1 = 1:10, V2 = 1:10)
#' range_split(x, t, ranges)
range_split <- function(x, t, ranges) {
  range_select(t, ranges) %>%
    split(., col(.)) %>%
    {
      if (is.null(ncol(x))) {
        lapply(., function(i) {
          x[i]
        })
      } else {
        lapply(., function(i) {
          x[i, ]
        })
      }
    } %>%
    set_names(NULL)
}

#' Generate sequential ranges
#'
#' @param from Starting value of the sequence. If \code{length() > 1}, \code{from = min()} and \code{to = max()}.
#' @param to If \code{length(from) = 1}, maximal end value of the sequence.
#' @param by Range width.
#' @param length.out Desired length of the sequence.
#' @param step Increment of the range starting value.
#' @export
#' @examples
#' x <- as_time(1:10)
#' seq_ranges(x, by = 3)
#' seq_ranges(x, length.out = 2)
#' seq_ranges(x, by = 3, step = 1)
#' seq_ranges(x[1], x[3])
seq_ranges <- function(from = 1, to = from[length(from)], by = (to - from[1]) / length.out, length.out = as.numeric(to - from[1]), step = by) {
  if (is.null(step)) {
    seq(from[1], to, by) %>%
    {data.frame(from = .[-length(.)], to = .[-1])}
  } else {
    seq(from[1], to - by, step) %>%
      data.frame(from = ., to = . + by)
  }
}

#' Generate rolling ranges
#'
#' @param x Numeric vector.
#' @param dx Width of ranges (by values of \code{x}).
#' @param dxi Width of ranges (by number of elements of \code{x}).
#' @param align Alignment of ranges with respect to values in \code{x}.
#' @param trim Method for trimming ranges extending beyond range of \code{x}.
#' @export
#' @examples
#' x <- as_time(seq(0, 60 * 60 * 5, by = 60 * 60))
#' dx <- 60 * 60 * 2.5
#' roll_ranges(x, dx, align = "left")
#' roll_ranges(x, dx, align = "center")
#' roll_ranges(x, dx, align = "right")
#' roll_ranges(x, dx, align = "left", trim = "snap")
#' roll_ranges(x, dx, align = "left", trim = "cut")
#' roll_ranges(x, dx, align = "left", trim = "drop")
#' dxi <- 3
#' roll_ranges(x, dxi = dxi, align = "left")
#' roll_ranges(x, dxi = dxi, align = "right")
#' roll_ranges(x, dxi = dxi, align = "center")
roll_ranges <- function(x, dx = NULL, dxi = 1, align = c("left", "center", "right"), trim = c("none", "snap", "cut", "drop")) {
  if (is.null(dx)) {
    dxi %<>% round() %>% rep_len(length(x))
    ind <- 1:length(x)
    switch(
      match.arg(align),
      left = {
        is_inbounds <- ind + dxi <= length(x)
        from <- x[ind[is_inbounds]]
        to <- x[(ind + dxi)[is_inbounds]]
      },
      center = {
        dx_left <- (ind - dxi) %>%
          replace(. < 1, NA) %>%
          {(x[ind] - x[.]) / 2}
        dx_right <- (ind + dxi) %>%
          replace(. > length(x), NA) %>%
          {(x[.] - x[ind]) / 2}
        dx_left[is.na(dx_left)] <- dx_right[is.na(dx_left)]
        dx_right[is.na(dx_right)] <- dx_left[is.na(dx_right)]
        from <- x[ind] - dx_left
        to <- x[ind] + dx_right
      },
      right = {
        is_inbounds <- (ind - dxi) > 0
        from <- x[(ind - dxi)[is_inbounds]]
        to <- x[ind[is_inbounds]]
      }
    )
  } else {
    dx %<>% rep_len(length(x))
    from <- switch(
      match.arg(align),
      left = x,
      center = x - dx / 2,
      right = x - dx
    )
    to <- from + dx
  }
  ranges <- data.frame(from, to)
  is_small <- from < min(x)
  is_large <- to > max(x)
  switch(
    match.arg(trim),
    snap = {
      from[is_small] %<>% {. + (min(x) - from[is_small])}
      to[is_small] %<>% {. + (min(x) - from[is_small])}
      from[is_large] %<>% {. - (to[is_large] - max(x))}
      to[is_large] %<>% {. - (to[is_large] - max(x))}
    },
    cut = {
      from[is_small] <- min(x)
      to[is_large] <- max(x)
    },
    drop = {
      is_inbounds <- from >= min(x) & to <= max(x)
      from <- from[is_inbounds]
      to <- to[is_inbounds]
    }
  )
  data.frame(from, to)
}

#' Mean of range endpoints
#'
#' @param ranges Ranges.
#' @export
#' @examples
#' ranges <- as_time(cbind(0, 10))
#' range_mean(ranges)
range_mean <- function(ranges) {
  ranges[, 1] + (ranges[, 2] - ranges[, 1]) / 2
}

#' Generate ranges from a column (vector)
#'
#' Constructs ranges that fit the coverage of point measures.
#'
#' @param t (atomic) Measures along a single dimension (temporal or otherwise) coercible to numeric.
#' @param maxdt (numeric) Maximum difference in \code{t} to consider as continuous.
#' @param x (atomic) Values of the same length as \code{t} used to ignore missing values (\code{NA}).
#' @export
colranges <- function(t, maxdt = Inf, x = NULL) {
  # Sort by time
  i <- order(t)
  t <- t[i]
  # Compute point criteria
  is_na <- is.na(t)
  if (!is.null(x)) {
    is_na <- is_na | is.na(x[i])
  }
  is_break <- c(diff(as.numeric(t)) > maxdt, FALSE)
  # Build endpoints
  # from: not NA & (previous is (break | NA) | first)
  # to: not NA & (next is (NA) | break | last)
  n <- length(t)
  is_from <- c(!is_na[1], !is_na[-1] & (is_na[-n] | is_break[-n]))
  from <- t[is_from]
  is_to <- c(!is_na[-n] & (is_break[-n] | is_na[-1]), !is_na[n])
  to <- t[is_to]
  # Close any small gaps (left by NAs)
  if (length(from) > 0) {
    ne <- length(from)
    gaps <- c((as.numeric(from[-1]) - as.numeric(to[-length(to)])) <= maxdt, FALSE)
    gaps <- c(gaps[1], gaps[-1] | gaps[-ne])
    groups <- as.numeric(gaps)
    groups[!gaps] <- 2:(sum(!gaps) + 1)
    temp <- rle(groups)
    groups <- rep(1:length(temp$values), temp$lengths)
    dt <- data.table::data.table(from, to, groups)[, .(from = from[1], to = to[.N]), by = groups][, .(from, to)]
  } else {
    dt <- data.table::data.table(from, to)
  }
  # Return result as data.frame
  as.data.frame(dt)
}

#' Generate ranges from a table
#'
#' Like \code{\link{colranges}}, but vectorized over a table.
#'
#' @param .data (data.frame)
#' @param time_col (character) Name of temporal column.
#' @param group_col (character) Name of grouping column.
#' @param maxdt (numeric) Maximum difference in \code{time_col} to consider as continuous.
#' @export
tblranges <- function(.data, time_col, group_col = NULL, maxdt = Inf) {
  variables = setdiff(names(.data), c(time_col, group_col))
  DT <- data.table::as.data.table(.data)
  lapply(variables, function(var) {
    DT[, colranges(get(time_col), maxdt = maxdt, x = get(var)), by = group_col]
  }) %>%
    set_names(variables) %>%
    data.table::rbindlist(idcol = "variable") %>%
    as.data.frame()
}

#' Coerce range endpoints to numeric
#'
#' @param ranges Ranges.
#' @export
ranges_to_numeric <- function(ranges) {
  ranges[["from"]] %<>% as.numeric()
  ranges[["to"]] %<>% as.numeric()
  ranges
}

#' Coerce range endpoints to date-time
#'
#' @param ranges Ranges.
#' @param ... Arguments passed to \code{\link{as_time}}.
#' @export
ranges_to_datetime <- function(ranges, ...) {
  ranges[["from"]] %<>% cgr::as_time(...)
  ranges[["to"]] %<>% cgr::as_time(...)
  ranges
}

#' Find interval indices
#'
#' @param pos (numeric)
#' @param breaks (numeric) Sorted (weakly) increasingly.
#' @export
find_interval <- function(pos, breaks) {
  ind <- findInterval(pos, breaks, rightmost.closed = TRUE)
  N <- length(breaks) - 1
  ind[ind == 0 | ind > N] <- NA
  ind
}

#' Sample variables by interval
#'
#' @param .data (data.frame) Variables to sample.
#' @param pos (numeric)
#' @param breaks (numeric) Sorted (weakly) increasingly.
#' @param fun (function|character) Sampling function
#' @param ... Additional arguments passed to \code{fun}
#' @export
#' @examples
#' df <- data.frame(
#'   t = as_time(0:9),
#'   x = 0:9,
#'   y = 10:19
#' )
#' breaks <- as_time(c(0,5,10))
#' sample_interval(df[, c("x", "y")], df$t, breaks, fun = mean, na.rm = TRUE)
#' sample_interval(df[, c("x", "y")], df$t, breaks, fun = max, na.rm = TRUE)
sample_interval <- function(.data, pos, breaks, fun = mean, ...) {
  if (data.table::is.data.table(.data)) {
    temp <- data.table::copy(.data)
  } else {
    temp <- data.table::as.data.table(.data)
  }
  temp2 <- temp[
    , `..bin..` := find_interval(pos, breaks)
  ][
    , lapply(.SD, match.fun(fun), ...), by = "..bin..", .SDcols = names(.data)
  ]
  midpoints <- data.table::data.table(
    `..bin..` = 1:(length(breaks) - 1),
    t = breaks[-length(breaks)] + diff(breaks) / 2
  )
  merged <- merge(midpoints, temp2, by = "..bin..", all.x = TRUE)
  as_table(merged[, `..bin..` := NULL], type = .data)
}
