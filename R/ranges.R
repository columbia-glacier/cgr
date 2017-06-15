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
