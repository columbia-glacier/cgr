#' Transform coordinates
#'
#' For transformations involving NAD27 coordinates ("+datum=NAD27"), custom parameters for Alaska (excluding Aleutian islands) are automatically applied ("+towgs84=-5,135,172"). See \url{http://web.archive.org/web/20130905025856/http://surveying.wb.psu.edu/sur351/DatumTrans/datum_transformations.htm}.
#'
#' @param xy Data.frame or numeric vector (coerced to single-row data.frame).
#' @param from Current proj.4 string.
#' @param to Target proj.4 string.
#' @param cols Column numbers or names specifying which \code{xy} columns are x and y coordinates.
#' @return Object of the same class and dimensions as \code{xy} with coordinates transformed in place.
#' @export
#' @family spatial functions
#' @examples
#' xy <- c(-147.25079, 61.18586, 505)
#' sp_transform(xy, "+proj=longlat +datum=WGS84", "+proj=utm +zone=6")
#' xy <- c(x = -147.25079, y = 61.18586, z = 505)
#' sp_transform(xy, "+proj=longlat +datum=WGS84", "+proj=utm +zone=6")
#' xy <- data.frame(z = 505, x = -147.25079, y = 61.18586)
#' sp_transform(xy, "+proj=longlat +datum=WGS84", "+proj=utm +zone=6", cols = c("x", "y"))
#' xy <- data.frame(x = c(NA, -147.25079), y = c(NA, 61.18586), z = 505)
#' sp_transform(xy, "+proj=longlat +datum=WGS84", "+proj=utm +zone=6")
sp_transform <- function(xy, from, to, cols = 1:2) {
  # Apply custom transformation parameters for NAD27
  from %<>%
    gsub("+datum=NAD27", "+ellps=clrk66 +towgs84=-5,135,172 +units=m +no_defs", .)
  to %<>%
    gsub("+datum=NAD27", "+ellps=clrk66 +towgs84=-5,135,172 +units=m +no_defs", .)
  # Coerce to data.frame
  is_vector <- is.vector(xy)
  txy <- xy
  if (is_vector) {
    txy %<>%
      t() %>%
      as.data.frame()
  }
  # Transform coordinates
  is_complete <- stats::complete.cases(txy[, cols])
  if (!any(is_complete)) {
    return(xy)
  }
  txy %<>%
    subset(is_complete) %>%
    sp::`coordinates<-`(cols) %>%
    sp::`proj4string<-`(sp::CRS(from)) %>%
    sp::spTransform(sp::CRS(to))
  if (is_vector) {
    xy[cols] <- unlist(txy@coords)
  } else {
    xy[is_complete, cols] <- txy@coords
  }
  xy
}
