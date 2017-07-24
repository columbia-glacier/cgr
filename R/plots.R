#' Plot a compact timeline
#'
#' @param ranges (data.frame) See \code{\link{tblranges}}.
#' @param start,end (character) Name of columns containing temporal endpoints.
#' @param name (character) Name of column containing labels.
#' @param group (character) Name of column containing group names.
#' @param width (numeric) Line width.
#' @param color Line color.
#' @export
timeline <- function(ranges, start = "from", end = "to", name, group = NULL, width = 2, color = "darkgrey") {
  p <- ggplot2::ggplot(ranges, ggplot2::aes_string(x = start, y = name)) +
    ggplot2::geom_segment(
      ggplot2::aes_string(x = start, xend = end, y = name, yend = name),
      size = width, color = color, na.rm = TRUE
    )
  if (!is.null(group)) {
    p <- p +
      ggplot2::facet_grid(paste(paste(group, "~ .")), scales = "free_y", space = "free_y", drop = TRUE, switch = "y") +
      ggplot2::scale_y_discrete(position = "right", name = NULL) +
      ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 180))
  }
  p
}
