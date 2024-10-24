#' Overlays path onto existing plot
#'
#' `add_path()` returns a plot with the path, specified by `df` and `path`,
#' overlaid. `slider` can be used to highlight a specific segment of the plot.
#'
#' @param plot A `ggplot` object. Plot the path is to be overlaid upon.
#' @param df A data frame containing columns named `x` and `y` containing the
#' coordinates of the points in the plot.
#' @param A named list returned by `get_shortest_path()`.
#' @param slider A non-negative numeric denoting which segment in the path to
#' highlight. If `slider = 0`, no segment will be highlighted.
#'
#' @returns A `ggplot` object.
add_path <- function(plot, df, path, slider=0) {
  path_ids <- as.numeric(path$vpath)

  color <- rep(1, length(path_ids))
  color[slider] <- 2

  plot + ggplot2::geom_segment(data=df[path_ids,],
                               ggplot2::aes(xend=dplyr::lead(x), yend=dplyr::lead(y)),
                               color=factor(color),
                               linewidth=0.3)
}
