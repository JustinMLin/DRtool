add_path <- function(plot, df, path, slider=0) {
  path_ids <- as.numeric(path$vpath)

  color <- rep(1, length(path_ids))
  color[slider] <- 2

  plot + ggplot2::geom_segment(data=df[path_ids,],
                               ggplot2::aes(xend=dplyr::lead(x), yend=dplyr::lead(y)),
                               color=factor(color),
                               linewidth=0.3)
}
