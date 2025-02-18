#' Plot meta data
#'
#' Plots the meta data for the selected groups. Categorical data is plotted
#' with pie charts, while numerical data is plotted with box plots.
#'
#' @param Z A numerical matrix containing the high-dimensional data.
#' @param path A named list returned by [get_shortest_path()].
#' @param g1,g2 A numerical vector of the indices of the points each group.
#' @param cluster A vector of length `nrow(Z)` with cluster labels.
#' @param meta_data A data frame with a number of rows equal to `nrow(Z)`. For
#' presenting extra meta data.
#' @param feature A string containing the name of the feature to be displayed.
#'
#' @returns A `ggplot` or `gtable` object.
meta_data_plot <- function(Z, path, cluster, meta_data, feature) {
  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  ids1 <- which(cluster == first_label)
  ids2 <- which(cluster == last_label)

  df <- data.frame(data = meta_data[[feature]][c(ids1,ids2)],
                   group = factor(rep(c(1,2), times=c(length(ids1), length(ids2)))))

  if (is.numeric(meta_data[[feature]])) {
    ggplot2::ggplot(df, ggplot2::aes(x=group, y=data, color=group)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(color = feature)
  }
  else {
    p1 <- ggplot2::ggplot(df[df$group == 1,], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::scale_fill_manual(limits=unique(df$data),
                                 values=scales::hue_pal()(length(unique(df$data))),
                                 drop=FALSE) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 1", fill=feature)

    p2 <- ggplot2::ggplot(df[df$group == 2,], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::scale_fill_manual(limits=unique(df$data),
                                 values=scales::hue_pal()(length(unique(df$data))),
                                 drop=FALSE) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 2", fill=feature)

    gridExtra::grid.arrange(p1, p2, nrow=1)
  }
}
