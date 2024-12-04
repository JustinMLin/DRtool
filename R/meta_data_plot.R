meta_data_plot <- function(Z, path, cluster, meta_data, feature) {
  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  ids1 <- which(cluster == first_label)
  ids2 <- which(cluster == last_label)

  df <- data.frame(data = meta_data$feature[c(ids1,ids2)],
                   group = factor(c(rep(1, length(ids1)), rep(2, length(ids2)))))

  if (is.numeric(meta_data$feature)) {
    ggplot2::ggplot(df, ggplot2::aes(x=group, y=data, color=group)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(color = feature)
  }
  else {
    p1 <- ggplot2::ggplot(df[df$group == 1,], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 1")

    p2 <- ggplot2::ggplot(df[df$group == 2,], ggplot2::aes(x=factor(1), fill=data)) +
      ggplot2::geom_bar(stat="count", width=1, color="white") +
      ggplot2::coord_polar(theta="y", start=0) +
      ggplot2::theme_void() +
      ggplot2::labs(title="Group 2")

    gridExtra::grid.arrnage(p1, p2, nrow=1)
  }
}
