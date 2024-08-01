plot_2d_projection_brush <- function(Z, path, g1, g2, cluster, id, slider) {
  # convert cluster to standard form
  cluster <- as.integer(as.factor(rank(cluster, ties.method="min")))

  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]
  pca <- prcomp(path_pts, rank.=2)
  var_explained <- sum(pca$sdev[1:2]^2)/sum(pca$sdev^2)

  ids <- unique(c(path_ids, g1, g2))
  pts <- Z[ids,]
  cols <- cluster[ids]

  projected_pts <- predict(pca, newdata=pts)

  df <- data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])

  color <- rep(1, length(path_ids))
  color[slider] <- 2

  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=x, y=y, label=id)) +
    ggplot2::geom_point(ggplot2::aes(x=x, y=y, color=factor(cols)), size=0.7) +
    ggplot2::scale_color_manual(values=scales::hue_pal()(length(unique(cluster)))[sort(unique(cols))]) +
    ggplot2::labs(x="", y="", color="Class") +
    ggplot2::geom_segment(data=df[1:length(path_ids),],
                          ggplot2::aes(xend=lead(x), yend=lead(y)),
                          color = factor(color),
                          linewidth=0.3)

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
