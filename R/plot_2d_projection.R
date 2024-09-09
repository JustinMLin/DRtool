plot_2d_projection <- function(Z, path, cluster, id, dim, degree, slider, adjust) {
  # convert cluster to standard form
  cluster <- as.integer(as.factor(rank(cluster, ties.method="min")))

  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  ids <- unique(c(path_ids, which(cluster == first_label), which(cluster == last_label)))
  pts <- Z[ids,]
  cols <- cluster[ids]

  pca <- prcomp(pts, rank.=dim)
  X <- predict(pca, pts)
  var_explained <- sum(pca$sdev[1:dim]^2)/sum(pca$sdev^2)

  ref_mat <- matrix(nrow=length(path$vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] <- (1:length(path$vpath))^i
  }

  lambda <- CCA::estim.regul(X[1:length(path_ids),], ref_mat,
                             grid1=seq(0.001, 1, length.out=10), grid2=c(0),
                             plt=FALSE)$lambda1
  cc1 <- CCA::rcc(X[1:length(path_ids),], ref_mat, lambda, 0)

  projected_pts <- X %*% cc1$xcoef

  df <- data.frame(x=projected_pts[,1], y=projected_pts[,2], id=id[ids])

  color <- rep(1, length(path_ids))
  color[slider] <- 2

  p = ggplot2::ggplot(data=df, ggplot2::aes(x=x, y=y, label=id)) +
    ggplot2::geom_point(ggplot2::aes(color=factor(cols)), size=0.7) +
    {if (adjust != 0) ggplot2::geom_density2d(ggplot2::aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    ggplot2::scale_color_manual(values=scales::hue_pal()(length(unique(cluster)))[sort(unique(cols))]) +
    ggplot2::labs(title=paste0("CCA with degree ", degree), x="", y="", color="Class") +
    ggplot2::geom_segment(data=df[1:length(path_ids),],
                          ggplot2::aes(xend=lead(x), yend=lead(y)),
                          color = factor(color),
                          linewidth=0.3)

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
