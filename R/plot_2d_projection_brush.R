plot_2d_projection_brush <- function(Z, path, g1, g2, cluster, id, dim, degree, slider, adjust, color_choice) {
  # convert cluster to standard form
  cluster <- as.integer(as.factor(rank(cluster, ties.method="min")))

  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  ids <- unique(c(path_ids, g1, g2))
  pts <- Z[ids,]

  if (color_choice == "Original Coloring") {
    cols <- cluster[ids]
  }
  else if (color_choice == "Group Coloring") {
    group_path_ids <- match(path_ids, ids)
    g1_ids <- match(g1, ids)
    g2_ids <- match(g2, ids)

    cols <- sapply(1:length(ids), function(i) {
      dplyr::case_when(
        i %in% group_path_ids ~ "Path Point",
        i %in% g1_ids & !(i %in% g2_ids) ~ "Group 1",
        i %in% g2_ids & !(i %in% g1_ids) ~ "Group 2",
        i %in% g1_ids & i %in% g2_ids ~ "Group 1 and Group 2"
      )
    })
    cols <- factor(cols, levels=c("Path Point", "Group 1", "Group 2", "Group 1 and Group 2"))
  }

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

  p <- ggplot2::ggplot(data=df, ggplot2::aes(x=x, y=y, label=id)) +
    ggplot2::geom_point(ggplot2::aes(color=factor(cols)), size=0.7) +
    {if (adjust != 0) ggplot2::geom_density2d(ggplot2::aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    {if (color_choice == "Original Coloring") ggplot2::scale_color_manual(values=scales::hue_pal()(length(unique(cluster)))[sort(unique(cols))])} +
    {if (color_choice == "Group Coloring") ggplot2::scale_color_manual(values=c("black", "#F8766D", "#00BFC4", "#C77CFF"), drop=FALSE)} +
    ggplot2::labs(title=paste0("CCA with degree ", degree), x="", y="", color="Color") +
    ggplot2::geom_segment(data=df[1:length(path_ids),],
                          ggplot2::aes(xend=lead(x), yend=lead(y)),
                          color = factor(color),
                          linewidth=0.3)

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
