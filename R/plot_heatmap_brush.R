#' @rdname plot_heatmap
plot_heatmap_brush <- function(Z, g1, g2, col_names) {
  both_groups <- intersect(g1, g2)
  just_g1 <- g1[!(g1 %in% g2)]
  just_g2 <- g2[!(g2 %in% g1)]

  pts <- Z[c(just_g1, both_groups, just_g2),]
  group <- factor(rep(c("Group 1", "Both", "Group 2"), times=c(length(just_g1),
                                                       length(both_groups),
                                                       length(just_g2))),
                  levels=c("Group 1", "Both", "Group 2"))

  hcr <- hclust(dist(t(pts)))
  ddr <- as.dendrogram(hcr)

  if (length(just_g1) > 0 & length(just_g2) > 0) {
    ddr <- reorder(ddr, colMeans(Z[just_g1,]) - colMeans(Z[just_g2,]))
  }

  ComplexHeatmap::Heatmap(pts, row_split=group,
                          cluster_columns = ddr,
                          column_labels = col_names,
                          cluster_row_slices=FALSE,
                          show_row_dend=FALSE,
                          show_column_dend=FALSE, show_column_names=FALSE,
                          show_heatmap_legend=FALSE,
                          row_gap = grid::unit(3, "mm"))
}
