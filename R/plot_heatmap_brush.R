plot_heatmap_brush <- function(Z, g1, g2) {
  both_groups <- intersect(g1, g2)
  just_g1 <- g1[-pmatch(g1, g2, nomatch=0)]
  just_g2 <- g2[-pmatch(g2, g1, nomatch=0)]

  pts <- prcomp(Z[c(just_g1, both_groups, just_g2),], rank.=100)$x
  group <- rep(c("Group 1", "Both", "Group 2"), times=c(length(just_g1),
                                                       length(both_groups),
                                                       length(just_g2)))
  ComplexHeatmap::Heatmap(pts, row_split=group,
                          show_row_dend=FALSE,
                          show_column_dend=FALSE, show_column_names=FALSE,
                          show_heatmap_legend=FALSE)
}
