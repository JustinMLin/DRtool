plot_heatmap <- function(Z, path, cluster, col_names) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  if (first_label == last_label) {
    ids <- which(cluster == first_label)
    group <- rep(first_label, length(ids))
  } else {
    first_ids = which(cluster == first_label)
    last_ids = which(cluster == last_label)

    ids <- c(first_ids, last_ids)
    group <- rep(c(first_label, last_label), times=c(length(first_ids), length(last_ids)))
  }

  pts <- Z[ids,]

  ComplexHeatmap::Heatmap(pts, row_split=group,
                          column_labels = col_names,
                          show_row_dend=FALSE,
                          show_column_dend=FALSE, show_column_names=FALSE,
                          show_heatmap_legend=FALSE,
                          row_gap = unit(3, "mm"))
}
