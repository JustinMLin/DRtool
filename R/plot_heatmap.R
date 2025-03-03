#' Construct the heatmap of high-dimensional points
#'
#' Creates a heatmap of the high-dimensional points. The rows are split
#' according to
#'  * class for [plot_heatmap()], and
#'  * user-defined groups for [plot_heatmap_brush()].
#'
#' @param Z A numerical matrix containing the high-dimensional data.
#' @param path A named list returned by [get_shortest_path()].
#' @param g1,g2 A numerical vector of the indices of the points each group.
#' @param cluster A vector of length `nrow(Z)` with cluster labels.
#' @param col_names A vector of length `col(Z)` with column names.
#'
#' @returns A `ComplexHeatmap` object.
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

  hcr <- hclust(dist(t(pts)))
  ddr <- as.dendrogram(hcr)

  if (first_label != last_label) {
    ddr <- reorder(ddr, colMeans(Z[first_ids,]) - colMeans(Z[last_ids,]))
  }

  suppressMessages(ComplexHeatmap::Heatmap(pts, row_split=group,
                                           cluster_columns = ddr,
                                           column_labels = col_names,
                                           show_row_dend=FALSE,
                                           show_column_dend=FALSE, show_column_names=FALSE,
                                           show_heatmap_legend=FALSE,
                                           row_gap = grid::unit(3, "mm")))
}
