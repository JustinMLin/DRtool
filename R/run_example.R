#' @export
run_example <- function(cluster="kmeans") {
  if (cluster == "real") run_app(Z_pca, X, real_labels, id)
  else if (cluster == "kmeans") run_app(Z_pca, X, kmeans_cluster, id)
  else stop('cluster must be "real" or "kmeans"')
}
