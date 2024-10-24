#' Launch dimension reduction tool examples
#'
#' The current implementation contains one example on the MNIST data set. For
#' each example, the tool can be launched visualizing the true class labels or
#' a computed k-means clustering.
#'
#' @param cluster "real" or "kmeans".
#'
#' @examples
#' run_example("real")
#' @export
run_example <- function(cluster="kmeans") {
  if (cluster == "real") run_app(Z_pca, X, real_labels, id)
  else if (cluster == "kmeans") run_app(Z_pca, X, kmeans_cluster, id)
  else stop('cluster must be "real" or "kmeans"')
}
