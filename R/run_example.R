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
  if (cluster == "real") run_app(MNIST_pca, MNIST_low, MNIST_labels, MNIST_id)
  else if (cluster == "kmeans") run_app(MNIST_pca, MNIST_low, MNIST_kmeans_cluster, MNIST_id)
  else stop('cluster must be "real" or "kmeans"')
}
