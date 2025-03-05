#' Launch dimension reduction tool examples
#'
#' The current implementation contains one example on the MNIST data set. For
#' each example, the tool can be launched visualizing the true class labels or
#' a computed k-means clustering.
#'
#' @param cluster A character string indicating the clustering to project onto
#' the low-dimensional embedding. Possible values of cluster are "real" or
#' "kmeans".
#' @param parallel A Boolean indicating whether parallel computing should be
#' used. The implementation uses [parallel::mclapply()], which is not available
#' on Windows.
#'
#' @returns None
#'
#' @examples
#' # parallel computing is not available on Windows
#' run_example(cluster="real", parallel=TRUE)
#' @export
run_example <- function(cluster=c("real", "kmeans"), parallel=FALSE) {
  if (!(cluster %in% c("real", "kmeans"))) stop("Invalid cluster argument. Please see documentation.")

  if (!(parallel %in% c(TRUE, FALSE))) stop("Parallel must be a Boolean.")

  col_names <- sapply(1:300, function(i) paste0("PC", i))

  if (cluster == "real") run_app(MNIST_pca, MNIST_low, MNIST_labels, id=MNIST_id, col_names=col_names, parallel=parallel)
  else if (cluster == "kmeans") run_app(MNIST_pca, MNIST_low, MNIST_kmeans_cluster, id=MNIST_id, col_names=col_names, parallel=parallel)
  else stop('cluster must be "real" or "kmeans"')
}
