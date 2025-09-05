#' Launch dimension reduction tool examples
#'
#' The current implementation contains one example on the MNIST data set. For
#' each example, the tool can be launched visualizing the true class labels or
#' a computed k-means clustering.
#'
#' @param example A character string indicating which example data set to use.
#' See [MNIST_pca] and [Wong_high].
#' @param cluster A character string indicating the clustering to project onto
#' the low-dimensional embedding. Possible values of cluster are "real" and
#' "kmeans".
#' @param parallel A Boolean indicating whether parallel computing should be
#' used. The implementation uses [parallel::mclapply()], which is not available
#' on Windows.
#'
#' @returns None
#'
#' @examples
#' # parallel computing is not available on Windows
#' if (interactive()) {
#'   run_example(example="MNIST", cluster="real", parallel=FALSE)
#' }
#'
#' @export
run_example <- function(example=c("MNIST", "Wong"), cluster=c("real", "kmeans"), parallel=FALSE) {
  if (!(example %in% c("MNIST", "Wong"))) stop("Invalid example argument. Please see documentation.")
  if (!(cluster %in% c("real", "kmeans"))) stop("Invalid cluster argument. Please see documentation.")
  if (!(parallel %in% c(TRUE, FALSE))) stop("Parallel must be a Boolean.")

  if (example == "MNIST") {
    col_names <- sapply(1:300, function(i) paste0("PC", i))

    if (cluster == "real") run_app(MNIST_pca, MNIST_low, MNIST_labels, id=MNIST_id, col_names=col_names, parallel=parallel)
    else if (cluster == "kmeans") run_app(MNIST_pca, MNIST_low, MNIST_kmeans_cluster, id=MNIST_id, col_names=col_names, parallel=parallel)
  }
  else if (example == "Wong") {
    if (cluster == "real") run_app(Wong_high, Wong_low, Wong_organ_labels, meta_data=data.frame(Wong_cell_labels), parallel=parallel)
    else if (cluster == "kmeans") run_app(Wong_high, Wong_low, Wong_kmeans, meta_data=data.frame(Wong_cell_labels), parallel=parallel)
  }
}
