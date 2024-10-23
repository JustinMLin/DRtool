#' Get medoid of a cluster
#'
#' `get_medoid()` returns the index of the medoid of the cluster of points
#' given by `g`.
#'
#' @param X_dist A `dist` object or distance matrix.
#' @param g A numerical vector of indices that denotes which group of points to get the
#' medoid of.
#'
#' @returns A numeric.
#'
#' @examples
#' X <- matrix(rnorm(40), nrow=10, ncol=4)
#' g <- c(1,2,3)
#' get_medoid(dist(X), g)
get_medoid <- function(X_dist, g) {
  if (length(g) == 0) stop("get_medoid: cluster does not exist!")

  if (length(g) == 1) { # check if cluster contains one point
    return(g)
  }
  else {
    total_dists <- rowSums(as.matrix(X_dist)[g,])

    medoid_id <- which(total_dists == min(total_dists))

    g[medoid_id]
  }
}
