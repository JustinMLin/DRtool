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
