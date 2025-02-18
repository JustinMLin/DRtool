#' Compute the projection of the selected points using the PCA/rCCA method
#'
#' @description
#' The selected points are first projected down to `dim` dimensions using PCA.
#' They are then projected down to two dimensions using rCCA. The comparison
#' matrix is the design matrix for a polynomial of degree `degree`. The points
#' are selected in one of two ways:
#' * `get_projection()` selects the path points along with all points belonging
#' to the same cluster as one of path's endpoints.
#' * `get_projection_brush()` selects the path points along with the points
#' belonging to the two groups selected by the user.
#'
#' @param Z A numerical matrix containing the high-dimensional data.
#' @param path A named list returned by [get_shortest_path()].
#' @param g1,g2 A numerical vector of the indices of the points in each group.
#' @param cluster A vector of length `nrow(Z)` with cluster labels.
#' @param dim A numeric greater than or equal to 2.
#' @param degree A positive numeric.
#'
#' @returns A list with the following components:
#' \item{projected_pts}{A numerical matrix with two columns containing the
#' projected coordinates.}
#'
#' \item{ids}{A numerical vector containing the indices of the selected points.}
#'
#' \item{path_ids}{A numerical vector containing the indices of the path points.}
#'
#' \item{var_explained}{A numeric. The proportion of variance retained during
#' the PCA projection step.}
get_projection <- function(Z, path, cluster, dim, degree) {
  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  ids <- unique(c(path_ids, which(cluster == first_label), which(cluster == last_label)))
  pts <- Z[ids,]

  pca <- prcomp(pts, rank.=dim)
  X <- predict(pca, pts)
  var_explained <- sum(pca$sdev[1:dim]^2)/sum(pca$sdev^2)

  ref_mat <- matrix(nrow=length(path$vpath), ncol=degree)
  for (i in 1:degree) {
    ref_mat[,i] <- (1:length(path$vpath))^i
  }

  if (length(path_ids) == 2) {
    proj_pca = prcomp(path_pts, rank.=2)
    projected_pts = predict(proj_pca, pts)
  } else {
    invisible(capture.output(lambda <- CCA::estim.regul(X[1:length(path_ids),], ref_mat,
                                                        grid1=10^(-3:2), plt=FALSE)$lambda1))
    cc1 <- CCA::rcc(X[1:length(path_ids),], ref_mat, lambda, 0)
    projected_pts = X %*% cc1$xcoef
  }

  list(projected_pts = projected_pts, ids = ids, path_ids = path_ids, var_explained = var_explained)
}
