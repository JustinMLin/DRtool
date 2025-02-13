#' @rdname get_projection
get_projection_brush <- function(Z, path, g1, g2, cluster, dim, degree) {
  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  ids <- unique(c(path_ids, g1, g2))
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
