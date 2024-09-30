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

  lambda <- CCA::estim.regul(X[1:length(path_ids),], ref_mat,
                             grid1=seq(0.001, 1, length.out=10), grid2=c(0),
                             plt=FALSE)$lambda1
  cc1 <- CCA::rcc(X[1:length(path_ids),], ref_mat, lambda, 0)

  list(projected_pts = X %*% cc1$xcoef, ids = ids, path_ids = path_ids, var_explained = var_explained)
}
