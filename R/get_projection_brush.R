#' @rdname get_projection
get_projection_brush <- function(Z, path, g1, g2, cluster, dim, degree) {
  path_ids <- as.numeric(path$vpath)
  path_pts <- Z[path_ids,]

  ids <- unique(c(path_ids, g1, g2))

  get_projection_inner(Z, path, dim, degree, path_ids, path_pts, ids)
}
