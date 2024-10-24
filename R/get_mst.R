#' Compute the minimum spanning tree of set of points
#'
#' Returns an `igraph` object containing the minimum spanning tree. Constructs a
#' graph using [igraph::graph_from_adjacency_matrix()], then calculates the minimum
#' spanning tree using [igraph::mst()].
#'
#' @param Z_dist A `dist` object or a distance matrix.
#'
#' @returns An `igraph` object.
#'
#' @examples
#' Z <- matrix(rnorm(40, nrow=10, ncol=4))
#' Z_dist = dist(Z)
#' get_mst(Z_dist)
get_mst <- function(Z_dist) {
  Z_dist <- as.matrix(Z_dist)

  graph = igraph::graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  igraph::mst(graph)
}
