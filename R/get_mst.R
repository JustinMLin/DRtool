#' Compute the minimum spanning tree of set of points
#'
#' Returns an `igraph` object containing the minimum spanning tree. Constructs a
#' graph using [igraph::graph_from_adjacency_matrix()], then calculates the minimum
#' spanning tree using [igraph::mst()].
#'
#' @param Z_dist A `dist` object or distance matrix.
#'
#' @returns An `igraph` object.
get_mst <- function(Z_dist) {
  Z_dist <- as.matrix(Z_dist)

  graph = igraph::graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  igraph::mst(graph)
}
