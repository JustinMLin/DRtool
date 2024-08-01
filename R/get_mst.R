get_mst <- function(Z_dist) {
  Z_dist <- as.matrix(Z_dist)

  graph = igraph::graph_from_adjacency_matrix(Z_dist, mode="undirected", weighted=TRUE)
  igraph::mst(graph)
}
