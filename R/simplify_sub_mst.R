simplify_sub_mst <- function(mst, g1, g2) {
  labeled_mst <- mst
  V(labeled_mst)$group = "outside"
  V(labeled_mst)$group[g1] = "g1"
  V(labeled_mst)$group[g2] = "g2"

  points <- c(g1, g2)

  # construct subtree
  vertices <- if (length(points) == 1) points else unique(unlist(igraph::all_simple_paths(mst, from=points[1], to=points[-1], mode="out")))
  ret <- igraph::induced_subgraph(labeled_mst, vertices)

  # simplify by replacing non-cluster vertices of degree 2
  while(TRUE) {
    node <- intersect(which(as.numeric(igraph::degree(ret)) == 2), which(V(ret)$group == "outside"))[1]

    if (is.na(node)) {
      return(ret)
    }

    neighbors <- igraph::neighbors(ret, v=node)
    if (length(neighbors) != 2) {
      stop("Chosen node does not have degree 2!")
    }

    total_weight <- sum(get_shortest_path(ret, neighbors[1], neighbors[2])$epath$weight)

    ret <- igraph::add_edges(ret, edges=neighbors, attr=list(weight = total_weight))
    ret <- ret - node
  }
}
