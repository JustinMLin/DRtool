simplify_sub_mst <- function(mst, g1, g2, cluster) {
  labeled_mst <- mst
  V(labeled_mst)$group <- "outside"
  V(labeled_mst)$group[g1] <- "g1"
  V(labeled_mst)$group[g2] <- "g2"
  V(labeled_mst)$cluster <- cluster

  points <- c(g1, g2)

  # construct subtree
  vertices <- if (length(points) == 1) points else unique(unlist(igraph::all_simple_paths(mst, from=points[1], to=points[-1], mode="out")))
  ret <- igraph::induced_subgraph(labeled_mst, vertices)

  # simplify by replacing non-cluster vertices of degree 2
  while(TRUE) {
    node <- intersect(which(as.numeric(igraph::degree(ret)) == 2), which(V(ret)$group == "outside"))[1]

    if (is.na(node)) break

    neighbors <- igraph::neighbors(ret, v=node)
    if (length(neighbors) != 2) {
      stop("Chosen node does not have degree 2!")
    }

    total_weight <- sum(get_shortest_path(ret, neighbors[1], neighbors[2])$epath$weight)

    ret <- igraph::add_edges(ret, edges=neighbors, attr=list(weight = total_weight))
    ret <- ret - node
  }

  while(TRUE) {
    head_groups <- igraph::head_of(ret, 1:igraph::ecount(ret))$group
    tail_groups <- igraph::tail_of(ret, 1:igraph::ecount(ret))$group

    edge <- which((head_groups == "outside" & tail_groups == "outside") == TRUE)[1]

    if (is.na(edge)) {
      return(ret)
    }

    head <- as.numeric(head_of(ret, edge))
    tail <- as.numeric(tail_of(ret, edge))

    head_neighbors <- igraph::neighbors(ret, head)
    for (neighbor in as.numeric(head_neighbors[head_neighbors != tail])) {
      weight <- sum(get_shortest_path(ret, tail, neighbor)$epath$weight)
      ret <- igraph::add_edges(ret, edges=c(tail, neighbor), attr=list(weight = weight))
    }

    ret <- ret - head
  }
}
