#' @rdname count_crossings
count_crossings_brush <- function(mst, g1, g2) {
  cluster = rep(3, igraph::vcount(mst))
  cluster[g1] = 1
  cluster[g2] = 2

  simp_mst <- simplify_sub_mst(mst, g1, g2, cluster)

  count <- 0
  for (i in 1:igraph::ecount(simp_mst)) {
    head <- igraph::head_of(simp_mst, i)
    tail <- igraph::tail_of(simp_mst, i)

    head_label <- igraph::V(simp_mst)$cluster[head]
    tail_label <- igraph::V(simp_mst)$cluster[tail]

    if (setequal(c(head_label, tail_label), c(1,2))) count <- count + 1
  }

  for (node in which(igraph::V(simp_mst)$group == "outside")) {
    neighbors <- igraph::neighbors(simp_mst, node)
    g1_neighbors <- sum(igraph::V(simp_mst)$group[neighbors] == "g1")
    g2_neighbors <- sum(igraph::V(simp_mst)$group[neighbors] == "g2")

    if (min(g1_neighbors, g2_neighbors) > 0) count <- count + max(g1_neighbors, g2_neighbors)
  }

  count
}

#' @rdname sim_crossings
sim_crossings_brush <- function(Z, g1, g2, b, keep=0.7, parallel=FALSE) {
  Z1 <- Z[g1,]
  Z2 <- Z[g2,]

  sim_crossings_inner(Z1, Z2, b, keep, parallel)
}

#' @rdname mst_test
mst_test_brush <- function(sim_crossings, num_crossings) {
  p_val <- mean(sim_crossings < num_crossings)

  print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
  print(paste0("Number of Crossings: ", num_crossings))
  print(paste0("p = ", round(p_val, 3)))
}
