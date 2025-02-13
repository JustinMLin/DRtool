count_crossings_brush <- function(mst, g1, g2) {
  cluster = rep(3, igraph::vcount(mst))
  cluster[g1] = 1
  cluster[g2] = 2

  simp_mst <- simplify_sub_mst(mst, g1, g2, cluster)

  count <- 0
  for (i in 1:igraph::ecount(simp_mst)) {
    head <- igraph::head_of(simp_mst, i)
    tail <- igraph::tail_of(simp_mst, i)

    head_label <- V(simp_mst)$cluster[head]
    tail_label <- V(simp_mst)$cluster[tail]

    if (setequal(c(head_label, tail_label), c(1,2))) count <- count + 1
  }

  for (node in which(V(simp_mst)$group == "outside")) {
    neighbors <- igraph::neighbors(simp_mst, node)
    g1_neighbors <- sum(V(simp_mst)$group[neighbors] == "g1")
    g2_neighbors <- sum(V(simp_mst)$group[neighbors] == "g2")

    count <- count + min(g1_neighbors, g2_neighbors)
  }

  count
}

sim_crossings_brush <- function(Z, g1, g2, cluster, b) {
  Z1 <- Z[c(g1, g2),]

  n <- dim(Z1)[1]
  p <- min(dim(Z1)[1], dim(Z))

  var_ratio <- prcomp(Z1)$sdev^2

  counts = vector(length=b)
  for (i in 1:b) {

    X <- matrix(runif(n, min=-var_ratio[1]/2, max=var_ratio[1]/2), ncol=1)
    for (j in 2:p) {
      X <- cbind(X, matrix(runif(n, min=-var_ratio[j]/2, max=var_ratio[j]/2), ncol=1))
    }

    # X <- MASS::mvrnorm(n, mu=rep(0, p), Sigma=diag(var_ratio))

    mst <- get_mst(dist(X))

    count <- 0
    for (k in 1:igraph::ecount(mst)) {
      head <- X[igraph::head_of(mst, k),]
      tail <- X[igraph::tail_of(mst, k),]

      if (sign(head[1]) != sign(tail[1])) count <- count + 1
    }

    counts[i] <- count
  }

  counts
}

mst_test_brush <- function(sim_crossings, num_crossings) {
  p_val <- mean(sim_crossings < num_crossings)

  print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
  print(paste0("Number of Crossings: ", num_crossings))
  print(paste0("p = ", round(p_val, 3)))
}
