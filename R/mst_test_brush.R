count_crossings_brush <- function(Z, g1, g2, cluster) {
  Z1 <- Z[c(g1, g2),]

  mst <- get_mst(dist(Z1))

  new_cluster <- cluster[c(g1, g2)]

  count <- 0
  for (i in 1:igraph::ecount(mst)) {
    head_label <- new_cluster[igraph::head_of(mst, i)]
    tail_label <- new_cluster[igraph::tail_of(mst, i)]

    if (head_label != tail_label) count <- count + 1
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
