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

  res1 <- get_log_density(Z1, keep)
  res2 <- get_log_density(Z2, keep)

  if (res1$log_density < res2$log_density) {
    num_pts <- nrow(Z1)
    side_lengths <- res1$sval * sqrt(12)
    p <- res1$p
  } else {
    num_pts <- nrow(Z2)
    side_lengths <- res2$sval * sqrt(12)
    p <- res2$p
  }

  counts = vector(length=b)

  if (parallel) {
    num_cores <- parallel::detectCores()

    counts <- parallel::mclapply(1:b, function(i) {
      X <- matrix(nrow=num_pts, ncol=p)
      for (j in 1:p) {
        X[,j] <- runif(num_pts, min=-side_lengths[j]/2, max=side_lengths[j]/2)
      }

      mst <- get_mst(dist(X))

      count <- 0
      for (k in 1:igraph::ecount(mst)) {
        head <- X[igraph::head_of(mst, k),]
        tail <- X[igraph::tail_of(mst, k),]

        if (sign(head[1]) != sign(tail[1])) count <- count + 1
      }

      counts[i] <- count
    }, mc.cores=num_cores-1)

    counts <- unlist(counts)
  }
  else {
    for (i in 1:b) {
      X <- matrix(nrow=num_pts, ncol=p)
      for (j in 1:p) {
        X[,j] <- runif(num_pts, min=-side_lengths[j]/2, max=side_lengths[j]/2)
      }

      mst <- get_mst(dist(X))

      count <- 0
      for (k in 1:igraph::ecount(mst)) {
        head <- X[igraph::head_of(mst, k),]
        tail <- X[igraph::tail_of(mst, k),]

        if (sign(head[1]) != sign(tail[1])) count <- count + 1
      }

      counts[i] <- count
    }
  }

  counts
}

#' @rdname mst_test
mst_test_brush <- function(sim_crossings, num_crossings) {
  p_val <- mean(sim_crossings < num_crossings)

  print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
  print(paste0("Number of Crossings: ", num_crossings))
  print(paste0("p = ", round(p_val, 3)))
}
