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

    count <- count + min(g1_neighbors, g2_neighbors)
  }

  count
}

#' @rdname sim_crossings
sim_crossings_brush <- function(Z, g1, g2, cluster, b, keep=0.9, parallel=FALSE) {
  Z1 <- Z[c(g1, g2),]

  n <- dim(Z1)[1]

  var_ratio <- prcomp(Z1)$sdev^2
  var_ratio <- var_ratio[cumsum(var_ratio)/sum(var_ratio) < keep]

  counts = vector(length=b)

  if (parallel) {
    num_cores <- parallel::detectCores()

    counts <- parallel::mclapply(1:b, function(i) {
      X <- matrix(nrow=n, ncol=length(var_ratio))
      for (j in 1:length(var_ratio)) {
        X[,j] <- runif(n, min=-var_ratio[j]/2, max=var_ratio[j]/2)
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
      X <- matrix(nrow=n, ncol=length(var_ratio))
      for (j in 1:length(var_ratio)) {
        X[,j] <- runif(n, min=-var_ratio[j]/2, max=var_ratio[j]/2)
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
