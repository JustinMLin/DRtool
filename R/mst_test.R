count_crossings <- function(mst, path, cluster) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  simp_mst <- simplify_sub_mst(mst,
                               which(cluster == first_label),
                               which(cluster == last_label),
                               cluster)

  count <- 0
  for (i in 1:igraph::ecount(simp_mst)) {
    head <- igraph::head_of(simp_mst, i)
    tail <- igraph::tail_of(simp_mst, i)

    head_label <- igraph::V(simp_mst)$cluster[head]
    tail_label <- igraph::V(simp_mst)$cluster[tail]

    if (setequal(c(head_label, tail_label), c(first_label, last_label))) count <- count + 1
  }

  for (node in which(igraph::V(simp_mst)$group == "outside")) {
    neighbors <- igraph::neighbors(simp_mst, node)
    g1_neighbors <- sum(igraph::V(simp_mst)$group[neighbors] == "g1")
    g2_neighbors <- sum(igraph::V(simp_mst)$group[neighbors] == "g2")

    count <- count + min(g1_neighbors, g2_neighbors)
  }

  count
}

sim_crossings <- function(Z, path, cluster, b) {
  path_ids <- as.numeric(path$vpath)

  first_pt = path_ids[1]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  Z1 <- Z[cluster %in% c(first_label, last_label),]

  n <- dim(Z1)[1]
  p <- min(dim(Z1)[1], dim(Z))

  var_ratio <- prcomp(Z1)$sdev^2

  counts = vector(length=b)
  for (i in 1:b) {

    # for (j in 2:p) {
      # X <- cbind(X, matrix(runif(n, min=-var_ratio[j]/2, max=var_ratio[j]/2), ncol=1))
    # }

    X <- MASS::mvrnorm(n, mu=rep(0, p), Sigma=diag(var_ratio))

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

get_path_endpts <- function(path, cluster, id) {
  path_ids <- as.numeric(path$vpath)

  same <- ifelse(cluster[path_ids[1]] == cluster[path_ids[length(path_ids)]], TRUE, FALSE)

  list(from=id[path_ids[1]], to=id[path_ids[length(path_ids)]], same=same)
}

mst_test <- function(sim_crossings, num_crossings, endpts) {
  p_val <- mean(sim_crossings < num_crossings)

  print(paste0("From: ", endpts$from, ", To: ", endpts$to))
  print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
  print(paste0("Number of Crossings: ", num_crossings))
  print(paste0("p = ", round(p_val, 3)))
}
