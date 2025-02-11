count_crossings <- function(Z, path, cluster) {
  path_ids <- as.numeric(path$vpath)

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  Z1 <- Z[cluster %in% c(first_label, last_label),]

  mst <- get_mst(dist(Z1))

  new_cluster <- cluster[cluster %in% c(first_label, last_label)]

  count <- 0
  for (i in 1:igraph::ecount(mst)) {
    head_label <- new_cluster[igraph::head_of(mst, i)]
    tail_label <- new_cluster[igraph::tail_of(mst, i)]

    if (head_label != tail_label) count <- count + 1
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

get_path_endpts <- function(path, cluster, id) {
  path_ids <- as.numeric(path$vpath)

  same <- ifelse(cluster[path_ids[1]] == cluster[path_ids[length(path_ids)]], TRUE, FALSE)

  list(from=id[path_ids[1]], to=id[path_ids[length(path_ids)]], same=same)
}

mst_test <- function(sim_crossings, num_crossings, from, to, same) {
  if (same) print("Endpoints must belong to different classes! Use the Custom Clusters tab for custom clusters.")
  else {
    p_val <- mean(sim_crossings < num_crossings)

    print(paste0("From: ", from, ", To: ", to))
    print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
    print(paste0("Number of Crossings: ", num_crossings))
    print(paste0("p = ", round(p_val, 3)))
  }
}
