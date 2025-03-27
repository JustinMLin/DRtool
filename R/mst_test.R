#' Count the number of crossings between two clusters in the inputted tree
#'
#' Given two clusters and a tree, a simplified subtree is created (see
#' [simplify_sub_mst()]). The number of crossings between the two clusters in
#' the simplified subtree is then counted.
#'
#' @param mst An `igraph` object.
#' @param path A named list returned by [get_shortest_path()].
#' @param cluster A vector of length `vcount(mst)` with cluster labels.
#' @param g1,g2 A numerical vector of indices of the points in each group.
#'
#' @returns A numeric.
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

    if (min(g1_neighbors, g2_neighbors) > 0) count <- count + max(g1_neighbors, g2_neighbors)
  }

  count
}

#' Calculate the log density of a cluster
#'
#' The density of a cluster is estimated to be the volume of a hyperrectangle
#' with side lengths equal to sqrt(12) times the singular values of the
#' cluster. This is because a uniform distribution on the hyperrectangle will
#' have variances equal to the variances of the cluster in the directions of the
#' principal components.
#'
#' @param Z A numerical data matrix.
#' @param keep A numeric between 0 and 1. The proportion of variance to retain
#' when truncating dimensions.
#'
#' @returns A list with the following components:
#' \item{log_density}{A numeric. The log density of the cluster.}
#'
#' \item{sval}{A numerical vector. The singular values retained after
#' truncation}
#'
#' \item{p}{A numeric. The number of dimensions retained after truncation.}
get_log_density <- function(Z, keep) {
  pca <- prcomp(Z)
  p <- which(cumsum(pca$sdev^2/sum(pca$sdev^2)) >= keep)[1]

  list(log_density = log(nrow(Z)) - sum(log(pca$sdev[1:p])),
       sval = pca$sdev[1:p],
       p=p)
}

#' Simulate the null distribution for number of crossings
#'
#' The MST is calculated on uniform sample drawn from a hyperrectangle. The
#' number of edges crossing a bisecting hyperplane is recorded, and this
#' process is repeated multiple times. The dimensions of the hyperrectangle are
#' chosen to match the data.
#'
#' @param Z A numerical matrix containing the high-dimensional data.
#' @param path A named list returned by [get_shortest_path()].
#' @param cluster A vector of length `nrow(Z)` with cluster labels.
#' @param b A positive numeric. The number of simulations to run.
#' @param keep A numeric between 0 and 1. The proportion of variance to retain
#' when truncating dimensions.
#' @param parallel A Boolean indicating whether parallel computing should be
#' used. The implementation uses [parallel::mclapply()], which is not available
#' on Windows.
#' @param g1,g2 A numerical vector of indices of the points in each group.
#'
#' @returns A numerical vector.
sim_crossings <- function(Z, path, cluster, b, keep=0.7, parallel=FALSE) {
  path_ids <- as.numeric(path$vpath)

  first_pt = path_ids[1]

  first_label <- cluster[path_ids[1]]
  last_label <- cluster[path_ids[length(path_ids)]]

  Z1 <- Z[cluster == first_label,]
  Z2 <- Z[cluster == last_label,]

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

#' Retrieves the id's of the path endpoints.
#'
#' This function retrieves the id's of the path endpoints, which may be
#' different from their indices.
#'
#' @param path A named list returned by [get_shortest_path()].
#' @param cluster A vector with cluster labels.
#' @param id A vector of point id's.
#'
#' @returns A list with the following components:
#' \item{from}{The id of the first point in the path.}
#'
#' \item{to}{The id of the last point in the path.}
#'
#' \item{same}{A Boolean denoting whether the first and last points of the path
#' belong to the same cluster.}
get_path_endpts <- function(path, cluster, id) {
  path_ids <- as.numeric(path$vpath)

  same <- ifelse(cluster[path_ids[1]] == cluster[path_ids[length(path_ids)]], TRUE, FALSE)

  list(from=id[path_ids[1]], to=id[path_ids[length(path_ids)]], same=same)
}

#' Runs the MST test for two clusters
#'
#' Calculates a p-value from the simulated numbers of crossings and the actual
#' number of crossings, then prints the results.
#'
#' @param sim_crossings A numerical vector of simulated numbers of crossings.
#' @param num_crossings The actual number of crossings.
#' @param endpts A named list returned by [get_path_endpts()].
mst_test <- function(sim_crossings, num_crossings, endpts) {
  p_val <- mean(sim_crossings < num_crossings)

  print(paste0("From: ", endpts$from, ", To: ", endpts$to))
  print(paste0("Expected: ", round(mean(sim_crossings), 3), ", SE: ", round(sd(sim_crossings), 3)))
  print(paste0("Number of Crossings: ", num_crossings))
  print(paste0("p = ", round(p_val, 3)))
}
