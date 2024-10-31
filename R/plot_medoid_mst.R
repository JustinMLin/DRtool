#' Get the minimum subtree containing the specified vertices
#'
#' The subtree contains all of the specified vertices along with the shortest
#' paths between each pair of them. Uses [igraph::induced_subgraph()].
#'
#' @param tree An `igraph` object
#' @param points A numeric vector of vertex indices.
#'
#' @returns An `igraph` object.
get_subtree <- function(tree, points) {
  vertices <- if (length(points) == 1) points else unique(unlist(igraph::all_simple_paths(tree, from=points[1], to=points[-1], mode="out")))
  igraph::induced_subgraph(tree, vertices)
}

#' Get the medoid of each cluster
#'
#' Returns the indices of the medoid of each cluster in the data set.
#'
#' @param Z_dist A `dist` object or numerical distance matrix.
#' @param cluster A vector of length `nrow(as.matrix(dist))`.
#'
#' @returns A numerical vector of medoid indices.
get_medoids <- function(Z_dist, cluster) {
  meds <- c()

  for (i in unique(cluster)) {
    cluster_dists <- usedist::dist_subset(Z_dist, which(cluster == i))
    pt_dists <- rowSums(as.matrix(cluster_dists))

    meds <- c(meds, as.numeric(names(which(pt_dists == min(pt_dists)))[1]))
  }

  meds
}

#' Constructs the minimal subtree containing all cluster medoids
#'
#' Uses [get_medoids()] to calculate cluster medoids, then uses [get_subtree()]
#' to get the minimal subtree.
#'
#' @param Z_dist A `dist` object or numerical distance matrix.
#' @param mst An `igraph` object.
#' @param cluster A vector of length `nrows(as.matrix(Z_dist))`.
#'
#' @returns An `igraph` object. The returned tree has vertex attribute `medoid`
#' denoting which cluster each point is the medoid of. For non-medoid points,
#' this attribute is NA.
get_medoid_mst <- function(Z_dist, mst, cluster) {
  meds <- get_medoids(Z_dist, cluster)

  tree <- get_subtree(mst, meds)
  igraph::V(tree)$medoid <- NA

  cluster_uniq <- unique(cluster)
  for (i in 1:length(cluster_uniq)) {
    igraph::V(tree)$medoid[which(igraph::V(tree)$name == meds[i])] <- cluster_uniq[i]
  }

  tree
}

#' Overlays the minimal subtree containing all cluster medoids
#'
#' Uses [get_medoid_mst()] to construct the minimal subtree containing the
#' cluster medoids, then overlays it over `plot`.
#'
#' @param plot A `ggplot` object.
#' @param df A data frame containing columns named `x` and `y` containing the
#' coordinates of the points in the plot.
#' @param Z_dist A `dist` object or numerical distance matrix.
#' @param tree An `igraph` object.
#'
#' @returns A `ggplot` object.
plot_medoid_mst <- function(plot, df, Z_dist, tree) {
  med_tree <- get_medoid_mst(Z_dist, tree, df$cluster)

  edge_matrix <- as.matrix(med_tree, matrix.type="edgelist")

  n <- length(edge_matrix[,1])

  if (n > 0) {
    for (i in 1:n) {
      plot <- plot + ggplot2::geom_path(data=df[as.numeric(edge_matrix[i,]),], color = "black")
    }
  }

  plot
}
