get_subtree <- function(tree, points) {
  vertices <- if (length(points) == 1) points else unique(unlist(igraph::all_simple_paths(tree, from=points[1], to=points[-1], mode="out")))
  igraph::induced_subgraph(tree, vertices)
}

get_medoids <- function(Z_dist, cluster) {
  meds <- c()

  for (i in unique(cluster)) {
    cluster_dists <- usedist::dist_subset(Z_dist, which(cluster == i))
    pt_dists <- rowSums(as.matrix(cluster_dists))

    meds <- c(meds, as.numeric(names(which(pt_dists == min(pt_dists)))[1]))
  }

  meds
}

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
