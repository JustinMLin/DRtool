#' Constructs plot of projected points
#'
#' Uses the output of [get_projection()] or [get_projection_brush()] to
#' construct a plot of the projected points. Overlaid on the plot are the
#' projected path and estimated density (optional). In addition, the use may
#' choose to display all edges of the minimum spanning tree, not just the path.
#'
#' @param mst A `igraph` object.
#' @param cluster A vector of length `nrow(projected_pts)` containing the
#' cluster labels.
#' @param id A numerical vector containing the labels of the points.
#' @param g1 A numerical vector containing the indices of the points in group
#' one.
#' @param g2 A numerical vector containing the indices of the points in group
#' two.
#' @param projected_pts A numerical matrix with two columns.
#' @param ids A numerical vector containing the indices of the projected points.
#' @param path_ids A numerical vector containing the indices of the path points.
#' @param var_explained A numeric.
#' @param degree A numeric. The degree of the polynomial design matrix during
#' rCCA when computing the projection.
#' @param slider A numeric. Determines which segment of the path should be
#' highlighted in the plot. If `slider = 0`, no segment will be highlighted.
#' @param adjust A numeric. The bandwidth of the overlaid density estimation. If
#' `adjust = 0`, the estimated density will not be overlaid the plot.
#' @param show_all_edges "Show" or "Hide". If "Show", all edges of the minimum
#' spanning tree will be drawn.
#' @param color_choice "Original Coloring" or "Group Coloring". Determines if
#' the colors of the projected points are returned or if they're colored
#' according to the user-selected groups.
#'
#' @returns A named list containing the plot `p` and the variance explained
#' `var_explained`.
#'
#' @details
#' The variance explained is not written on the plot because it would get
#' removed by [plotly::ggplotly()]. Instead the variance explained is passed along, then
#' added after the plot is converted into a `plotly` object.
#' @importFrom magrittr "%>%"
plot_2d_projection <- function(mst, cluster, id, projected_pts, ids, path_ids, var_explained, degree, slider, adjust, show_all_edges) {
  #induced_subgraph re-orders vertices by vids, low to high
  plotting_graph <- igraph::induced_subgraph(mst, vids=ids) %>%
    igraph::set_vertex_attr("color", value=factor(cluster[sort(ids)])) %>%
    igraph::set_vertex_attr("id", value=id[sort(ids)])

  num_edges <- igraph::ecount(plotting_graph)
  edge_mat <- igraph::as_edgelist(plotting_graph, names=FALSE)
  path_color <- rep(NA, num_edges)
  edge_type <- rep("non-path", num_edges)

  for (i in 1:num_edges) {
    index_in_path_ids <- match(igraph::V(plotting_graph)$name[edge_mat[i,]], path_ids)
    if (!any(is.na(index_in_path_ids))) {
      if (min(index_in_path_ids) == slider) {
        path_color[i] <- 2
      } else {
        path_color[i] <- 1
      }
      edge_type[i] <- "path"
    }
  }

  plotting_graph <- plotting_graph %>%
    igraph::set_edge_attr("edge_type", value=factor(edge_type)) %>%
    igraph::set_edge_attr("path_color", value=factor(path_color))

  df <- ggnetwork::ggnetwork(plotting_graph, layout=projected_pts[order(ids),1:2])

  #ggnetwork overlays multiple nodes so density must be constructed from projected_pts
  #ggnetwork scales data fit in [0,1] x [0,1]
  m_hor = (max(projected_pts[,1]) - min(projected_pts[,1]))^{-1}
  b_hor = min(projected_pts[,1]) * -m_hor

  m_vert = (max(projected_pts[,2]) - min(projected_pts[,2]))^{-1}
  b_vert = min(projected_pts[,2]) * -m_vert

  df_points = data.frame(x=m_hor*projected_pts[,1] + b_hor,
                         y=m_vert*projected_pts[,2] + b_vert)

  p <- ggplot2::ggplot(df) +
    suppressWarnings(ggnetwork::geom_nodes(ggplot2::aes(x=x, y=y, fill=color, label=id), size=0.8, color="transparent", shape=21)) +
    ggplot2::scale_fill_manual(values=scales::hue_pal()(length(unique(cluster)))[sort(unique(cluster[ids]))]) +
    ggnetwork::geom_edges(data=df[df$edge_type == "path",],
                          ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color=path_color), linewidth=0.3) +
    ggplot2::scale_color_manual(values=c("black", "red")) +
    {if (show_all_edges == "Show") ggnetwork::geom_edges(data=df[df$edge_type == "non-path",],
                                                         ggplot2::aes(x=x, y=y, xend=xend, yend=yend), linewidth=0.3, alpha=0.2)} +
    {if (adjust != 0) ggplot2::geom_density2d(data=df_points, ggplot2::aes(x=x, y=y), adjust=adjust, alpha=.5)} +
    ggplot2::labs(title=paste0("CCA with degree ", degree), x="", y="", color="Class")

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
