#' @importFrom magrittr "%>%"
plot_2d_projection <- function(mst, cluster, projected_pts, ids, path_ids, var_explained, degree, slider, adjust, show_all_edges) {
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

  p <- ggplot2::ggplot(df) +
    ggnetwork::geom_nodes(ggplot2::aes(x=x, y=y, fill=color, label=id), size=0.8, color="transparent", shape=21) +
    ggplot2::scale_fill_manual(values=scales::hue_pal()(length(unique(cluster)))[sort(unique(cluster[ids]))]) +
    ggnetwork::geom_edges(data=df[df$edge_type == "path",],
                          ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color=path_color), linewidth=0.3) +
    ggplot2::scale_color_manual(values=c("black", "red")) +
    {if (show_all_edges == "Show") ggnetwork::geom_edges(data=df[df$edge_type == "non-path",],
                                                         ggplot2::aes(x=x, y=y, xend=xend, yend=yend), linewidth=0.3, alpha=0.2)} +
    {if (adjust != 0) ggplot2::geom_density2d(ggplot2::aes(x=x, y=y), inherit.aes=FALSE, adjust=adjust, alpha=.5)} +
    ggplot2::labs(title=paste0("CCA with degree ", degree), x="", y="", color="Class")

  # ggplotly doesn't translate geom_text, add annotation later
  list(p=p, var_explained=var_explained)
}
