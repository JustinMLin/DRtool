#' Computes shortest path between two points in a graph
#'
#' Wrapper for [igraph::shortest_paths()]. Returns the shortest path in `graph`
#' between vertices `from` and `to`.
#'
#' @details
#' The function assumes the inputted graph is a tree.
#'
#' @param graph An `igraph` object.
#' @param from A numeric.
#' @param to A numeric.
#'
#' @returns A named list with the `epath` and `vpath` returned by
#' [igraph::shortest_paths()]. See [igraph::distance_table()] for reference.
get_shortest_path <- function(graph, from, to) {
  path <- igraph::shortest_paths(graph, from, to, output="both")

  list(epath = path$epath[[1]],
       vpath = path$vpath[[1]])
}
