get_shortest_path <- function(graph, from, to) {
  path <- igraph::shortest_paths(graph, from, to, output="both")

  list(epath = path$epath[[1]],
       vpath = path$vpath[[1]])
}
