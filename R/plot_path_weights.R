#' Get path weights
#'
#' Returns a vector of weights for a given path.
#'
#' @param path A named list returned by [get_shortest_path()].
#'
#' @returns A numerical vector of length `length(path$epath)`.
get_path_weights <- function(path) {
  epath <- path$epath

  if (!inherits(epath, "igraph.es")) {
    stop("get_path_weights: input is not of type igraph.es")
  }

  num_paths <- length(epath[])
  weights <- vector(length = num_paths)

  for (i in 1:num_paths) {
    weights[i] <- epath[[i]]$weight
  }

  weights
}

#' Draw a barplot of path weights
#'
#' Draws a barplot of the path weights. A specific bar may be highlighted.
#'
#' @param path A named list returned by [get_shortest_path()].
#' @param highlight A non-negative numeric determining which bar should be
#' highlighted. If `highlight = 0`, no bar will be highlighted.
#' @param max A numeric. The max value the bar heights should be scaled to.
#'
#' @returns A `ggplot` object.
plot_path_weights <- function(path, highlight=0, max) {
  path_weights <- get_path_weights(path)
  num_paths <- length(path_weights)

  fill <- rep(0, num_paths)
  fill[highlight] <- 1

  df <- data.frame(x = 1:length(path_weights), weight=path_weights, fill=factor(fill))
  q <- ggplot2::ggplot(df, ggplot2::aes(x=x, y=weight, fill=fill)) +
    ggplot2::geom_col(width=1, show.legend=FALSE) +
    ggplot2::labs(y = "Weight") +
    ggplot2::scale_fill_manual(values=c("black", "red")) +
    ggplot2::ylim(0, max) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  print(q)
}
