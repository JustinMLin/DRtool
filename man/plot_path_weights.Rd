% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_path_weights.R
\name{plot_path_weights}
\alias{plot_path_weights}
\title{Draw a barplot of path weights}
\usage{
plot_path_weights(path, highlight = 0, max)
}
\arguments{
\item{path}{A named list returned by \code{\link[=get_shortest_path]{get_shortest_path()}}.}

\item{highlight}{A non-negative numeric determining which bar should be
highlighted. If \code{highlight = 0}, no bar will be highlighted.}

\item{max}{A numeric. The max value the bar heights should be scaled to.}
}
\value{
A \code{ggplot} object.
}
\description{
Draws a barplot of the path weights. A specific bar may be highlighted.
}
