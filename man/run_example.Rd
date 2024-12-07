% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_example.R
\name{run_example}
\alias{run_example}
\title{Launch dimension reduction tool examples}
\usage{
run_example(cluster = "kmeans")
}
\arguments{
\item{cluster}{"real" or "kmeans".}
}
\description{
The current implementation contains one example on the MNIST data set. For
each example, the tool can be launched visualizing the true class labels or
a computed k-means clustering.
}
\examples{
run_example("real")
}
