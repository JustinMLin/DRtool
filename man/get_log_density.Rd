% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mst_test.R
\name{get_log_density}
\alias{get_log_density}
\title{Calculate the log density of a cluster}
\usage{
get_log_density(Z, keep)
}
\arguments{
\item{Z}{A numerical data matrix.}

\item{keep}{A numeric between 0 and 1. The proportion of variance to retain
when truncating dimensions.}
}
\value{
A list with the following components:
\item{log_density}{A numeric. The log density of the cluster.}

\item{sval}{A numerical vector. The singular values retained after
truncation}

\item{p}{A numeric. The number of dimensions retained after truncation.}
}
\description{
The density of a cluster is estimated to be the volume of a hyperrectangle
with side lengths equal to sqrt(12) times the singular values of the
cluster. This is because a uniform distribution on the hyperrectangle will
have variances equal to the variances of the cluster in the directions of the
principal components.
}
