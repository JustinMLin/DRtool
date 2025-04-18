% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mst_test.R
\name{sim_crossings_inner}
\alias{sim_crossings_inner}
\title{Internal function for \code{\link[=sim_crossings]{sim_crossings()}} and \code{\link[=sim_crossings_brush]{sim_crossings_brush()}}}
\usage{
sim_crossings_inner(Z1, Z2, b, keep, parallel)
}
\arguments{
\item{Z1}{A numerical matrix containing the data for group 1.}

\item{Z2}{A numerical matrix containing the data for group 2.}

\item{b}{A positive numeric. The number of simulations to run.}

\item{keep}{A numeric between 0 and 1. The proportion of variance to retain
when truncating dimensions.}

\item{parallel}{A Boolean indicating whether parallel computing should be
used. The implementation uses \code{\link[parallel:mclapply]{parallel::mclapply()}}, which is not available
on Windows.}
}
\description{
Internal function used by \code{\link[=sim_crossings]{sim_crossings()}} and \code{\link[=sim_crossings_brush]{sim_crossings_brush()}} to
simulate the null distribution of crossings once and the two groups are
specified.
}
