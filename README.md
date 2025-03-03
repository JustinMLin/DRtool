
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DRtool

<!-- badges: start -->
<!-- badges: end -->

**DRtool** provides an interactive tool used to analyze clusterings of
high-dimensional data. The minimum spanning tree is used in a
manifold-learning process to approximate the high-dimensional data’s
distribution. Various analytical plots provide a multi-faceted
perspective on the dimension reduction results to help distinguish
signal from byproducts of the dimension reduction process.

## Installation

**DRtool** is available on CRAN and can be installed with:

``` r
install.packages("DRtool")
```

The development version is available on
[Github](https://github.com/JustinMLin/DRtool) and can be installed
with:

``` r
remotes::install_github("JustinMLin/DRtool")
```

## Example

To run an example on the
[MNIST](https://en.wikipedia.org/wiki/MNIST_database) data set:

``` r
library(DRtool)

# parallel computing is not available on Windows
run_example(cluster="real", parallel=TRUE)
```
