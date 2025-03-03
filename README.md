
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DRtool

<!-- badges: start -->
<!-- badges: end -->

DRtool provides an interactive tool used to analyze clusterings of
high-dimensional data. The minimum spanning tree is used in a
manifold-learning process to approximate the high-dimensional data’s
distribution. Various analytical plots provide a multi-faceted
perspective on the dimension reduction results to help distinguish
signal from byproducts of the dimension reduction process.

## Installation

You can install the development version of DRtool from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JustinMLin/DRtool")
```

## Example

To run an example on the
[MNIST](https://en.wikipedia.org/wiki/MNIST_database) data:

``` r
library(DRtool)

# parallel computing is not available on Windows
run_example(cluster="real", parallel=FALSE)
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
