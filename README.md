
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggoce

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paleolimbot/ggoce/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/ggoce/actions)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/ggoce/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/ggoce?branch=master)
<!-- badges: end -->

The goal of ggoce is to provide a minimal set of
[ggplot2](https://ggplot2.tidyverse.org) components to make
publication-quality plots of [oce](https://dankelley.github.io/oce/)
objects.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/ggoce")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggplot2)
library(ggoce)
data(ctd, package = "oce")

ggplot(ctd, aes(x = salinity, y = depth)) +
  geom_point() +
  scale_y_reverse()
```

<img src="man/figures/README-example-1.png" width="100%" />
