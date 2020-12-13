<!-- badges: start -->
[![Travis build status](https://travis-ci.com/juanhillon/STAT302Package.svg?branch=master)](https://travis-ci.com/juanhillon/STAT302Package)
[![Codecov test coverage](https://codecov.io/gh/juanhillon/STAT302Package/branch/master/graph/badge.svg)](https://codecov.io/gh/juanhillon/STAT302Package?branch=master)
<!-- badges: end -->

## Installation

To install and load STAT302Package, use the code below.

``` r
# install.packages("devtools")
devtools::install_github("juanhillon/STAT302Package")
library(STAT302Package)
```

## Use

The vignette demonstrates example usage of all main functions.  You can see the vignette by using the following code :

``` r
# install.packages("devtools")
devtools::install_github("juanhillon/STAT302Package", build_vignette = TRUE, build_opts = c())
library(STAT302Package)
# Use this to view the vignette in the corncob HTML help
help(package = "STAT302Package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302Package")
```
