
# `github.observatory` <img src="https://raw.githubusercontent.com/harell/github.observatory/master/pkgdown/logo.png" align="right" style="float:right; height:75px"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/harell/github.observatory/workflows/R-CMD-check/badge.svg)](https://github.com/harell/github.observatory/actions)
[![Codecov test
coverage](https://codecov.io/gh/harell/github.observatory/branch/master/graph/badge.svg)](https://codecov.io/gh/harell/github.observatory?branch=master)

<!-- badges: end -->

<img src="https://www.tmt.org/assets/tmt_comp_background-f920515202d7fa2ce6dc8c485d53df5c2a9f5f3e4ff1414500ab8d5da629e49e.jpg" width="100%" style="display: block; margin: auto;" />

## Overview

Investigate which R users on Github are similar to each other, and
recommend them some R packages that they may like.

## Installation

You can install `github.observatory` by using:

``` r
install.packages("remotes")
remotes::install_github("harell/github.observatory")
```

Set AWS credentials

``` r
Sys.setenv(
    AWS_ACCESS_KEY_ID = "<your-access-key-id>",
    AWS_SECRET_ACCESS_KEY= "<your-secret>",
    AWS_REGION = "ap-southeast-2"
)
```
