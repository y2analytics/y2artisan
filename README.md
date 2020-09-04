
<!-- README.md is generated from README.Rmd. Please edit that file -->

# y2clerk

<!-- badges: start -->

<!-- badges: end -->

## Overview

`y2aristan` is the 3rd package in the y2analytics package series. It
relies heavily on the first two packages, `y2clerk` and `orderlabel`, so
be sure to have those installed first.

The goal of `y2artisan` is to quickly and easily create visualizations
relying on the infrastructure provided by either `ggplot2` or `mschart`.
Using one of those two bases, you can create the following types of
charts with pre-set defaults: `*_grouped_y2` (bar) `*_histo_y2`
`*_line_y2` `*_pie_y2` `*_single_y2` (bar) `*_stacked_y2` (bar)

You can also add these charts to PowerPoint through the `add*c_y2`
functions, or look at open ended questions with `openend_y2` and
`wordcloud_y2`. And don’t forget to add in all the Qualtrics’ color
defaults with `add_colors_y2`.

## Installation

You can install the released version of y2clerk from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("y2artisan")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nick-moffitt/y2artisan")
```

## Examples

Below you will find a few basic examples which show you how to quickly
get a frequencies table with `freqs()`:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(y2clerk)
library(orderlabel)
library(y2artisan)
library(officer)

# Create a data frame for the charts
frequencies <- mtcars %>%
  y2clerk::freqs(carb) %>%
  orderlabel::order_label(inherent_order_label = T)

# ggplot2 based charts
chart <- gg_single_y2(font_family = "sans")
#> Using `n` as weighting variable
#> ℹ Quiet this message with `wt = n` or count rows with `wt = 1`

# mschart based charts
color_settings <- list('blue')
text_settings<- list('result' = fp_text(font.size = 20))
#chart <- ms_single_y2()
```

## Help

If you have issues using y2artisan, please post your issue on
[GitHub](https://github.com/nick-moffitt/y2artisan/issues) along with a
minimal reproducible example. We will do our best to address your issues
and get them fixed for the next version of y2artisan
