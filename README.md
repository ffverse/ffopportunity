
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffexpectedpoints

*Models and Data for Expected Fantasy Points*

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Dev
status](https://img.shields.io/github/r-package/v/ffverse/ffexpectedpoints/main?label=dev&style=flat-square&logo=github)](https://ffexpectedpoints.ffverse.com/)
[![CRAN
status](https://www.r-pkg.org/badges/version/ffexpectedpoints)](https://CRAN.R-project.org/package=ffexpectedpoints)
<!-- badges: end -->

Downloads Expected Points data from ffverse repositories if
available, and otherwise builds up expected points data by applying
models to nflfastR play-by-play data.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("ffverse/ffexpectedpoints")
```

## Outline/Goals

    Main Functions: 
    ep_load(season = ?, week = NULL) # Tries to load specified data from GitHub repository.
    ep_build(season = NULL, week = NULL, rebuild = FALSE) # if rebuild is FALSE and season/week = NULL (i.e. the default), checks nflfastR's game repo for completed games, then checks DP GH to see what games are fully processed, then builds EP for the missing games. Otherwise, checks for the given season/week + if rebuild = TRUE then downloads that week's data and builds EP from that, or else just downloads EP from GH repo for that season/week.

    Subfunctions:
    .ep_check_missing_games(season, week) # Figures out what completed games don't have EP data in the repo
    .ep_download_nflfastr_data() # downloads required nflfastr data
    .ep_preprocess() # figures out running averages required for players.
    .ep_predict() # applies model to preprocessed data
