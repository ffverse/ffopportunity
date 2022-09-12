
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffopportunity <a href='https://ffopportunity.ffverse.com'><img src='man/figures/logo.svg' align="right" width="25%" min-width="120px"/></a>

*Models and Data for Expected Fantasy Points*

<!-- badges: start -->

[![CRAN
status](https://img.shields.io/cran/v/ffopportunity?style=flat-square&logo=R&label=CRAN)](https://CRAN.R-project.org/package=ffopportunity)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Dev
status](https://img.shields.io/github/r-package/v/ffverse/ffopportunity/main?label=dev&style=flat-square&logo=github)](https://ffopportunity.ffverse.com/)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/ffverse/ffopportunity?label=codecov&style=flat-square&logo=codecov)](https://app.codecov.io/gh/ffverse/ffopportunity?branch=main)
[![nflverse
discord](https://img.shields.io/discord/789805604076126219?color=7289da&label=nflverse%20discord&logo=discord&logoColor=fff&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)
<!-- badges: end -->

ffopportunity builds a dataframe of Expected Fantasy Points by
preprocessing and applying an xgboost model to nflverse play-by-play
data. It also includes utilities to download precomputed data from
automated GitHub releases.

## About

Expected Fantasy Points are a measure of player opportunities in fantasy
football - essentially aiming to quantify how many points the average
player would score given a specific situation and opportunity. It uses
xgboost and tidymodels trained on public nflverse data from 2006-2020 to
do this.

For more on the modeling details, see the articles posted to this
website: <https://ffopportunity.ffverse.com/articles/>

## Installation

<!--Install the stable version from CRAN with:

```r
install.packages("ffopportunity")
```
-->

Install the development version from GitHub with:

``` r
install.packages("ffopportunity", repos = c("https://ffverse.r-universe.dev", getOption("repos")))

# or use remotes/devtools
# install.packages("remotes")
remotes::install_github("ffverse/ffopportunity")
```

## Usage

The two main functions of {ffopportunity} are `ep_load()` and
`ep_build()`.

You can download the latest version of the EP data with `ep_load()` as
follows:

``` r
library(ffopportunity)
#> Warning: package 'ffopportunity' was built under R version 4.2.1
ep_load(season = 2020:2021, type = "weekly")
#> → <ffopportunity predictions>
#> → Generated 2022-09-12 12:59:18 with ep model version "latest"
#> # A tibble: 11,769 × 159
#>    season posteam  week game_id  playe…¹ full_…² posit…³ pass_…⁴ rec_a…⁵ rush_…⁶
#>    <chr>  <chr>   <dbl> <chr>    <chr>   <chr>   <chr>     <dbl>   <dbl>   <dbl>
#>  1 2020   SF          1 2020_01… 00-003… Jimmy … QB           33       0       1
#>  2 2020   SF          1 2020_01… 00-003… George… TE            0       5       1
#>  3 2020   ARI         1 2020_01… 00-003… Kyler … QB           39       0      11
#>  4 2020   ARI         1 2020_01… 00-003… DeAndr… WR            0      16       0
#>  5 2020   ARI         1 2020_01… 00-002… Larry … WR            0       5       0
#>  6 2020   ARI         1 2020_01… <NA>    <NA>    <NA>          0       2       0
#>  7 2020   SF          1 2020_01… 00-003… Raheem… RB            0       5      15
#>  8 2020   ARI         1 2020_01… 00-003… Kenyan… RB            0       2      16
#>  9 2020   ARI         1 2020_01… 00-003… Christ… WR            0       5       0
#> 10 2020   SF          1 2020_01… 00-003… Trent … WR            0       5       0
#> # … with 11,759 more rows, 149 more variables: pass_air_yards <dbl>,
#> #   rec_air_yards <dbl>, pass_completions <dbl>, receptions <dbl>,
#> #   pass_completions_exp <dbl>, receptions_exp <dbl>, pass_yards_gained <dbl>,
#> #   rec_yards_gained <dbl>, rush_yards_gained <dbl>,
#> #   pass_yards_gained_exp <dbl>, rec_yards_gained_exp <dbl>,
#> #   rush_yards_gained_exp <dbl>, pass_touchdown <dbl>, rec_touchdown <dbl>,
#> #   rush_touchdown <dbl>, pass_touchdown_exp <dbl>, rec_touchdown_exp <dbl>, …
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```

You can also build EP from base nflverse data with `ep_build()` as
follows:

``` r
ep_build(season = 2021, version = "latest")
```

    #> -- Starting ep build for 2021 season(s)! 2022-01-11 07:58:44 -------------------
    #> > Loading pbp 2022-01-11 07:58:44
    #> > Preprocessing pbp 2022-01-11 07:58:46
    #> > Generating predictions 2022-01-11 07:58:54
    #> > Summarizing data 2022-01-11 07:59:33
    #> -- Finished building ep for 2021 season(s)! 2022-01-11 07:59:33 ----------------
    #> > <ffopportunity predictions>
    #> > Generated 2022-01-11 07:59:33 with model version latest
    #> List of 5
    #>  $ ep_weekly  : tibble [5,756 x 159] (S3: tbl_df/tbl/data.frame)
    #>  $ ep_pbp_pass: tibble [18,747 x 57] (S3: tbl_df/tbl/data.table/data.frame)
    #>  $ ep_pbp_rush: tibble [14,038 x 47] (S3: tbl_df/tbl/data.table/data.frame)
    #>  $ ep_version : chr "latest"
    #>  $ timestamp  : POSIXct[1:1], format: "2022-01-11 07:59:33"

## Data

ffopportunity data is automated with GitHub Actions and can be manually
downloaded in RDS, parquet, and csv formats from the [releases
page](https://github.com/ffverse/ffopportunity/releases).

## Getting help

The best places to get help on this package are:

-   the [nflverse discord](https://discord.com/invite/5Er2FBnnQa) (for
    both this package as well as anything R/NFL related)
-   opening [an
    issue](https://github.com/ffverse/ffopportunity/issues/new/choose)

## Contributing

Many hands make light work! Here are some ways you can contribute to
this project:

-   You can [open an
    issue](https://github.com/ffverse/ffopportunity/issues/new/choose)
    if you’d like to request specific data or report a bug/error.

-   If you’d like to contribute code, please check out [the contribution
    guidelines](https://ffopportunity.ffverse.com/CONTRIBUTING.html).

## Terms of Use

The R code for this package is released as open source under the [GPL v3
License](https://ffopportunity.ffverse.com/LICENSE.html). The models and
expected points data included within this package’s are licensed under
[Creative Commons Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/)

## Code of Conduct

Please note that the ffopportunity project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
