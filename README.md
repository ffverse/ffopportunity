
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ffexpectedpoints

*Models and Data for Expected Fantasy Points*

<!-- badges: start -->

[![CRAN
status](https://img.shields.io/cran/v/ffexpectedpoints?style=flat-square&logo=R&label=CRAN)](https://CRAN.R-project.org/package=ffexpectedpoints)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Dev
status](https://img.shields.io/github/r-package/v/ffverse/ffexpectedpoints/main?label=dev&style=flat-square&logo=github)](https://ffexpectedpoints.ffverse.com/)
[![nflverse
discord](https://img.shields.io/discord/789805604076126219?color=7289da&label=nflverse%20discord&logo=discord&logoColor=fff&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)
<!-- badges: end -->

ffexpectedpoints builds Expected Fantasy Points data by applying an
xgboost model to nflverse play-by-play data, as well as providing
utilities to download precomputed data.

## Installation

<!--Install the stable version from CRAN with:

```r
install.packages("ffexpectedpoints")
```
-->

Install the development version from GitHub with:

``` r
install.packages("ffexpectedpoints", repos = "https://ffverse.r-universe.dev")

# or use remotes/devtools
# install.packages("remotes")
remotes::install_github("ffverse/ffexpectedpoints")
```

## Usage

The two main functions of {ffexpectedpoints} are `ep_load()` and
`ep_build()`.

You can download the latest version of the EP data with `ep_load()` as
follows:

``` r
library(ffexpectedpoints)
ep_load(season = 2020:2021, type = "weekly")
#> > <ffexpectedpoints predictions>
#> > Generated 2022-01-10 09:39:41 with ep model version "latest"
#> # A tibble: 11,529 x 159
#>    season posteam  week game_id     player_id full_name    position pass_attempt
#>    <chr>  <chr>   <dbl> <chr>       <chr>     <chr>        <chr>           <dbl>
#>  1 2020   SF          1 2020_01_AR~ 00-00313~ Jimmy Garop~ QB                 33
#>  2 2020   SF          1 2020_01_AR~ 00-00332~ George Kitt~ TE                  0
#>  3 2020   ARI         1 2020_01_AR~ 00-00352~ Kyler Murray QB                 39
#>  4 2020   ARI         1 2020_01_AR~ 00-00305~ DeAndre Hop~ WR                  0
#>  5 2020   ARI         1 2020_01_AR~ 00-00229~ Larry Fitzg~ WR                  0
#>  6 2020   ARI         1 2020_01_AR~ <NA>      <NA>         <NA>                0
#>  7 2020   SF          1 2020_01_AR~ 00-00316~ Raheem Most~ RB                  0
#>  8 2020   ARI         1 2020_01_AR~ 00-00331~ Kenyan Drake RB                  0
#>  9 2020   ARI         1 2020_01_AR~ 00-00347~ Christian K~ WR                  0
#> 10 2020   SF          1 2020_01_AR~ 00-00332~ Trent Taylor WR                  0
#> # ... with 11,519 more rows, and 151 more variables: rec_attempt <dbl>,
#> #   rush_attempt <dbl>, pass_air_yards <dbl>, rec_air_yards <dbl>,
#> #   pass_completions <dbl>, receptions <dbl>, pass_completions_exp <dbl>,
#> #   receptions_exp <dbl>, pass_yards_gained <dbl>, rec_yards_gained <dbl>,
#> #   rush_yards_gained <dbl>, pass_yards_gained_exp <dbl>,
#> #   rec_yards_gained_exp <dbl>, rush_yards_gained_exp <dbl>,
#> #   pass_touchdown <dbl>, rec_touchdown <dbl>, rush_touchdown <dbl>,
#> #   pass_touchdown_exp <dbl>, rec_touchdown_exp <dbl>,
#> #   rush_touchdown_exp <dbl>, ...
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
    #> > <ffexpectedpoints predictions>
    #> > Generated 2022-01-11 07:59:33 with model version latest
    #> List of 5
    #>  $ ep_weekly  : tibble [5,756 x 159] (S3: tbl_df/tbl/data.frame)
    #>  $ ep_pbp_pass: tibble [18,747 x 57] (S3: tbl_df/tbl/data.table/data.frame)
    #>  $ ep_pbp_rush: tibble [14,038 x 47] (S3: tbl_df/tbl/data.table/data.frame)
    #>  $ ep_version : chr "latest"
    #>  $ timestamp  : POSIXct[1:1], format: "2022-01-11 07:59:33"

## Data

ffexpectedpoints data is automated with GitHub Actions and can be
manually downloaded in RDS, parquet, and csv formats from the [releases
page](https://github.com/ffverse/ffexpectedpoints/releases).

## Getting help

The best places to get help on this package are:

-   the [nflverse discord](https://discord.com/invite/5Er2FBnnQa) (for
    both this package as well as anything R/NFL related)
-   opening [an
    issue](https://github.com/ffverse/ffexpectedpoints/issues/new/choose)

## Contributing

Many hands make light work! Here are some ways you can contribute to
this project:

-   You can [open an
    issue](https://github.com/ffverse/ffexpectedpoints/issues/new/choose)
    if you’d like to request specific data or report a bug/error.

-   If you’d like to contribute code, please check out [the contribution
    guidelines](https://ffexpectedpoints.ffverse.com/CONTRIBUTING.html).

## Terms of Use

The R code for this package is released as open source under the [GPL v3
License](https://ffexpectedpoints.ffverse.com/LICENSE.html). The models
and expected points data included within this package’s are licensed
under [Creative Commons Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/)

## Code of Conduct

Please note that the ffexpectedpoints project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
