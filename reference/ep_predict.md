# Predict EP

This function runs the prediction functions over preprocessed data.

## Usage

``` r
ep_predict(preprocessed_pbp, version = c("latest", "v1.0.0"))
```

## Arguments

- preprocessed_pbp:

  list with dataframes created by `ep_preprocess`

- version:

  ep model version - available is `"latest"` or `"v1.0.0"` (these are
  currently the same thing)

## Value

a dataframe with the expected fields added

## Examples

``` r
# \donttest{
  try({
  preprocessed <- readRDS(system.file("ep_preprocessed.rds",package = "ffopportunity"))
  # this file is equivalent to nflreadr::load_pbp(2021) %>% head(1000) %>% ep_preprocess()
  ep_predict(preprocessed)
  })
#> $rush_df
#> # A tibble: 252 × 50
#>    game_id    play_id desc  rusher_player_id full_name posteam two_point_attempt
#>    <chr>        <dbl> <chr> <chr>            <chr>     <chr>               <dbl>
#>  1 2021_01_A…      55 (15:… 00-0032764       Derrick … TEN                     0
#>  2 2021_01_A…     181 (12:… 00-0035228       Kyler Mu… ARI                     0
#>  3 2021_01_A…     282 (11:… 00-0034681       Chase Ed… ARI                     0
#>  4 2021_01_A…     473 (9:1… 00-0032764       Derrick … TEN                     0
#>  5 2021_01_A…     716 (8:1… 00-0032764       Derrick … TEN                     0
#>  6 2021_01_A…     737 (7:2… 00-0032764       Derrick … TEN                     0
#>  7 2021_01_A…     816 (6:4… 00-0034681       Chase Ed… ARI                     0
#>  8 2021_01_A…    1011 (2:2… 00-0032764       Derrick … TEN                     0
#>  9 2021_01_A…    1032 (1:4… 00-0032764       Derrick … TEN                     0
#> 10 2021_01_A…    1185 (14:… 00-0033553       James Co… ARI                     0
#> # ℹ 242 more rows
#> # ℹ 43 more variables: two_point_converted <dbl>, rush_attempt <dbl>,
#> #   first_down_rush <dbl>, fumble_lost <dbl>, season <dbl>, week <int>,
#> #   rushing_yards <dbl>, rush_touchdown <fct>, first_down <fct>,
#> #   posteam_type <chr>, run_location <chr>, run_gap <chr>, run_gap_dir <chr>,
#> #   surface <chr>, wind <int>, temp <int>, roof <chr>, position <chr>,
#> #   yardline_100 <dbl>, half_seconds_remaining <dbl>, …
#> 
#> $pass_df
#> # A tibble: 430 × 57
#>    game_id       play_id desc  passer_player_id passer_full_name passer_position
#>    <chr>           <dbl> <chr> <chr>            <chr>            <chr>          
#>  1 2021_01_ARI_…      76 (14:… 00-0029701       Ryan Tannehill   QB             
#>  2 2021_01_ARI_…     100 (13:… 00-0029701       Ryan Tannehill   QB             
#>  3 2021_01_ARI_…     152 (13:… 00-0035228       Kyler Murray     QB             
#>  4 2021_01_ARI_…     218 (12:… 00-0035228       Kyler Murray     QB             
#>  5 2021_01_ARI_…     253 (11:… 00-0035228       Kyler Murray     QB             
#>  6 2021_01_ARI_…     386 (10:… 00-0035228       Kyler Murray     QB             
#>  7 2021_01_ARI_…     410 (9:2… 00-0035228       Kyler Murray     QB             
#>  8 2021_01_ARI_…     599 (8:3… 00-0035228       Kyler Murray     QB             
#>  9 2021_01_ARI_…     621 (8:2… 00-0035228       Kyler Murray     QB             
#> 10 2021_01_ARI_…     660 (8:1… 00-0035228       Kyler Murray     QB             
#> # ℹ 420 more rows
#> # ℹ 51 more variables: receiver_player_id <chr>, receiver_full_name <chr>,
#> #   receiver_position <chr>, posteam <chr>, two_point_attempt <dbl>,
#> #   two_point_converted <dbl>, pass_attempt <dbl>, receiving_yards <dbl>,
#> #   first_down_pass <dbl>, fumble_lost <dbl>, season <dbl>, week <int>,
#> #   complete_pass <fct>, yards_after_catch <dbl>, pass_touchdown <fct>,
#> #   first_down <fct>, interception <fct>, relative_to_endzone <dbl>, …
#> 
# }
```
