# Summarize EP

This function summarizes the EP data up to the game level

## Usage

``` r
ep_summarize(
  predicted_pbp,
  stat_type = c("all", "expected_points", "team_stats")
)
```

## Arguments

- predicted_pbp:

  list with dataframes created by `ep_predict`

- stat_type:

  options to limit the columns returned by `ep_summarize` - available
  options are "all", "expected_points", and "team stats"

## Value

a dataframe with the expected points fields added

## See also

`vignette("basic")` for example usage

## Examples

``` r
# \donttest{
try({
  predicted <- readRDS(system.file("ep_predicted.rds",package = "ffopportunity"))
  # equivalent to nflreadr::load_pbp(2021) %>% head(100) %>% ep_preprocess() %>% ep_predict()
  ep_summarize(predicted)
})
#> # A tibble: 124 × 159
#>    season posteam  week game_id        player_id full_name position pass_attempt
#>    <chr>  <chr>   <dbl> <chr>          <chr>     <chr>     <chr>           <dbl>
#>  1 2021   TEN         1 2021_01_ARI_T… 00-00297… Ryan Tan… QB                 35
#>  2 2021   TEN         1 2021_01_ARI_T… 00-00327… Derrick … RB                  0
#>  3 2021   TEN         1 2021_01_ARI_T… 00-00323… Chester … WR                  0
#>  4 2021   ARI         1 2021_01_ARI_T… 00-00352… Kyler Mu… QB                 32
#>  5 2021   ARI         1 2021_01_ARI_T… 00-00305… DeAndre … WR                  0
#>  6 2021   ARI         1 2021_01_ARI_T… 00-00346… Chase Ed… RB                  0
#>  7 2021   ARI         1 2021_01_ARI_T… 00-00279… A.J. Gre… WR                  0
#>  8 2021   ARI         1 2021_01_ARI_T… 00-00347… Christia… WR                  0
#>  9 2021   ARI         1 2021_01_ARI_T… 00-00369… Rondale … WR                  0
#> 10 2021   ARI         1 2021_01_ARI_T… 00-00301… Demetriu… TE                  0
#> # ℹ 114 more rows
#> # ℹ 151 more variables: rec_attempt <dbl>, rush_attempt <dbl>,
#> #   pass_air_yards <dbl>, rec_air_yards <dbl>, pass_completions <dbl>,
#> #   receptions <dbl>, pass_completions_exp <dbl>, receptions_exp <dbl>,
#> #   pass_yards_gained <dbl>, rec_yards_gained <dbl>, rush_yards_gained <dbl>,
#> #   pass_yards_gained_exp <dbl>, rec_yards_gained_exp <dbl>,
#> #   rush_yards_gained_exp <dbl>, pass_touchdown <dbl>, rec_touchdown <dbl>, …
# }
```
