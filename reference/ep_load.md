# Load Expected Points data

This function downloads precomputed expected points data from the
ffopportunity automated releases.

## Usage

``` r
ep_load(
  season = nflreadr::most_recent_season(),
  type = c("weekly", "pbp_pass", "pbp_rush"),
  version = c("latest", "v1.0.0")
)
```

## Arguments

- season:

  A numeric vector of four digit years associated with given NFL
  seasons - defaults to latest season.

- type:

  Data type - one of `"weekly"`, `"pbp_pass"`, or `"pbp_rush"`

- version:

  EP model version: one of "latest" (default) or "v1.0.0" - these are
  currently identical.

## Value

a dataframe identical to what would be returned by
[`ffopportunity::ep_build()`](https://ffopportunity.ffverse.com/reference/ep_build.md)
for a given season.

## See also

Other main:
[`ep_build()`](https://ffopportunity.ffverse.com/reference/ep_build.md)

## Examples

``` r
# \donttest{
try({
  ep_load() %>% head(10)
  ep_load(2020:2021) %>% head(10)
  ep_load(2021, type = "pbp_pass") %>% head(10)
  ep_load(2006, type = "pbp_rush", version = "v1.0.0") %>% head(10)
})
#> Warning: cannot open URL 'https://github.com/ffverse/ffopportunity/releases/download/latest-data/ep_weekly_2020.rds': HTTP status was '503 Service Unavailable'
#> Warning: Failed to readRDS from <https://github.com/ffverse/ffopportunity/releases/download/latest-data/ep_weekly_2020.rds>
#> Warning: cannot open URL 'https://github.com/ffverse/ffopportunity/releases/download/v1.0.0-data/ep_pbp_rush_2006.rds': HTTP status was '503 Service Unavailable'
#> Warning: Failed to readRDS from <https://github.com/ffverse/ffopportunity/releases/download/v1.0.0-data/ep_pbp_rush_2006.rds>
#> → <ffopportunity predictions>
#> → Generated 2025-12-11 15:19:23.292259 with ep model version "v1.0.0"
#> # A tibble: 0 × 0
# }
```
