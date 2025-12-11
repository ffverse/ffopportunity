# Build EP

This function builds Expected Fantasy Points predictions by downloading
the xgboost models and play-by-play data, applying the model, and
summarizing to player level.

## Usage

``` r
ep_build(season = nflreadr::most_recent_season(), version = "latest")
```

## Arguments

- season:

  a numeric vector of seasons that defaults to most recent season. Must
  be later than 2006.

- version:

  an EP model version - one of "latest" (default) or "v1.0.0" (these are
  currently identical)

## Value

a list containing three dataframes: `ep_weekly` provides a game-level
summary by player, `ep_pbp_pass` provides EP data on pass plays, and
`ep_pbp_rush` provides EP data on rush plays.

## See also

Other main:
[`ep_load()`](https://ffopportunity.ffverse.com/reference/ep_load.md)

## Examples

``` r
# \donttest{
try({ # prevents cran-related errors
  ep_build(season = 2021)
})
#> ── Starting ep build for 2021 season(s)! 2025-12-11 15:29:48.082594 ────────────
#> → Downloading EP models, if required 2025-12-11 15:29:48.087442
#> ℹ Could not find ep model version "latest" in cache directory: 
#> ~/.cache/ffopportunity 
#> ℹ Attempting download
#> ✔ Successfully downloaded ep model version "latest" to cache!
#> → Loading pbp 2025-12-11 15:29:51.583642
#> → Preprocessing pbp 2025-12-11 15:29:57.219579
#> → Generating predictions 2025-12-11 15:30:08.819998
#> → Summarizing data 2025-12-11 15:30:09.651761
#> ── Finished building ep for 2021 season(s)! 2025-12-11 15:30:09.655087 ─────────
#> → <ffopportunity predictions>
#> → Generated 2025-12-11 15:30:09.658931 with model version latest
#> List of 5
#>  $ ep_weekly  : tibble [6,065 × 159] (S3: tbl_df/tbl/data.frame)
#>  $ ep_pbp_pass: nflvrs_d [19,769 × 57] (S3: nflverse_data/tbl_df/tbl/data.table/data.frame)
#>  $ ep_pbp_rush: nflvrs_d [15,164 × 50] (S3: nflverse_data/tbl_df/tbl/data.table/data.frame)
#>  $ ep_version : chr "latest"
#>  $ timestamp  : POSIXct[1:1], format: "2025-12-11 15:30:09"
# }
```
