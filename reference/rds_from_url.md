# Load .rds file from a remote connection

Load .rds file from a remote connection

## Usage

``` r
rds_from_url(url)
```

## Arguments

- url:

  a character url

## Value

a dataframe as created by
[`readRDS()`](https://rdrr.io/r/base/readRDS.html)

## Examples

``` r
# \donttest{
try({ rds_from_url("https://github.com/nflverse/nfldata/raw/master/data/games.rds") })
#> Error in rds_from_url("https://github.com/nflverse/nfldata/raw/master/data/games.rds") : 
#>   could not find function "rds_from_url"
# }
```
