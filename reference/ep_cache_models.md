# Model versioning

This function checks the cache for a previously downloaded model and
then (optionally) tries to download the model from GitHub release.

## Usage

``` r
ep_cache_models(
  version = c("latest", "v1.0.0"),
  force = FALSE,
  ask = interactive()
)
```

## Arguments

- version:

  one of "latest" or "v1.0.0" - currently these refer to the same thing

- force:

  TRUE or FALSE - forces download regardless of currently existing

- ask:

  TRUE or FALSE - ask before downloading - `force` will skip this.

## Value

a status message after attempting to download the model.
