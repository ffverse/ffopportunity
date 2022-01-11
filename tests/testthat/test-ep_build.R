test_that("ep_build and ep_load work", {
  skippy()
  ep_2020 <- ep_build(2020, version = "v1.0.0")
  expect_s3_class(ep_2020,"ffopps_build")
  expect_gte(nrow(ep_2020$ep_weekly), 5000)
  expect_gte(nrow(ep_2020$ep_pbp_pass), 18000)
  expect_gte(nrow(ep_2020$ep_pbp_rush), 13000)

  ep_weekly <- ep_load(2020, type = "weekly",version = "v1.0.0")
  ep_pbp_pass <- ep_load(2020, type = "pbp_pass",version = "v1.0.0")
  ep_pbp_rush <- ep_load(2020, type = "pbp_rush",version = "v1.0.0")

  expect_equal(nrow(ep_2020$ep_weekly), nrow(ep_weekly))
  expect_equal(nrow(ep_2020$ep_pbp_pass), nrow(ep_pbp_pass))
  expect_equal(nrow(ep_2020$ep_pbp_rush), nrow(ep_pbp_rush))

})
