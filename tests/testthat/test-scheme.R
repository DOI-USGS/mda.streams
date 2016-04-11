context("scheme")

test_that("schemes can be get and set", {
  expect_equal(get_scheme(), "mda_streams")
  set_scheme("mda_streams_dev")
  expect_equal(get_scheme(), "mda_streams_dev")
  set_scheme("mda_streams")
})