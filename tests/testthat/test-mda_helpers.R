context("mda_helpers")

test_that("ts variable names can be made and parsed", {
  expect_equal(mda.streams:::make_ts_name("doobs"), "ts_doobs")
  expect_equal(mda.streams:::parse_ts_name("ts_doobs"), "doobs")
  expect_equal(mda.streams:::parse_ts_name(mda.streams:::make_ts_name("wtr")), "wtr")
  expect_equal(mda.streams:::make_ts_name(mda.streams:::parse_ts_name("ts_wtr")), "ts_wtr")
})