context("config")

test_that("config files can be staged, verified, and interpreted", {
  
  # stage
  expect_warning(config_file <- stage_metab_config(
    tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=tempfile()))
  expect_is(read.table(config_file, header=TRUE, stringsAsFactors=FALSE), "data.frame")
  expect_warning(
    expect_equal(3, nrow(stage_metab_config(
      tag="0.0.1", strategy="test", site=c("nwis_01433500", "nwis_01454700", "nwis_01463500"), filename=NULL))))
  
  # verify
  expect_warning(egconfig <- stage_metab_config(
    tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=NULL))
  expect_true(verify_config(egconfig))
  expect_false(suppressWarnings(verify_config(egconfig[,c(1,3:5,9:30,9)])))
  expect_warning(verify_config(egconfig[,c(1,3:5,9:30,9)]))
  expect_error(testthat::expect_warning(verify_config(egconfig[,c(1,3:5,9:30,9)], on_fail=stop)))
})