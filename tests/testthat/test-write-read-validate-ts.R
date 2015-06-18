context("write_ts read_ts verify_ts")

test_that("verify_ts and write_ts and read_ts work", {
  dat <- unitted::u(data.frame(DateTime=as.POSIXct("2004-05-03 08:09:10", tz="UTC"), doobs=10.4), c(NA, "mg L^-1"))
  
  # verify_ts
  expect_true(verify_ts(unitted::u(dat, c('UTC', "mg L^-1")), var="doobs"))
  expect_warning(verify_ts(unitted::u(dat, c('UTC', "mg L^-1")), var="wtr"))
  
  # write and read
  datfile <- write_ts(dat, "nwis_06893820", "doobs", "nwis", tempdir())
  expect_equal(basename(datfile), "nwis_06893820-ts_doobs_nwis.tsv.gz")
  expect_equal(read_ts(datfile), dat)
  expect_true(file.remove(datfile))
})
