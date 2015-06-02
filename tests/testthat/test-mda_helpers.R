context("mda_helpers")

test_that("ts variable names can be made and parsed", {
  # make_ts_name
  #expect_equal(mda.streams:::make_ts_name(c("doobs","doobs","wtr")), c("ts_doobs","ts_doobs","ts_wtr"))
  #expect_error(mda.streams:::make_ts_name(c("doobs","ts_doobs")))
  #expect_error(mda.streams:::make_ts_name(c("wtr","dobbythehouseelf")))
  
  # parse_ts_name
  #expect_equal(mda.streams:::parse_ts_name(c("ts_doobs", "ts_disch")), c("doobs", "disch"))
  #expect_error(mda.streams:::parse_ts_name(c("doobs","ts_doobs")))
  #expect_error(mda.streams:::parse_ts_name(c("ts_doobs","ts_dobby")))
  
  # back and forth
  #expect_equal(mda.streams:::parse_ts_name(mda.streams:::make_ts_name("wtr")), "wtr")
  #expect_equal(mda.streams:::make_ts_name(mda.streams:::parse_ts_name("ts_wtr")), "ts_wtr")
})

test_that("site names can be made and parsed", {
  # make_site_name
  expect_equal(mda.streams:::make_site_name(c(100010, 123987)), c("nwis_100010", "nwis_123987"))
  expect_equal(mda.streams:::make_site_name(c("100010", "abc32")), c("nwis_100010", "nwis_abc32"))
  expect_error(mda.streams:::make_site_name(c("16587623", "nwis_31486")))
  expect_error(mda.streams:::make_site_name(c(100010, 123987), "bobs"))
  
  # parse_site_name
  expect_equal(mda.streams:::parse_site_name(c("nwis_10293847", "nwis_203847965")), c("10293847","203847965"))
  expect_equal(mda.streams:::parse_site_name(c("nwis_10293847", "nwis_203847965"), use_names=TRUE), c(nwis_10293847="10293847",nwis_203847965="203847965"))
  expect_equal(mda.streams:::parse_site_name(c("nwis_10293847", "nwis_203847965"), out=c("database","sitenum")), 
               data.frame(database=rep("nwis",2), sitenum=c("10293847","203847965"), row.names=c("nwis_10293847", "nwis_203847965"), stringsAsFactors=FALSE))
  expect_error(mda.streams:::parse_site_name("nwis"))
  expect_error(mda.streams:::parse_site_name("nwis_"))
  
  # back and forth
  expect_equal(mda.streams:::parse_site_name(mda.streams:::make_site_name(c(123,345,1324))), as.character(c(123,345,1324)))
  expect_equal(mda.streams:::make_site_name(mda.streams:::parse_site_name(c("nwis_1230498", "nwis_23"))), c("nwis_1230498", "nwis_23"))
})