context("mda_helpers")

test_that("var_src can be made and parsed", {
  expect_equal(mda.streams:::make_var_src("baro","nldas"), "baro_nldas")
  expect_equal(mda.streams:::parse_var_src("baro_nldas"), data.frame(var="baro", src="nldas", stringsAsFactors=FALSE))
  expect_equal(mda.streams:::parse_var_src("baro_nldas", "var"), "baro")
  expect_equal(mda.streams:::parse_var_src("baro_nldas", "src"), "nldas")
})

test_that("ts variable names can be made and parsed", {
  # make_ts_name
  expect_equal(make_ts_name(c("doobs","doobs","wtr"), "nwis"), c("ts_doobs_nwis","ts_doobs_nwis","ts_wtr_nwis"))
  expect_equal(make_ts_name(c("doobs_nwis","doobs_nwis","wtr_nwis")), c("ts_doobs_nwis","ts_doobs_nwis","ts_wtr_nwis"))
  expect_equal(make_ts_name(c("doobs","depth","wtr"), c("nwis","google","nwis")), c("ts_doobs_nwis","ts_depth_google","ts_wtr_nwis"))
  expect_equal(make_ts_name("doobs_nwis"), "ts_doobs_nwis")
  expect_error(make_ts_name("ts_doobs"), "ScienceBase format already")
  expect_error(make_ts_name("ts_doobs_nwis"), "ScienceBase format already")
  expect_error(make_ts_name("doobs"), "improper var_src format")
  expect_error(make_ts_name(c("doobs","ts_doobs"), "nwis"))
  #expect_error(make_ts_name(c("wtr","dobbythehouseelf"), "nwis"))
  
  # parse_ts_name
  expect_equal(parse_ts_name("ts_doobs_nwis"), "doobs_nwis")
  expect_equal(parse_ts_name("ts_doobs_nwis", use_names=TRUE), c(ts_doobs_nwis="doobs_nwis"))
  expect_equal(parse_ts_name(c("ts_doobs_nwis", "ts_disch_nwis")), c(ts_doobs_nwis="doobs_nwis", ts_disch_nwis="disch_nwis"))
  expect_equal(parse_ts_name(c("ts_doobs_nwis", "ts_disch_nwis"), out="var", use_names=FALSE), c("doobs", "disch"))
  expect_equal(parse_ts_name(c("ts_doobs_nwis", "ts_disch_nwis"), out="src", use_names=FALSE), c("nwis", "nwis"))
  expect_equal(parse_ts_name(c("ts_doobs_nwis", "ts_disch_nwis"), out=c("var","var_src","src"), use_names=FALSE),
               data.frame(var=c("doobs","disch"), var_src=c("doobs_nwis","disch_nwis"), src=c("nwis", "nwis"), stringsAsFactors=FALSE))
  expect_equal(parse_ts_name("ts_disch_nwis", out=c("var","src"), use_names=FALSE),
               data.frame(var="disch", src="nwis", stringsAsFactors=FALSE), info="1 row, several columns")
  expect_error(parse_ts_name(c("doobs","ts_doobs")), "unexpected ts variable prefix")
  expect_error(parse_ts_name(c("ts_doobs_nwis","ts_dobby_shhh")), "var_src isn't listed in get_var_src_codes")
  
  # back and forth
  expect_equal(parse_ts_name(make_ts_name("wtr","nwis")), "wtr_nwis")
  expect_equal(make_ts_name(parse_ts_name("ts_wtr_nwis")), "ts_wtr_nwis")
})

test_that("site names can be made and parsed", {
  # make_site_name
  expect_equal(make_site_name(c(100010, 123987)), c("nwis_100010", "nwis_123987"))
  expect_equal(make_site_name(c("100010", "abc32")), c("nwis_100010", "nwis_abc32"))
  expect_error(make_site_name(c("16587623", "nwis_31486")))
  expect_error(make_site_name(c(100010, 123987), "bobs"))
  
  # parse_site_name
  expect_equal(parse_site_name(c("nwis_10293847", "nwis_203847965")), c("10293847","203847965"))
  expect_equal(parse_site_name(c("nwis_10293847", "nwis_203847965"), use_names=TRUE), c(nwis_10293847="10293847",nwis_203847965="203847965"))
  expect_equal(parse_site_name(c("nwis_10293847", "nwis_203847965"), out=c("database","sitenum")), 
               data.frame(database=rep("nwis",2), sitenum=c("10293847","203847965"), row.names=c("nwis_10293847", "nwis_203847965"), stringsAsFactors=FALSE))
  expect_error(parse_site_name("nwis"))
  expect_error(parse_site_name("nwis_"))
  
  # back and forth
  expect_equal(parse_site_name(make_site_name(c(123,345,1324))), as.character(c(123,345,1324)))
  expect_equal(make_site_name(parse_site_name(c("nwis_1230498", "nwis_23"))), c("nwis_1230498", "nwis_23"))
})

test_that("ts file paths can be made and parsed", {
  # make_ts_path
  expect_equal(make_ts_path("nwis_12398074", "ts_doobs_nwis"), "nwis_12398074-ts_doobs_nwis.tsv.gz")
  expect_equal(make_ts_path(make_site_name("12398074"), make_ts_name("doobs","nwis")), "nwis_12398074-ts_doobs_nwis.tsv.gz")
  
  # parse_ts_path
  expect_equal(colnames(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", use_names=FALSE)), eval(formals(parse_ts_path)$out))
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="ts_name"), "ts_doobs_nwis")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="var"), "doobs")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="src"), "nwis")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="var_src"), "doobs_nwis")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="site_name"), "nwis_12398074")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="sitenum"), "12398074")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="database"), "nwis")
  expect_equal(parse_ts_path("nwis_12398074-ts_doobs_nwis.tsv.gz", out="file_name"), "nwis_12398074-ts_doobs_nwis.tsv.gz")
  
})

