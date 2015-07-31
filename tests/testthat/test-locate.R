context("locate_item")

if(!is.null(sbtools::current_session())) {
  login_sb()
  
  test_that("locate_item by tag, dir, either all works", {
    # find a folder
    expect_equal(3, length(which(!is.na(mda.streams:::locate_item(key=c("sites","proPOSals","PublicaTiOns"), type="root", format="folder_url")))))
    expect_equal(nchar(mda.streams:::locate_item(parent=locate_folder("PROJECT"), title="PublicationS", by="dir")), 24)
    # find a site
    expect_equal(nchar(mda.streams:::locate_item(key="nwis_02322688", type="site_root")), 24)
    expect_equal(nchar(mda.streams:::locate_item(parent=locate_folder("sites"), title="nwis_02322688", by="dir")), 24) # much slower, as expected
    expect_equal(nchar(mda.streams:::locate_item(key="nwis_02322688", type="site_root", parent=locate_folder("sites"), title="nwis_02322688", by="either")), 24)
    # find a time series
    expect_true(is.na(mda.streams:::locate_item(key="nwis_02322688", type="ts_par_nwis")))
    expect_equal(nchar(mda.streams:::locate_item(key="nwis_01021050", type="ts_doobs_nwis")), 24)
    expect_equal(nchar(mda.streams:::locate_item(parent=locate_site("nwis_01021050"), title="ts_doobs_nwis", by="dir")), 24)
  })
  
  test_that("locate_folder, locate_site, and locate_ts work", {
    # locate_folder
    expect_equal(nchar(locate_folder("publications")), 24)
    expect_equal(nchar(locate_folder("publications", by="dir")), 24)
    expect_equal(nchar(locate_folder("publications", by="either")), 24)
    expect_error(locate_folder("notafolder"), "should be one of")
    
    # locate_site
    expect_equal(nchar(locate_site("nwis_02322688")), 24)
    #expect_equal(nchar(locate_site("nwis_02322688", by="dir")), 24) # so slow!
    expect_equal(nchar(locate_site("nwis_02322688", by="either")), 24)
    
    # locate_ts
    expect_error(locate_ts("ts_par_nwis", "nwis_02322688"), "in ScienceBase format already")
    expect_true(is.na(locate_ts("par_nwis", "nwis_02322688")))
    expect_equal(nchar(locate_ts("doobs_nwis", "nwis_01021050")), 24)
    expect_equal(nchar(locate_ts("doobs_nwis", "nwis_01021050", by="dir")), 24)
    expect_equal(nchar(locate_ts("doobs_nwis", "nwis_01021050", by="either")), 24)
  })
  
  test_that("locate_folder, locate_site, and locate_ts work in Dev/Sites", {
    set_scheme("mda_streams_dev")
    
    # locate_folder
    expect_equal(nchar(locate_folder("project")), 24)
    expect_equal(nchar(locate_folder("sites", by="dir")), 24)

    # locate_site
    expect_equal(nchar(locate_site("nwis_040871488")), 24)
    
    # locate_ts
    expect_true(is.na(locate_ts("par_nwis", "nwis_040871488")))
    expect_equal(nchar(locate_ts("doobs_nwis", "nwis_040871488")), 24)
    
    set_scheme("mda_streams")
  })
}