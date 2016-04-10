context("bulk staging & posting")

if(sbtools::is_logged_in()) {
  set_scheme("mda_streams_dev")
 
  test_that("can stage sitelist and post sites", {
    # stage
    sites <- stage_nwis_sitelist(c('doobs'), 'WI')
    expect_equal(length(sites), 1118) # this was 22 back in summer 2015 - WI has added a loooottt of doobs sites, or maybe we're no longer filtering correctly?
    expect_true(all(make_site_name(c('04027000', '040871488', '05406500'), 'nwis') %in% sites))
    
    # post
    sites <- make_site_name(c('04027000', '040871488', '05406500'), 'nwis')
    post_site(sites[1:3], on_exists="clear")
    expect_true(all(sites[1:3] %in% list_sites()))
  }) 
  
  test_that("can stage and post nwis data", {
    sites <- make_site_name(c('04027000', '040871488', '05406500'), 'nwis')

    # clear any existing doobs data
    mda.streams:::delete_ts("doobs_nwis", sites)
    
    # stage. best to save files in their own subdir of tempdir so they're not overwritten by nwis_files_2
    folder <- paste0(tempdir(), "\\1")
    dir.create(folder, showWarnings=FALSE)
    nwis_files <- stage_nwis_ts(sites, var="doobs", times=c('2014-01-01', '2014-01-05'), folder=folder)
    expect_equal(length(nwis_files), 2) # one site has no data
    
    # post
    post_ts(nwis_files, on_exists="skip")
    
    # repairs as needed
    if(sum(!is.na(locate_ts("doobs_nwis", sites))) != length(nwis_files)) {
      repair_ts("doobs_nwis", sites) # might need this here. leaving warnings so we know when this runs
    }
    
    # check
    expect_equal(sum(!is.na(locate_ts("doobs_nwis", sites))), length(nwis_files))
    sumry <- summarize_ts("doobs_nwis", sites, version='rds')
    expect_true(all(is.na(sumry$end_date) | (
      sumry$end_date < as.POSIXct("2014-01-05 00:00:00", tz="UTC") &
        sumry$end_date > as.POSIXct("2014-01-01 00:00:00", tz="UTC"))))
    
    # stage more
    folder <- paste0(tempdir(), "\\2")
    dir.create(folder, showWarnings=FALSE)
    nwis_files_2 <- stage_nwis_ts(sites, var="doobs", times=c('2014-01-05', '2014-01-10'), folder=folder)
    
    # post with merge
    post_ts(nwis_files_2, on_exists="merge")
    if(sum(!is.na(locate_ts("doobs_nwis", sites))) != length(nwis_files)) {
      repair_ts("doobs_nwis", sites) # more often needed on the merge here. leaving warnings so we know when this runs
    }
    
    # check
    sumry_2 <- summarize_ts("doobs_nwis", sites, version='rds', on_local_exists='replace')
    expect_true(all(is.na(sumry$num_rows) | sumry$num_rows < unitted::u(50,'rows') | sumry$num_rows < sumry_2$num_rows))
    
  })
  
  set_scheme("mda_streams")
}