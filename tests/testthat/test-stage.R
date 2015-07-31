context("bulk staging & posting")

if(!is.null(sbtools::current_session())) {
  login_sb()
  set_scheme("mda_streams_dev")
 
  test_that("can stage sitelist and post sites", {
    # stage
    sites <- stage_nwis_sitelist(c('doobs'), 'WI')
    expect_equal(length(sites), 22)
    
    # post
    post_site(sites, on_exists="clear")
    expect_true(all.equal(sort(sites), sort(get_sites())))
  }) 
  
  test_that("can stage and post nwis data", {
    sites <- get_sites()

    # clear any existing doobs data
    mda.streams:::delete_ts("doobs_nwis", sites)
    
    # stage. best to save files in their own subdir of tempdir so they're not overwritten by nwis_files_2
    folder <- paste0(tempdir(), "\\1")
    dir.create(folder, showWarnings=FALSE)
    nwis_files <- stage_nwis_ts(sites, var="doobs", times=c('2014-01-01', '2014-01-05'), folder=folder)
    
    # post
    post_ts(nwis_files, on_exists="skip")
    
    # repairs as needed
    if(sum(!is.na(locate_ts("doobs_nwis", sites))) != length(nwis_files)) {
      repair_ts("doobs_nwis", sites) # might need this here. leaving warnings so we know when this runs
    }
    
    # check
    expect_equal(sum(!is.na(locate_ts("doobs_nwis", sites))), length(nwis_files))
    sumry <- summarize_ts("doobs_nwis", sites)
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
    sumry_2 <- summarize_ts("doobs_nwis", sites)
    expect_true(all(is.na(sumry$num_rows) | sumry$num_rows < 50 | sumry$num_rows < sumry_2$num_rows))
    
  })
  
  set_scheme("mda_streams")
}