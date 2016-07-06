context("bulk staging & posting")

if(sbtools::is_logged_in()) {
 
  test_that("can stage sitelist and post sites", {
    set_scheme("mda_streams_dev")
    # stage
    sites <- stage_nwis_sitelist(c('doobs'), HUCs=21)
    expect_equal(length(sites), 1118) # this was 22 back in summer 2015 - WI has added a loooottt of doobs sites, or maybe we're no longer filtering correctly?
    expect_true(all(make_site_name(c('04027000', '040871488', '05406500'), 'nwis') %in% sites))
    
    # post
    sites <- make_site_name(c('04027000', '040871488', '05406500'), 'nwis')
    post_site(sites[1:3], on_exists="clear")
    expect_true(all(sites[1:3] %in% list_sites()))
  }) 
  
  test_that("can stage and post nwis data", {
    set_scheme("mda_streams_dev")
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
  
  test_that("can stage calculated tses", {
    set_scheme("mda_streams_dev")
    
    site="nwis_08062500"
    post_site(site, on_exists="clear")
    #bf <- stage_meta_basic()
    #post_meta(bf, on_exists = 'replace')
    #df <- stage_meta_dvqcoefs()
    #post_meta(df, on_exists = 'replace')
    
    # download & post starter data from nwis
    file_doobs <- stage_nwis_ts(site, 'doobs', times=c('2014-01-01', '2014-01-05'))
    post_ts(file_doobs, on_exists="skip", verbose=TRUE) # need this posted for next calcs
    file_disch <- stage_nwis_ts(site, 'disch', times=c('2014-01-01', '2014-01-05'))
    post_ts(file_disch, on_exists="skip", verbose=TRUE) # need this posted for next calcs
    file_wtr <- stage_nwis_ts(site, 'wtr', times=c('2014-01-01', '2014-01-05'))
    post_ts(file_wtr, on_exists="skip", verbose=TRUE) # need this posted for next calcs
    summarize_ts(list_tses(site), site, version='rds')
    
    # calc
    
    file <- stage_calc_ts(sites=site, var="sitetime", src="calcLon", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)

    file <- stage_calc_ts(sites=site, var="suntime", src="calcLon", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE) # need this posted for next calcs
    
    file <- stage_calc_ts(sites=site, var="par", src="calcLat", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    # file <- stage_calc_ts(sites=site, var="par", src="calcSw", verbose=TRUE)

    file <- stage_calc_ts(sites=site, var="depth", src="calcDischRaymond", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="depth", src="calcDischHarvey", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="veloc", src="calcDischRaymond", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="veloc", src="calcDischHarvey", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    # file <- stage_calc_ts(sites=site, var="dosat", src="calcGGbts", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var='baro', src='calcElev', verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="dosat", src="calcGGbconst", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="dopsat", src="calcObsSat", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    summarize_ts(list_tses(site), site, version='rds')
    alltses <- get_ts(
      c('doobs_nwis', 'suntime_calcLon', 'sitetime_calcLon', 'sitedate_calcLon', 
        'par_calcLat', 'depth_calcDischHarvey', 'dosat_calcGGbconst'), 
      site, version='rds')
    
    # daily means
    
    # file <- stage_calc_ts(sites=site, var="sitetimedaily", src="calcLon", verbose=TRUE)
    # head(read_ts(file))
    
    file <- stage_calc_ts(sites=site, var="sitedate", src="calcLon", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="doamp", src="calcDAmp", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)

    file <- stage_calc_ts(sites=site, var="dischdaily", src="calcDMean", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    file <- stage_calc_ts(sites=site, var="velocdaily", src="calcDMean", verbose=TRUE)
    head(read_ts(file))
    post_ts(file, on_exists="skip", verbose=TRUE)
    
    
    # sim

    simsite = 'styx_000000'
    post_site(simsite, on_exists="clear")
    
    library(dplyr); library(unitted)
    real_doobs <- get_ts("doobs_nwis", site, version='rds', on_local_exists="replace")

    file_suntime <- stage_calc_ts(sites=simsite, var="suntime", src="simLon", verbose=TRUE,
      inputs=list(utctime=real_doobs$DateTime, longitude=u(-96.46304, "degE")))
    head(read_ts(file_suntime))
    post_ts(file_suntime, on_exists="skip", verbose=TRUE)
    
    file_par <- stage_calc_ts(sites=simsite, var="par", src="simLat", verbose=TRUE,
      inputs=list(utctime=real_doobs$DateTime, suntime=read_ts(file_suntime)$suntime,
      latitude=u(32.42653, "degN")))
    head(read_ts(file_par))

    file_depth <- stage_calc_ts(sites=simsite, var="depth", src="simDischRaymond", verbose=TRUE,
      inputs=list(utctime=real_doobs$DateTime, disch=u(rep(2900, nrow(real_doobs)), "ft^3 s^-1")))
    head(read_ts(file_depth))

    file_dosat <- stage_calc_ts(sites=simsite, var="dosat", src="simGGbconst", verbose=TRUE,
      inputs=list(utctime=real_doobs$DateTime, wtr=u(rep(12, nrow(real_doobs)), "degC"), baro=u(90000, "Pa")))
    head(read_ts(file_dosat))

    # simNew

    file_par2 <- stage_calc_ts(sites=simsite, var="par", src="simNew", verbose=TRUE,
      inputs=list(utctime=real_doobs$DateTime,
                  value=u(rnorm(real_doobs$DateTime, 10, 2), "umol m^-2 s^-1")))
    head(read_ts(file_par2))

    # simCopy

    file_suntime2 <- stage_calc_ts(sites=simsite, var="suntime", src="simCopy", verbose=TRUE,
      inputs=list(from_src="calcLon", from_site=site, filter_fun=function(df) {
        df[df$DateTime >= min(real_doobs$DateTime) &
             df$DateTime <= max(real_doobs$DateTime), ] }))
    head(read_ts(file_suntime2))
       
  })
  
  set_scheme("mda_streams")
}