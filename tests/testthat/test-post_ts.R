context("post_ts and delete_ts")

if(sbtools::is_logged_in()) {
  set_scheme("mda_streams_dev")
  # the following commands require some waiting between lines. probably best to test manually.
  manual <- TRUE
  # EVEN with waiting, both manual and automated, there will be times when 
  # things don't get written or become visible immediately. Such is ScienceBase.
  # Try not to do a lot of things to a single item in quick succession.
  
  test_that("post_ts works with all on_exists options", {
    # save whatever is currently in ScienceBase for the sites we're going to test with
    sites <- c("nwis_040871488","nwis_05406457")
    if(manual) cat(locate_site(sites, "url", br=F), sep="\n")
    var <- "doobs"
    var_src <- make_var_src(var, "nwis")
    if(!file.exists("tests/temp")) dir.create("tests/temp")
    orig_ts <- download_ts(var_src, sites, on_local_exists="replace", on_remote_missing="return_NA", folder="tests/temp")
    orig_ts_locs <- locate_ts(var_src, sites, by="either") # same NA/character structure as orig_ts but works even for flawed ts items
    delete_notes <- sapply(seq_along(orig_ts_locs), function(num) {
      if(!is.na(orig_ts_locs[num])) {
        if(exists("orig_ts")) message("moving SB file to tests/temp: ", paste0(orig_ts_locs[num], collapse=", "))
        mda.streams:::delete_ts(var_src=var_src, site_name=sites[num])
      }
      # this is a delete_ts test whenever there was something on SB for this var_src & site
      expect_equal(locate_ts(var_src=var_src, site_name=sites[num], by="either"), NA)
    })
    
    # get data for testing
    dir.create(file.path(tempdir(), "test_post_ts"), showWarnings=FALSE)
    paths <- file.path(tempdir(), paste0("test_post_ts/", c("jan1","jan2","decjan")))
    shh <- sapply(paths, dir.create, showWarnings=FALSE)
    files_jan1 <- stage_nwis_ts(
      sites = sites, var = var, times = c('2014-01-01','2014-01-02'), verbose=FALSE, folder=paths[1])
    files_jan2 <- stage_nwis_ts(
      sites = sites, var = var, times = c('2014-01-02','2014-01-03'), verbose=FALSE, folder=paths[2])
    files_decjan <- stage_nwis_ts(
      sites = sites, var = var, times = c('2013-12-11','2014-01-11'), verbose=FALSE, folder=paths[3])
    
    # post fresh
    post_ts(files=files_jan1, on_exists="stop", verbose=FALSE)
    expect_equal(nchar(locate_ts(var_src, sites)), c(24,24))
    if(manual) summarize_ts(var_src, sites)
    
    # post stop stop on exists
    expect_message(expect_error(post_ts(files=files_jan1, on_exists="stop"), "already exists"), "preparing to post")
    if(manual) summarize_ts(var_src, sites)
    
    # post with replace
    expect_message(post_ts(files=files_jan1, on_exists="replace"), "replace")
    post_ts(files=files_jan1, on_exists="replace", verbose=FALSE) # expect silent action
    if(manual) summarize_ts(var_src, sites)
    
    # post with skip
    expect_message(post_ts(files=files_jan1, on_exists="skip"), "skipping")
    post_ts(files=files_jan1, on_exists="skip", verbose=FALSE) # expect silent action
    if(manual) summarize_ts(var_src, sites)
    
    # post with merge a non-overlapping dataset
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(24, 288))
    expect_message(post_ts(files=files_jan2, on_exists="merge"), "merging")
    if(manual) summarize_ts(var_src, sites)
    post_ts(files=files_jan2, on_exists="merge", verbose=FALSE) # expect silent action
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(48, 576))
    
    # post with merge an overlapping dataset
    expect_message(post_ts(files=files_decjan, on_exists="merge"), "merging")
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(1929, 8928))
    if(manual) summarize_ts(var_src, sites)
    
    # replace the original ts data on SB
    replace_notes <- sapply(seq_along(orig_ts), function(num) {
      if(!is.na(orig_ts[num])) {
        message("moving file back from tests/temp to SB: ", paste0(orig_ts[num], collapse=", "))
        post_ts(orig_ts[num], on_exists="replace", verbose=FALSE)
        file.remove(orig_ts[num])
      } else {
        mda.streams:::delete_ts(var_src=var_src, site_name=sites[num], verbose=FALSE)
      }
    })
    
  })
  
  set_scheme("mda_streams")
}