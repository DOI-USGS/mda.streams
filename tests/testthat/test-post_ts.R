context("post_ts and delete_ts")

if(!is.null(sbtools::current_session())) {
  sbtools::authenticate_sb()
  
  test_that("post_ts works with all on_exists options", {
    # save whatever is currently in ScienceBase for the sites we're going to test with
    sites <- c("nwis_06893820","nwis_01484680")
    cat(locate_site(sites, "url"), sep="\n")
    var <- "doobs"
    var_src <- make_var_src(var, "nwis")
    if(!file.exists("tests/temp")) dir.create("tests/temp")
    orig_ts <- download_ts(var_src, sites, on_remote_missing="return_NA", folder="tests/temp") # might error if you've already saved a backup copy
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
    post_ts(files=files_jan1, on_exists="stop")
    
    # post stop stop on exists
    expect_message(expect_error(post_ts(files=files_jan1, on_exists="stop"), "already exists"), "preparing to post")
    
    # post with replace
    expect_message(post_ts(files=files_jan1, on_exists="replace"), "replace")
    post_ts(files=files_jan1, on_exists="replace", verbose=FALSE) # expect silent action
    
    # post with skip
    expect_message(post_ts(files=files_jan1, on_exists="skip"), "skipping")
    post_ts(files=files_jan1, on_exists="skip", verbose=FALSE) # expect silent action
    
    # post with merge a non-overlapping dataset
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(96, 96))
    expect_message(post_ts(files=files_jan2, on_exists="merge"), "merging")
    post_ts(files=files_jan2, on_exists="merge", verbose=FALSE) # expect silent action
    Sys.sleep(2) # SB is a slug. Need to wait or the summary won't get the 2nd file
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(192, 192))
    
    # post wtih merge an overlapping dataset
    expect_message(post_ts(files=files_decjan, on_exists="merge"), "merging")
    expect_equal(summarize_ts(var_src, sites)$num_rows, c(2854, 2976))
    
    # replace the original ts data on SB
    replace_notes <- sapply(seq_along(orig_ts), function(num) {
      if(!is.na(orig_ts[num])) {
        message("moving file back from tests/temp to SB: ", paste0(orig_ts[num], collapse=", "))
        post_ts(orig_ts[num], on_exists="replace")
        file.remove(orig_ts[num])
      } else {
        mda.streams:::delete_ts(var_src=var_src, site_name=sites[num])
      }
    })
    
  })
}