context("post_ts and delete_ts")

if(!is.null(sbtools::current_session())) {
  test_that("post_ts works with all on_exists options", {
    # save whatever is currently in ScienceBase for the sites we're going to test with
    sites <- c("nwis_06893820","nwis_01484680")
    var <- "doobs"
    var_src <- make_var_src(var, "nwis")
    if(!file.exists("tests/temp")) dir.create("tests/temp")
    orig_ts <- download_ts(var_src, sites, on_remote_missing="return_NA", folder="tests/temp")
    delete_notes <- sapply(seq_along(orig_ts), function(num) {
      if(!is.na(orig_ts[num])) {
        message("moving SB file to tests/temp: ", paste0(orig_ts[num], collapse=", "))
        mda.streams:::delete_ts(var_src=var_src, site_name=sites[num])
      }
      # this is a delete_ts test whenever there was something on SB for this var_src & site
      expect_equal(locate_ts(var_src=var_src, site_name=sites[num]), NA)
    })
    
    # get data for testing
    dir.create(file.path(tempdir(), "test_post_ts"), showWarnings=FALSE)
    paths <- file.path(tempdir(), paste0("test_post_ts/", c("jan1","jan2","decjan")))
    sapply(paths, dir.create, showWarnings=FALSE)
    files_jan1 <- stage_nwis_ts(
      sites = sites, var = "doobs", times = c('2014-01-01','2014-01-02'), verbose=TRUE, folder=paths[1])
    files_jan2 <- stage_nwis_ts(
      sites = sites, var = "doobs", times = c('2014-01-02','2014-01-03'), verbose=TRUE, folder=paths[2])
    files_decjan <- stage_nwis_ts(
      sites = sites, var = "doobs", times = c('2013-12-11','2014-01-11'), verbose=TRUE, folder=paths[3])
    
    # post fresh
    #post_ts(files=files_jan1, on_exists="stop")
    
    # post with replace
    
    # post with skip
    
    # post with merge a non-overlapping dataset
    
    # post wtih merge an overlapping dataset

    # replace the original ts data on SB
    replace_notes <- sapply(seq_along(orig_ts), function(num) {
      if(!is.na(orig_ts[num])) {
        message("moving file back from tests/temp to SB: ", paste0(orig_ts[num], collapse=", "))
        post_ts(orig_ts[num], on_exists="replace")
      } else {
        mda.streams:::delete_ts(var_src=var_src, site_name=sites[num])
      }
    })
    
  })
}