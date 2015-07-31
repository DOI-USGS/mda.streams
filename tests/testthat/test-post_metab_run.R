context("post_metab_run and delete_metab_run")

test_that("metab runs can be posted & deleted", {
  dontrun <- function() {
    set_scheme('mda_streams_dev')
    login_sb()
    
    proj_dir <- if(file.exists('test-post_metab_run.R')) "../.." else "."
    metab_dir <- file.path(proj_dir, "tests/temp/150713 0.0.1 test_run")
    dir.create(metab_dir, showWarnings=FALSE)
    write.table(data.frame(x=1:10), row.names=FALSE, quote=FALSE, file=file.path(metab_dir,"dummy_config.tsv"))
    writeLines(c("this is a dummy","log file."), con=file.path(metab_dir,"dummy_out.Rout"))
    writeLines(c("this is a dummy","data file."), con=file.path(metab_dir,"dummy_dat.RData"))
    
    # post
    expect_message(post_metab_run(folder=metab_dir, files=c("dummy_out.Rout", "dummy_config.tsv")))
    
    # append
    expect_message(post_metab_run(folder=metab_dir, files="dummy_dat.RData", on_exists="addfiles"))
    expect_message(expect_error(post_metab_run(folder=metab_dir, files="dummy_dat.RData", on_exists="addfiles")))
    
    # locate
    expect_equal(nchar(mrid <- locate_metab_run(title=basename(metab_dir))), 24)
    
    # download files
    expect_equal(1, length(localfile <- download_item_files(item_ids=mrid, item_names="metabrun1", files="dummy_config.tsv", on_local_exists='replace')))
    expect_equal(3, length(localfiles <- download_item_files(item_ids=mrid, item_names="metabrun1", files=NA, on_local_exists='replace')))
    expect_true(all(c("dummy_config.tsv","dummy_dat.RData","dummy_out.Rout") %in% dir(tempdir())))
    #view_folder(localfiles[1])
    
    # delete files
    mda.streams:::delete_metab_run(basename(metab_dir), files_only=TRUE)
    expect_true(is.na(download_item_files(item_ids=mrid, item_names="metabrun1", files=NA, on_remote_missing="return_NA")))
    
    # delete item
    expect_message(post_metab_run(folder=metab_dir, files=c("dummy_config.tsv","dummy_dat.RData"), on_exists="addfiles"))
    mda.streams:::delete_metab_run(basename(metab_dir))
    
    # check that it's gone
    expect_true(is.na(locate_metab_run(title=basename(metab_dir))))
    
    set_scheme('mda_streams')
  }
})
