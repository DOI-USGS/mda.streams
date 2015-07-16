#' Post files to a model run item on SB
#' 
#' Post any number of files to an SB item representing a complete, multi-site 
#' model run. These files will probably include a .tsv config file, an .RData 
#' file containing a list of model fit objects, and an .Rout file containing the
#' log information from the run.
#' 
#' @param folder a single folder name from which the tag, strategy, and date
#'   (and therefore metab_run title) will be pulled, and where named files will
#'   be sought
#' @param files a vector of filenames to select from within folder and to post 
#'   to the new SB item
#' @param on_exists character. what should be done when an item already exists?
#' @param verbose logical. Should status messages be given?
#' @export
post_metab_run <- function(folder, files, on_exists=c("stop", "skip", "addfiles"), verbose=TRUE) {

  # check inputs
  on_exists <- match.arg(on_exists)
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before posting")
  if(length(folder) != 1) stop("one folder name per call to post_metab_run, please")
  title <- basename(folder)
  
  expect_id_loss <- TRUE
  run_id <- locate_metab_run(title=title, by="either")
  if(!is.na(run_id)) {
    if(verbose) message('the metab_run item ', title, ' already exists on SB')
    switch(
      on_exists,
      "stop"={ 
        stop('the metab_run item already exists and on_exists="stop"') },
      "skip"={ 
        if(isTRUE(verbose)) message("skipping posting of the metab_run item") 
        return(NA)
      },
      "addfiles"={ 
        if(verbose) message("adding files to existing metab_run: ", run_id)
        # check if files with these names already exist; otherwise, proceed
        # without altering item
        existing_files <- item_list_files(run_id)$fname
        if(any(files %in% existing_files)) 
          stop("new files (",paste0(files, collapse=", "),
               ") must have distinct names from existing files (",
               paste0(existing_files, collapse=", "),")")
        expect_id_loss <- FALSE
      })
  } else {
    # create the item
    if(verbose) message("creating metab_run item: ", title)
    run_id <- item_create(parent_id=locate_folder("metab_runs"), title=title)
  }
  
  # attach data file to ts item. SB quirk: must be done before tagging with 
  # identifiers, or identifiers will be lost
  if(verbose) message("posting metab_run files: ", paste0(files, collapse=", "))
  item_append_files(run_id, files = file.path(folder, files))
  
  # tag item with our special identifiers. if the item already existed,
  # identifiers should be wiped out by a known SB quirk, so sleep to give time
  # for the files to be added and the identifiers to disappear so we can replace them
  if(expect_id_loss) {
    for(wait in 1:100) {
      Sys.sleep(0.2)
      if(nrow(sbtools::item_list_files(run_id)) > 0 && is.null(item_get(run_id)$identifiers)) break
      if(wait==100) stop("identifiers didn't disappear and so can't be replaced")
    }
    if(verbose) message("adding/replacing identifiers for item ", run_id, ": ",
                        "scheme=", get_scheme(), ", type=","metab_run", ", key=", title)
    repair_metab_run(title, limit=5000)
  }
  
  run_id
}
