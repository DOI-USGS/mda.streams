#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
#' @param on_exists character specifying an action when a file to post already
#'   exists on ScienceBase
#' @param verbose logical. Should status messages be given?
#' @author Luke Winslow, Corinna Gries, Jordan S Read
#' @import sbtools
#' @importFrom lubridate tz
#' @importFrom unitted u v get_units
#' @examples
#' \dontrun{
#' sbtools::authenticate_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_05406479", "nwis_05435950", "nwis_04087119")
#' post_site(sites, on_exists="clear")
#' files <- stage_nwis_ts(sites = sites, var = "doobs", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' post_ts(files)
#' locate_ts("doobs_nwis", sites) # find the newly posted data online
#' post_ts(files, on_exists="skip")
#' post_ts(files, on_exists="replace")
#' post_site(sites, on_exists="clear")
#'  
#' set_scheme("mda_streams_")
#' }
#' @export
post_ts = function(files, on_exists=c("stop", "skip", "replace", "merge"), verbose=TRUE){
  
  # check inputs & session
  if(is.null(files)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before posting")
  
  # loop through files, posting each and recording whether we'll need to add tags
  expect_id_loss <- TRUE
  ts_ids <- sapply(1:length(files), function(i) {
    if(verbose) message('preparing to post file ', files[i])
    
    # parse the file name to determine where to post the file
    ts_path <- parse_ts_path(files[i], out = c('ts_name','var','src','var_src','site_name','file_name','dir_name'))
    
    # find the site root
    site_root = locate_site(ts_path$site_name)
    if(is.na(site_root)){
      stop('no site folder available for site ', ts_path$site_name)
    }

    # look for an existing ts and respond appropriately
    ts_id <- locate_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, by="either")
    if (is.na(ts_id)) {
      ts_id <- item_create(parent_id=site_root, title=ts_path$ts_name)
    } else {
      if(verbose) message('the ', ts_path$ts_name, ' timeseries for site ', ts_path$site_name, ' already exists')
      switch(
        on_exists,
        "stop"={ stop('item already exists and on_exists="stop"') },
        "skip"={ 
          if(verbose) message("skipping timeseries item already on ScienceBase: ", ts_id)
          return(NA) # na is signal that doesn't need new tags
        },
        "replace"={ 
          if(verbose) message("deleting timeseries data before replacement: ", ts_id)
          delete_ts(ts_path$var_src, ts_path$site_name, files_only=TRUE, verbose=verbose)
          expect_id_loss <<- FALSE
        },
        "merge"={ 
          if(verbose) message("merging new timeseries with old: ", ts_id)
          pre_merge_dir <- file.path(ts_path$dir_name, "pre_merge_temp")
          dir.create(pre_merge_dir, showWarnings=FALSE)
          ts_old <- read_ts(download_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, folder=pre_merge_dir, on_local_exists="replace"))
          ts_new <- read_ts(files[i])
          # join. these lines should be changed once unitted::full_join.unitted is implemented
          if(!all.equal(get_units(ts_old), get_units(ts_new))) stop("units mismatch between old and new ts files")
          ts_merged <- u(unique(rbind(v(ts_old), v(ts_new))), get_units(ts_old))
          if(!all.equal(unique(v(ts_merged$DateTime)), v(ts_merged$DateTime))) stop("merge failed. try on_exists='replace'")
          if(verbose) message("num rows before & after merge: old=", nrow(ts_old), ", new=", nrow(ts_new), ", merged=", nrow(ts_merged))
          # replace the input file, but write to a nearby directory so we don't overwrite the user's file
          post_merge_dir <- file.path(ts_path$dir_name, "post_merge_temp")
          dir.create(post_merge_dir, showWarnings=FALSE)
          files[i] <- write_ts(data=ts_merged, site=ts_path$site_name, var=ts_path$var, src=ts_path$src, folder=post_merge_dir)
          # delete the old one in preparation for overwriting
          delete_ts(ts_path$var_src, ts_path$site_name, files_only=TRUE, verbose=verbose)
          expect_id_loss <<- FALSE
        })
    }
    
    # attach data file to ts item. SB quirk: must be done before tagging with 
    # identifiers, or identifiers will be lost
    if(verbose) message("posting file to site ", ts_path$site_name, ", timeseries ", ts_path$ts_name)
    item_append_files(ts_id, files = files[i])

    # return id as signal that needs tags
    return(ts_id)
  })
  
  # separate loop to increase probability of success in re-tagging files when length(files) >> 1
  posted_items <- sapply(1:length(ts_ids), function(i) {
    
    # if we skipped it once, skip it again
    if(is.na(ts_ids[i])) 
      return(NA)
    else
      ts_id <- ts_ids[i]
    
    # parse (again) the file name to determine where to post the file
    ts_path <- parse_ts_path(files[i], out = c('ts_name','var','src','var_src','site_name','file_name','dir_name'))
    
    # tag item with our special identifiers. if the item already existed,
    # identifiers should be wiped out by a known SB quirk, so sleep to give time
    # for the files to be added and the identifiers to disappear so we can replace them
    if(expect_id_loss) {
      for(wait in 1:100) {
        Sys.sleep(0.2)
        if(nrow(item_list_files(ts_id)) > 0 && is.null(item_get(ts_id)$identifiers)) break
        if(wait==100) stop("identifiers didn't disappear and so can't be replaced; try again later with ",
                           "repair_ts('", ts_path$var_src, "', '", ts_path$site_name, "')")
      }
      if(verbose) message("adding/replacing identifiers for item ", ts_id, ": ",
                          "scheme=", get_scheme(), ", type=", ts_path$ts_name, ", key=", ts_path$site_name)
      repair_ts(ts_path$var_src, ts_path$site_name, limit=5000)
    }
    
    ts_id
  })
  
  invisible(posted_items)
}
