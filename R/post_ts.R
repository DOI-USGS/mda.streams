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
#' files <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), var = "baro", 
#'                         times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' post_ts(files)
#' }
#' @export
post_ts = function(files, on_exists=c("stop", "skip", "replace", "merge"), verbose=TRUE){
  
  # check inputs & session
  if(is.null(files)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before posting")
  
  # loop through files, posting each
  posted_items <- sapply(1:length(files), function(i) {
    if(verbose) message('preparing to post file ', files[i])
    
    # parse the file name to determine where to post the file
    out <- parse_ts_path(files[i], out = c('ts_name','var','src','var_src','site_name','file_name','dir_name'))
    
    # check & respond if item already exists
    ts_id <- locate_ts(var_src=out$var_src, site_name=out$site_name)
    if (!is.na(ts_id)) {
      if(verbose) message('the ', out$ts_name, ' timeseries for site ', out$site_name, ' already exists')
      switch(
        on_exists,
        "stop"={ stop('item already exists and on_exists="stop"') },
        "skip"={ 
          if(verbose) message("skipping timeseries item already on ScienceBase: ", ts_id)
          return(NA) 
        },
        "replace"={ 
          if(verbose) message("deleting timeseries item before replacement: ", ts_id)
          delete_ts(out$var_src, out$site_name, verbose=verbose)
        },
        "merge"={ 
          if(verbose) message("merging new timeseries with old: ", ts_id)
          ts_old <- read_ts(download_ts(var_src=out$var_src, site_name=out$site_name, on_local_exists="replace"))
          ts_new <- read_ts(files[i])
          # join. these lines should be changed once unitted::full_join.unitted is implemented
          if(!all.equal(get_units(ts_old), get_units(ts_new))) stop("units mismatch between old and new ts files")
          ts_merged <- u(full_join(v(ts_old), v(ts_new), by=names(ts_old)), get_units(ts_old)) 
          if(!all.equal(unique(v(ts_merged$DateTime)), v(ts_merged$DateTime))) stop("merge failed. try on_exists='replace'")
          # replace the input file, but write to a nearby directory so we don't overwrite the user's file
          merge_dir <- file.path(out$dir_name, "post_merge_temp")
          dir.create(merge_dir)
          files[i] <- write_ts(data=ts_merged, site=out$site_name, var=out$var, src=out$src, folder=merge_dir)
          # delete the old one in preparation for overwriting
          delete_ts(out$var_src, out$site_name, verbose=verbose)
        })
    }
    
    # find the site root
    site_root = locate_site(out$site_name)
    if(is.na(site_root)){
      stop('no site folder available for site ', out$site_name)
    }
    
    if(verbose) message("posting file to site ", out$site_name, ", timeseries ", out$ts_name)
    
    # create the ts item if it does not exist
    ts_item = item_create(parent_id=site_root, title=out$ts_name)
    
    # attach data file to ts item. SB quirk: must be done before tagging with 
    # identifiers, or identifiers will be lost
    item_append_files(ts_item, files = files[i])
    
    # tag item with our special identifiers
    item_update_identifier(ts_item, scheme = get_scheme(), type = out$ts_name, key=out$site_name)
    
    ts_item
  })
  
  invisible(posted_items)
}

#' Delete a time series item and its data
#' 
#' Deletes timeseries objects specified by all combinations of variable and site
#' 
#' @inheritParams locate_ts
#' @param verbose logical. Should status messages be given?
#' @keywords internal
#' @examples 
#' \dontrun{
#' mda.streams:::delete_ts(c("doobs_nwis","wtr_nwis"), 
#'   c("nwis_03271510", "nwis_412126095565201", "nwis_03409500"), verbose=TRUE)
#' }
delete_ts <- function(var_src, site_name, verbose=TRUE) {
  
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before deleting")
  
  deletion_msgs <- lapply(setNames(var_src, var_src), function(var) {
    lapply(setNames(site_name, site_name), function(site) {
      # find the item id by hook or by crook (aka tag or dir)
      ts_id <- locate_ts(var_src=var, site_name=site, by="either")
      
      # if the item exists, delete it and its children
      if(is.na(ts_id)) {
        if(verbose) message("skipping missing ", var, " timeseries for site ", site)
        NULL
      } else {
        if(verbose) message("deleting ", var, " timeseries for site ", site)
        
        # delete any data files from the item
        item_status <- item_rm_files(ts_id)
        if(!is.list(item_status)) stop("couldn't delete item because couldn't find children")
        
        # sleep to give time for full deletion
        for(wait in 1:50) {
          Sys.sleep(0.2)
          if(nrow(item_list_files(ts_id)) == 0) break
          if(wait==50) stop("failed to delete children & therefore item")
        }
        
        # delete the item itself
        deletion_msg <- item_rm(ts_id) 
        
        # sleep (again!) to give time for full deletion
        for(wait in 1:50) {
          Sys.sleep(0.2)
          if(all(!is.na(locate_ts(var_src=var, site_name=site, by="either"))))
          if(wait==50) stop("failed to delete item")
        }
      }
    })
  })
  
  invisible(deletion_msgs)
}