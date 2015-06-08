#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
#' @param on_exists character specifying an action when a file to post already
#'   exists on ScienceBase
#' @param verbose logical. Should status messages be given?
#' @param ... args passed to \code{\link[sbtools]{session_check_reauth}}
#' @author Luke Winslow, Corinna Gries, Jordan S Read
#' @import sbtools
#' @examples
#' \dontrun{
#' files <- stage_nldas_ts(sites = c("nwis_06893820","nwis_01484680"), variable = "baro", 
#'                         times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' post_ts(files, session = sbtools::authenticate_sb())
#' }
#' @export
post_ts = function(files, on_exists=c("stop", "skip", "replace"), verbose=TRUE, ...){
  
  # check inputs
  if(is.null(files)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  
  posted_items <- sapply(1:length(files), function(i) {
    if(verbose) message('posting file ', files[i])
    
    # parse the file name to determine where to post the file
    out <- parse_ts_path(files[i], out = c('ts_name','var_src','site_name','file_name'))
    site <- out$site_name
    
    # Check if item already exists
    ts_id <- locate_ts(var_src=out$var_src, site_name=out$site_name, ...)
    if (!is.na(ts_id)) {
      if(verbose) message('the ', out$ts_name, ' timeseries for site ', out$site_name, ' already exists')
      switch(on_exists,
             "stop"={ stop('item already exists and on_exists="stop"') },
             "skip"={ next },
             "append"={ stop("oops - append isn't implemented yet") },
             "replace"={ if(verbose) message("deleting timeseries item before replacement")
               delete_ts(out$var_src, out$site_name, verbose=verbose, ...)
             })
    }
    
    # find the site root
    site_root = locate_site(out$site_name, ...)
    if(is.na(site_root)){
      stop('no site folder available for site ', out$site_name)
    }
    
    if(verbose) message("posting file to site ", out$site_name, ", timeseries ", out$ts_name)
    
    # create the ts item if it does not exist
    ts_item = item_create(parent_id=site_root, title=out$ts_name, ...)
    
    # attach data file to ts item
    item_append_files(ts_item, files = files[i], ...)
    
    # tag item with our special identifiers
    item_update_identifier(ts_item, scheme = get_scheme(), type = out$ts_name, key=out$site_name, ...)
    
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
delete_ts <- function(var_src, site_name, verbose=TRUE, ...) {
  
  deletion_msgs <- lapply(setNames(var_src, var_src), function(var) {
    lapply(setNames(site_name, site_name), function(site) {
      # find the item id
      ts_id <- locate_ts(var_src=var, site_name=site, ...)
      
      if(is.na(ts_id)) {
        if(verbose) message("skipping missing ", var, " timeseries for site ", site)
        NULL
      } else {
        if(verbose) message("deleting ", var, " timeseries for site ", site)
        
        # delete any data files from the item
        item_rm_files(ts_id) 
        
        # sleep to give time for full deletion
        for(wait in 1:20) {
          Sys.sleep(0.2)
          if(nrow(item_list_files(ts_id)) == 0) break
        }
        
        # delete the item itself
        item_rm(ts_id) 
      }
    })
  })
  
  invisible(deletion_msgs)
}