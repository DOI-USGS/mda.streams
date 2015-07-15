#' Delete a time series item and its data
#' 
#' Deletes timeseries objects specified by all combinations of variable and site
#' 
#' @inheritParams locate_ts
#' @param files_only logical. If TRUE, only the files will be deleted, leaving
#'   an empty ts item
#' @param verbose logical. Should status messages be given?
#' @keywords internal
#' @examples 
#' \dontrun{
#' sbtools::authenticate_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_05406479", "nwis_05435950", "nwis_04087119")
#' files <- stage_nwis_ts(sites = sites, var = "doobs", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'))
#' post_ts(files)
#' locate_ts("doobs_nwis", sites) # find the newly posted data online
#' mda.streams:::delete_ts("doobs_nwis", sites)
#' locate_ts("doobs_nwis", sites, by="either") # confirm it's all gone
#'  
#' set_scheme("mda_streams")
#' }
delete_ts <- function(var_src, site_name, files_only=FALSE, verbose=TRUE) {
  
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before deleting")
  
  # find the item id by hook or by crook (aka tag or dir)
  query_args <- data.frame(var_src=var_src, site_name=site_name, 
                           ts_id=locate_ts(var_src=var_src, site_name=site_name, by="either"), 
                           stringsAsFactors=FALSE)
  
  deletion_msgs <- 
    lapply(1:nrow(query_args), function(arg) {
      var <- query_args[arg, "var_src"]
      site <- query_args[arg, "site_name"]
      ts_id <- query_args[arg, "ts_id"]
      
      # if the item exists, delete it and its children
      if(is.na(ts_id)) {
        if(verbose) message("skipping deletion of missing ", var, " timeseries for site ", site)
        return(NA) # do nothing if it's already not there
      } else {
        if(verbose) message("deleting ", var, " timeseries for site ", site)
        
        # delete any data files from the item
        item_status <- item_rm_files(ts_id)
        if(!is.list(item_status)) stop("couldn't delete item because couldn't find files")
        
        # sleep to give time for full deletion
        for(wait in 1:50) {
          Sys.sleep(0.2)
          if(nrow(item_list_files(ts_id)) == 0) break
          if(wait==50) stop("failed to delete files & therefore item")
        }
        
        # delete the ts item itself or return its ID
        if(files_only) {
          return(ts_id) 
        } else {
          # delete the item
          out <- item_rm(ts_id) 
          # sleep (again!) to give time for full deletion
          for(wait in 1:20) {
            # longer wait between tries because this next call has been giving
            # occasional errors on sapply(query_out$id, function(id)
            # {item_get_fields(id, "parentId")})
            Sys.sleep(1) 
            if(is.na(locate_ts(var_src=var, site_name=site, by="either"))) break
            if(wait==20) stop("failed to delete item")
          }
          # if it worked, return the output
          return(out)
        }
      }
    })
  
  invisible(deletion_msgs)
}
