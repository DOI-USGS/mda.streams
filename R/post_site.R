#' Create new site in SB
#' 
#' @param sites one or more local site IDs (e.g. nwis_09238475)
#' @param ... Additional parameters supplied to \code{\link[sbtools]{session_check_reauth}}
#' @param replace_existing logical. Should an item that already exists be
#'   replaced?
#' @param verbose logical. Should status messages be given?
#' @return an item list
#' @author Corinna Gries
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' locate_site(c("nwis_08437710","nwis_03401385","nwis_02423397"), format="url")
#' post_site("nwis_08437710") # returns NA
#' post_site("nwis_08437710", replace_existing=TRUE)
#' post_site(c("nwis_08437710","nwis_03401385","nwis_02423397"), replace_existing=TRUE)
#' }
post_site <- function(sites, replace_existing = FALSE, verbose = TRUE, ...){
  
	session_check_reauth(...)
	
  sapply(setNames(sites, sites), function(site) {
    
    # check whether it already exits
    item <- locate_site(site)
    
    # if it already exists, either replace it or return without doing anything
    if(!is.na(item)) {
      if(replace_existing) {
        if(isTRUE(verbose)) message("deleting site ", site, " before replacement")
        delete_site(site, verbose=verbose)
        for(wait in 1:10) {
          Sys.sleep(1) # sleep to give time for full deletion
          if(is.na(locate_site(site))) break
        }
      } else {
        if(isTRUE(verbose)) message("ignoring existing site ", site)
        return(NA)
      }
    }
    
    # create the item and add title, scheme, type, and key
    if(isTRUE(verbose)) message("posting site ", site)
    item <- item_create(parent_id=locate_folder("sites"), title=site)
    item_update_identifier(item, scheme=get_scheme(), type="site_root", key=site)
    
    # sleep for a half second to avoid overloading SB
    Sys.sleep(0.5)
    
    return(item)  
  })
}

#' Delete a site and all its files.
#' 
#' @param sites one or more site IDs such as "nwis_08437710" to look up and
#'   delete
#' @param verbose logical. Should status messages be given?
#' @param ... other args passed to \code{\link[sbtools]{session_check_reauth}}
#' @return anything passed back from the final item_rm
#' @keywords internal
#' @examples
#' \dontrun{
#' cat(locate_site("nwis_08437710", format="url"))
#' files <- sapply(list_datasets("nwis_08437710", type="ts"), function(var_src) {
#'   download_ts(var_src, "nwis_08437710")
#' })
#' 
#' # try delete_site
#' mda.streams:::delete_site("nwis_08437710")
#' mda.streams:::delete_site(c("nwis_08437710","nwis_03401385","nwis_02423397"))
#' 
#' # now repair the damage
#' post_site("nwis_08437710")
#' Sys.sleep(1)
#' post_ts(files[!is.na(files)])
#' 
#' # or if you REALLY messed it up:
#' files <- sapply(c("doobs","disch","stage","wtr"), function(var) {
#'   staged <- stage_nwis_ts("nwis_08437710", var, 
#'     times=as.POSIXct(paste(c("2014-01-01","2015-01-01"), "00:00:00"), tz="UTC"))
#'   if(is.null(staged)) NA else staged
#' })
#' }
delete_site <- function(sites, verbose=TRUE, ...) {
  session_check_reauth(...)
  
  lapply(sites, function(site) {
    
    item <- locate_site(site)
    if(is.na(item)) {
      if(isTRUE(verbose)) message("not deleting the missing site ", site)
      return() # do nothing if it's already not there
    }
    if(isTRUE(verbose)) message("deleting site ", site)
    
    # delete the children and their files
    children <- item_list_children(item, limit=100)
    if(nrow(children) > 0) {
      for(child in children$id) {
        item_rm_files(child) # delete any data files from the child
        for(wait in 1:20) {
          Sys.sleep(0.2) # sleep to give time for full deletion
          if(nrow(item_list_files(child)) == 0) break
        }
        item_rm(child) # delete the child itself
      }
    }
    
    # sleep for a bit so it can finish deleting the children
    for(wait in 1:20) {
      Sys.sleep(1) # sleep to give time for full deletion
      if(nrow(item_list_children(item, limit=2)) == 0) break
    }
    
    # delete the site folder
    item_rm(item)
  })
}