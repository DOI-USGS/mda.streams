#' Create new site in SB
#' 
#' @param site Local site ID (e.g. nwis_09238475)
#' @param ... Additional parameters supplied to \code{\link[sbtools]{session_check_reauth}}
#' @param replace_existing logical. Should an item that already exists be
#'   replaced?
#' @return an item list
#' @author Corinna Gries
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_08437710", format="url")
#' create_site("nwis_08437710") # returns list()
#' create_site("nwis_08437710", replace_existing=TRUE)
#' }
create_site <- function(site, replace_existing = FALSE, ...){
  
	session_check_reauth(...)
	
  # check whether it already exits
  item = locate_site(site)
  
  # if it already exists, either replace it or return without doing anything
  if(!is.na(item)) {
    if (replace_existing) {
      delete_site(site)
    } else {
      return(list())
    }
  }
  
  # create the item and add title, scheme, type, and key
  item = item_create(parent_id=locate_folder("sites"), title=site)
  item_update_identifier(item, scheme=get_scheme(), type="site_root", key=site)
  
  return(item)  
  
}

#' Delete a site and all its files.
#' 
#' @param site a site ID such as "nwis_08437710" to look up and delete
#' @param ... other args passed to \code{\link[sbtools]{session_check_reauth}}
#' @return anything passed back from the final item_rm
#' @keywords internal
#' @examples
#' \dontrun{
#' cat(locate_site("nwis_08437710", format="url"))
#' files <- sapply(get_ts_variables("nwis_08437710"), function(var) {
#'   download_ts("nwis_08437710", var)
#' })
#' 
#' # try delete_site
#' mda.streams:::delete_site("nwis_08437710")
#' 
#' # now repair the damage
#' create_site("nwis_08437710")
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
delete_site <- function(site, ...) {
  session_check_reauth(...)
  item <- locate_site(site)
  if(is.na(item)) return() # do nothing if it's already not there
  
  # delete the children and their files
  children <- item_list_children(item, limit=100)
  if(nrow(children) > 0) {
    for(child in children$id) {
      item_rm_files(child) # delete any data files from the child
      item_rm(child) # delete the child itself
    }
  }
  
  # sleep for a second so it can finish deleting the children
  Sys.sleep(1)
  
  # delete the site folder
  item_rm(item)
}