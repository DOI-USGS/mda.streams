#' Create new site in SB
#' 
#' @param sites one or more local site IDs (e.g. nwis_09238475)
#' @param on_exists character. what should be done when an item already exists?
#' @param verbose logical. Should status messages be given?
#' @return an item list
#' @author Corinna Gries
#' @import sbtools
#' @importFrom stats setNames
#' @export
#' @examples 
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_00000000", "nwis_00000001", "nwis_00000002")
#' is.null(post_site()) # returns NULL (empty list) if sites is missing or NULL
#' (post_site(sites)) # adds sites, returns item IDs
#' (post_site(sites, on_exists="skip")) # leaves existing items untouched, returns item IDs
#' (post_site(sites, on_exists="clear")) # empties existing sites of child items, returns item IDs
#' mda.streams:::delete_site(sites)
#' 
#' set_scheme("mda_streams")
#' }
post_site <- function(sites, on_exists=c("stop", "skip", "clear", "replace"), verbose = TRUE){
  
  # check inputs & session
  if(missing(sites) || is.null(sites)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  if(is.null(current_session()) || !session_validate()) stop("need ScienceBase access; call login_sb() first")
  
  posted_items <- sapply(setNames(sites, sites), function(site) {
    
    # look for an existing ts and respond appropriately
    site_id <- locate_site(site, by="either")
    if(!is.na(site_id)) {
      if(verbose) message('the folder for site ', site, ' already exists')
      switch(
        on_exists,
        "stop"={ 
          stop('site already exists and on_exists="stop"') },
        "skip"={ 
          if(isTRUE(verbose)) message("skipping site") 
          return(site_id)
        },
        "clear"={
          if(isTRUE(verbose)) message("deleting children from the site")
          return(delete_site(site, children_only=TRUE, verbose=verbose))
        },
        "replace"={
          if(isTRUE(verbose)) message("deleting the entire site")
          delete_site(site, verbose=verbose)
        }
      )
    }
     
    # create the item and add title, scheme, type, and key
    site_id <- item_create(locate_folder("sites"), title=site)$id
    item_update_identifier(site_id, scheme=get_scheme(), type="site_root", key=site)
    
    return(site_id)  
  })
  
  invisible(posted_items)
}
