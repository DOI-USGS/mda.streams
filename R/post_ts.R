#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
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
post_ts = function(files, ...){
  
  for (i in 1:length(files)){
    # parse the file name to determine where to post the file
    out <- parse_ts_path(files[i], out = c('variable','site','file_name'))
    site <- out[2]
    ts_varname = make_ts_name(out[1])
    
    # Check if item already exists
    if (!is.na(locate_ts(variable=out[1], siteid=site, ...))) {
      stop('The ', ts_varname, ' timeseries for this site already exists')
    }
    
    # find the site root
    site_root = locate_site(site, ...)
    if(is.na(site_root)){
      stop('There is no site root available for site:', site)
    }
    
    # create the ts item if it does not exist
    ts_item = item_create(parent_id=site_root, title=ts_varname, ...)
    
    # attach data file to ts item
    item_append_files(ts_item, files = files[i], ...)
    
    # tag item with our special identifiers
    item_update_identifier(ts_item, scheme = get_scheme(), type = ts_varname, key=site, ...)
    
  }
  
}