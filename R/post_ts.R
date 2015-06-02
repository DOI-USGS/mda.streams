#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
#' @param ... Session object from \link{authenticate_sb}
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
	
  
  scheme = get_scheme()
  
	for (i in 1:length(files)){
    base_file <- basename(files[i])
    pieces <- strsplit(base_file, '[-.]')
    site <- pieces[[1]][1]
	  ts_varname = pieces[[1]][2]
	  #Check if item already exists
	  if (item_exists(scheme=scheme,type=ts_varname, 
	                 key=site, ...)){
	    stop('The ', ts_varname, ' timeseries for this site already exists')
	  }
    
    #find site root
    site_root = query_item_identifier(scheme= scheme, 
                                      type='site_root', key=site, ...)
    if(nrow(site_root) != 1){
      stop('There is no site root available for site:', site)
    }
    
	  #Create item if it does not exist
	  ts_item = item_create(parent_id=site_root$id, 
	                        title=ts_varname, ...)
	  
	  #attach file to item
	  item_append_files(ts_item, files = files[i], ...)
	  
	  #tag item with our special identifier
	  item_update_identifier(ts_item, scheme = scheme, type = ts_varname, 
	                         key=site, ...)
    
	}

}