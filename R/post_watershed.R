#' Post a new watershed shape to a site on SB
#' 
#' Posts a file from the local computer to ScienceBase.
#' 
#' @param site The unique site identifie?r
#' @param files String vector of files that make up the shapefile
#' @return Returns SB id of newly created watershed item
#' @author Luke Winslow
#' @export
post_watershed = function(site, files){
	#parent as site, scheme, type=watershed, key = site, title=watershed
	
	#check session
  sb_require_login("stop")
  
	##TODO: Check input
	
	#Check that it doesn't already exist
	check = query_item_identifier(scheme=get_scheme(),type='watershed', key=site)
	
	if(length(check) > 0){
		stop('Watershed item already exists for this site')
	}
	
	#get site_root
	site_root = query_item_identifier(scheme=get_scheme(), type='site_root', key=site)
	
	if(length(site_root) != 1){
		stop('Site root not found or ambiguous, check site ID:', site)
	}
	
	ws_id = item_upload_create(site_root$id, files=files)
	
	item_update_identifier(ws_id, scheme=get_scheme(), type='watershed', key=site)
	
	item_update(ws_id, list('title'=unbox('watershed')))
	
	return(ws_id)
	
}