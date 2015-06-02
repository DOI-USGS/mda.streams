#' Post a new watershed shape to a site on SB
#' 
#' Posts a file from the local computer to ScienceBase.
#' 
#' @param site The unique site identifie?r
#' @param files String vector of files that make up the shapefile
#' @param session A SB session from \link{authenticate_sb}
#' @return Returns SB id of newly created watershed item
#' @author Luke Winslow
#' @export
post_watershed = function(site, files, session){
	#parent as site, scheme, type=watershed, key = site, title=watershed

	##TODO: Check input
	
	#Check that it doesn't already exist
	check = query_item_identifier(get_scheme(),'watershed', site, session)
	
	if(nrow(check) > 0){
		stop('Watershed item already exists for this site')
	}
	
	#get site_root
	site_root = query_item_identifier(scheme=get_scheme(), type='site_root',
												key=site, session)
	
	if(nrow(site_root) != 1){
		stop('Site root not found or ambiguous, check site ID:', site)
	}
	
	ws_id = item_upload_create(site_root$id, files=files, session=session)
	
	item_update_identifier(ws_id, scheme=get_scheme(), type='watershed',
												 key=site, session=session)
	
	item_update(ws_id, list('title'=unbox('watershed')), session)
	
	return(ws_id)
	
}