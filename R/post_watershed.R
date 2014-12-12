#'
#'@title Post a new watershed shape to a site on SB
#'
#'@param site The unique site identifie?r
#'@param files String vector of files that make up the shapefile
#'@param session A SB session from \link{authenticate_sb}
#'
#'@return Returns SB id of newly created watershed item
#'
#'@author Luke Winslow
#'
#'
#'
#'@export
post_watershed = function(site, files, session){
	#parent as site, scheme, type=watershed, key = site, title=watershed

	##TODO: Check input
	
	#Check that it doesn't already exist
	check = query_item_identifier('mda_streams','watershed', site, session)
	
	if(nrow(check) > 0){
		stop('Watershed item already exists for this site')
	}
	
	#get site_root
	site_root = query_item_identifier(scheme='mda_streams', type='site_root',
												key=site, session)
	
	if(nrow(site_root) != 1){
		stop('Site root not found or ambiguous, check site ID:', site)
	}
	
	ws_id = item_upload_create(site_root$id, files=files, session=session)
	
	item_update_identifier(ws_id, scheme='mda_streams', type='watershed',
												 key=site, session=session)
	
	return(ws_id)
	
}