#'
#'@title Post a new timeseries file to SB
#'
#'
#'@param site Unique site ID
#'@param data A data.frame containing the timeseries
#'@param session Session object from \link{authenticate_sb}
#'
#'@author Luke Winslow, Corinna Gries
#'
#'
#'@import sbtools
#'@examples
#'\dontrun{
#'df <- get_nwis_df(site = "06893820", variable_name = "doobs", p_code = "00300", 
#'                  startDate = '2014-01-01', endDate = '2014-02-01')
#'post_ts(site = "06893820", data = df)
#'}
#'
#'@export
post_ts = function(site, data, session){
	
	
  ts_varname <-  names(data)[-1]
	#check input
	## TODO: check input and format of DATA
	data[,1] <- strftime(data[,1], usetz = T, tz = 'UTC') # lock in the timezone. coerce to char
	#save data as a file
	
	fpath = tempfile(fileext = paste0('.',get_ts_extension(), '.gz'))
	
  gz1 <- gzfile(fpath, "w")
	write.table(data,  gz1, sep=get_ts_delim(), row.names=FALSE, quote = FALSE)
  close(gz1)
	
	#Check if item already exists
  if(item_exists(scheme='mda_streams',type=ts_varname, 
															 key=site, session=session)){
    stop('This Timeseries for this site already exists')
  }

	
	#find site root
	site_root = query_item_identifier(scheme='mda_streams', 
																		type='site_root', key=site, session=session)
	if(nrow(site_root) != 1){
		stop('There is no site root available for site:', site)
	}
	
	#Create item if it does not exist
	ts_item = item_create(parent_id=site_root$id, 
												title=ts_varname, session=session)
	
  #attach file to item
  item_append_file(ts_item, filename=fpath, session=session)
  
	#tag item with our special identifier
	item_update_identifier(ts_item, scheme='mda_streams', type=ts_varname, 
												 key=site, session=session)
	
	
	
	return(ts_item)
}