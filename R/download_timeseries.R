#'
#'@title Download timeseries to local computer
#'
#'
#'@param site A unique site ID eg [NWIS_392572094]
#'@param variable Timeseries identifiying string eg [ts_stage, ts_doobs, ts_wtr, etc]
#'@param destination Full folder path and file name
#'@param session Session object from \link{authenticate_sb}
#'
#'@author Corinna Gries
#'
#'
#'
#'@import sbtools
#'@export
download_timeseries=function(site, variable, destination=NULL, session=NULL){
  
  if(is.null(destination)){
    destination = tempfile(pattern=paste(site, variable, sep="" ), 
                           tmpdir = tempdir(), fileext = ".tsv")
  }
  
 # find item ID for download 
  item = query_item_identifier(scheme='mda_streams',type=variable, 
                               key=site, session=session)
  
  # find file name for download
  file_list=item_list_files(item$id, session)
  
  file_list$fname
    
  if(nrow(file_list) > 1){
    stop("There are more than one files in this item")
  }
  
  if(nrow(file_list) < 1){
    stop("There is no file available in this item")
  }
  
  item_file_download(item$id, file_list$fname, destination)
  
    
  return(destination)
  
}