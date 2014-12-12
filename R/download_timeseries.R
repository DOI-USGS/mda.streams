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
#'@import sbtools R.utils
#'@export
download_timeseries=function(site, variable, destination=NULL, session=NULL){
  
 # find item ID for download 
  item = query_item_identifier(scheme='mda_streams',type=variable, 
                               key=site, session=session)
  
  # find file name for download
  file_list=item_list_files(item$id, session)
  
  #check how many file names are coming back, we need only one  
  if(nrow(file_list) > 1){
    stop("There are more than one files in this item")
  }
  
  if(nrow(file_list) < 1){
    stop("There is no file available in this item")
  }
  
  #make sure we have two destinations, one for the downloaded gzip file and one for the unzipped
  if(is.null(destination)){
    destination = tempfile(pattern=paste(site, variable, sep="" ), 
                                 tmpdir = tempdir(), fileext = ".gz")
  }else{
    final_desitnation = destination
  }
  
  #set the intermediate destination for downloaded gzip file
  destination = tempfile(pattern=paste(site, variable, sep="" ), 
                         tmpdir = tempdir(), fileext = ".gz")
  
  
  item_file_download(item$id, file_list$fname, destination)
  
  out_destination = gunzip(destination, destname=final_destination,
         temporary=FALSE, skip=FALSE, overwrite=FALSE, remove=TRUE, BFR.SIZE=1e+07)
  
  
  #unzip file and save to final destination
  file_df = read.table(gzip(destination))
    
  return(out_destination)
  
}