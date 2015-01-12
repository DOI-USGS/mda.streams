#'@title download timeseries data to local file destination
#'@description download a timeseries file to a user-specified (or temp file) location
#'@param site a valid mda.streams site (see \link{get_sites})
#'@param variable a valid variable name for timeseries data (see \link{get_ts_variables})
#'@param destination string for a folder location
#'@param session a valid sciencebase session (see \code{\link[sbtools]{authenticate_sb}}). 
#'Set \code{session = NULL} (default) for sites on sciencebase that are public.
#'@return file handle for downloaded file
#'@author Corinna Gries, Jordan S Read, Luke A Winslow
#'@examples
#'\dontrun{
#'download_ts(site = 'nwis_01018035', variable = 'doobs')
#'}
#'@import sbtools R.utils tools
#'@export
download_ts=function(site, variable, destination = NULL, session = NULL){
  
  ts_variable <- make_ts_variable(variable)
  
  # find item ID for download 
  item = query_item_identifier(scheme='mda_streams',type=ts_variable, 
                               key=site, session=session)
  
  # find file name for download (get filename)
  file_list = item_list_files(item$id, session)
  
  #check how many file names are coming back, we need only one  
  if(nrow(file_list) > 1){
    stop("There are more than one files in this item")
  }
  
  if(nrow(file_list) < 1){
    stop("There is no file available in this item")
  }
  
  if(is.null(destination)){
    destination  = file.path(tempdir(), paste0(paste(site, ts_variable, sep = "_" ), get_ts_extension()))
  }

  #set the intermediate (staging) destination for downloaded gzip file
  if (isGzipped(file_list$url)){
    stage_file <- tempfile(fileext = paste0(get_ts_extension(), '.gz'))
    stage_file = item_file_download(item$id, file_list$fname, stage_file)
    out_destination = gunzip(stage_file, destname = destination,
                             temporary = FALSE, skip = FALSE, overwrite = FALSE, remove = TRUE, BFR.SIZE = 1e+07)
  } else {
    out_destination = item_file_download(item$id, file_list$fname, destination)
    
  }
  
  if(out_destination){
    return(destination)
  } else {
    stop('download failed')
  }
  
  
}