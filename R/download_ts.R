#'@title download timeseries data to local file destination
#'@description download a timeseries file to a user-specified (or temp file) location
#'@param site a valid mda.streams site (see \link{get_sites})
#'@param variable a valid variable name for timeseries data
#'@param destination string for a folder location
#'@param session a valid sciencebase session (see \code{\link[sbtools]{authenticate_sb}}). 
#'Set \code{session = NULL} (default) for sites on sciencebase that are public.
#'@return file handle for downloaded file
#'@examples
#'\dontrun{
#'download_ts(site = 'nwis_01018035', variable = 'doobs')
#'}
#'@import sbtools
#'@export
download_ts = function(site, variable, destination = NULL, session = NULL){
  
  if (is.null(destination)){
    file_handle = tempfile(fileext = '.tsv')
  } else {
    file_handle = paste0(destination,site,'.tsv')
  }
  
  type <- paste0('ts_', variable)
  
  item <- query_item_identifier(scheme = 'mda_streams', type = type, key = site, session = session, limit = 5)
  id <- item$id
  
  file_names <- item_list_files(id, session)
  if (nrow(file_names) > 1){
    stop('item ', item, ' has more than one associated file for ', variable)
  }
  
  file_get <- item_file_download(id = id, names = file_names$fname, destination = file_handle, session = session)
  
  if (file_get){
    return(file_handle)
  } else {
    warning('file failed to download')
    return(NULL)
  }
  
  
}