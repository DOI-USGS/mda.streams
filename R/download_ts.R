#'@title download timeseries data to local file destination
#'@description download a timeseries file to a user-specified (or temp file) location
#'@param site a valid mda.streams site (see \link{get_sites})
#'@param variable a valid variable name for timeseries data (see \link{get_ts_variables})
#'@param folder string for a folder location
#'@param ... additional arguments passed to \code{\link[sbtools]{session_check_reauth}} 
#'
#'@return file handle for downloaded file
#'@author Corinna Gries, Jordan S Read, Luke A Winslow
#'@examples
#'\dontrun{
#'download_ts(site = 'nwis_06893300', variable = 'doobs')
#'}
#'@import sbtools 
#'@import tools
#'@export
download_ts=function(site, variable, folder = tempdir(), ...){

  ts_variable <- paste0(pkg.env$ts_prefix, variable)
  scheme = get_scheme()
  
  session_check_reauth(...)

  # find item ID for download 
  item = query_item_identifier(scheme=scheme,type=ts_variable, 
                               key=site)
  
  # find file name for download (get filename)
  file_list = item_list_files(item$id)
  
  #check how many file names are coming back, we need only one  
  if(nrow(file_list) > 1){
    stop("There are more than one files in this item")
  }
  
  if(nrow(file_list) < 1){
    stop("There is no file available in this item")
  }

  destination  = file.path(folder, file_list$fname)
  out_destination = item_file_download(id = item$id, names = file_list$fname, destinations = destination)
  
  if(out_destination){
    return(destination)
  } else {
    stop('download failed')
  }
  
  
}