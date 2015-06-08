#' @title download timeseries data to local file destination
#' @description download a timeseries file to a user-specified (or temp file) 
#'   location
#' @param site a valid mda.streams site (see \link{get_sites})
#' @param var_src a valid variable name for timeseries data (see 
#'   \code{get_var_codes(out='var_src', type='ts')})
#' @param folder string for a folder location
#' @param on_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @param ... additional arguments passed to 
#'   \code{\link[sbtools]{session_check_reauth}}
#'   
#' @return file handle for downloaded file
#' @author Corinna Gries, Jordan S Read, Luke A Winslow
#' @examples
#' \dontrun{
#' download_ts(site = 'nwis_06893300', var_src = 'doobs_nwis')
#' }
#' @import sbtools
#' @import tools
#' @export
download_ts=function(site, var_src, folder = tempdir(), on_exists=c("stop","skip","replace"), ...){

  # check inputs
  on_exists <- match.arg(on_exists)
  
  # check or authorize the session
  session_check_reauth(...)

  # find item ID for download 
  item = locate_ts(var_src=var_src, site_name=site)
  
  # find file name for download (get filename)
  file_list = item_list_files(item)
  
  # check how many file names are coming back; we need exactly one  
  if(nrow(file_list) > 1) {
    stop("There are more than one files in this item")
  } else if(nrow(file_list) < 1) {
    stop("There is no file available in this item")
  }

  destination  = file.path(folder, file_list$fname)
  out_destination <- 
    if(file.exists(destination) && on_exists %in% c("stop","skip")) {
      switch(
        on_exists,
        "stop"=stop("download destination already has a file and on_exists=='stop'"),
        "skip"=NULL)
    } else {
      # either on_exists="replace" or the file doesn't already exist; either way, overwrite_file=TRUE is fine
      item_file_download(id = item, names = file_list$fname, destinations = destination, overwrite_file=TRUE)
    }
  
  if(out_destination){
    return(destination)
  } else {
    stop('download failed')
  }
  
}