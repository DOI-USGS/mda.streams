#' @title download timeseries data to local file destination
#' @description download a timeseries file to a user-specified (or temp file) 
#'   location
#' @param site_name a valid mda.streams site (see \link{get_sites})
#' @param var_src a valid variable name for timeseries data (see 
#'   \code{get_var_codes(out='var_src', type='ts')})
#' @param folder string for a folder location
#' @param on_remote_missing character indicating what to do if the 
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @param ... additional arguments passed to 
#'   \code{\link[sbtools]{session_check_reauth}}
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Corinna Gries, Jordan S Read, Luke A Winslow, Alison P Appling
#' @examples
#' \dontrun{
#' download_ts(var_src = 'doobs_nwis', site_name = 'nwis_06893300')
#' }
#' @import sbtools
#' @import tools
#' @export
download_ts <- function(var_src, site_name, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace"), ...) {

  # check inputs & session
  on_local_exists <- match.arg(on_local_exists)
  on_remote_missing <- match.arg(on_remote_missing)
  session_check_reauth(...)

  # find item IDs for download
  items <- locate_ts(var_src=var_src, site_name=site_name)
  
  # loop through items, downloading each file and returning a file path or NA
  # for each. collect the outputs in a character vector.
  sapply(seq_along(items), function(item_num) {
    item <- items[item_num]
    item_name <- paste0(site_name[item_num], '-', var_src[item_num])
    
    # skip or stop if item is unavailable
    if(is.na(item)) {
      switch(
        on_remote_missing,
        "return_NA"=return(as.character(NA)),
        "stop"=stop("ts item unavailable on ScienceBase: ", item_name))
    }
    
    # find file name for download (get filename)
    file_list = item_list_files(item)
    
    # check how many file names are coming back; we need exactly one  
    if(nrow(file_list) != 1) {
      stop("there are ", nrow(file_list), " files in SB item: ", item_name, "; need exactly 1")
    }
    
    # do the downloading
    destination  = file.path(folder, file_list$fname)
    out_destination <- 
      if(file.exists(destination) && on_local_exists %in% c("stop","skip")) {
        switch(
          on_local_exists,
          "stop"=stop("for ", item_name, ", download destination already has a file and on_local_exists=='stop'"),
          "skip"=NA )
      } else if(!file.exists(destination) || on_local_exists=="replace") {
        item_file_download(id = item, names = file_list$fname, destinations = destination, overwrite_file=TRUE)
      } else {
        stop("unexpected destination file condition or on_local_exists value")
      }
    
    # return the file path if we successfully downloaded or skipped the download
    if(isTRUE(out_destination) || is.na(out_destination)) {
      return(destination)
    } else {
      stop("download failed for ", item_name)
    }
  })  
}