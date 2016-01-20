#' Download timeseries data to local file destination
#' 
#' Download a timeseries file to a user-specified (or temp file) location
#' 
#' @param site_name a valid mda.streams site (see \link{get_sites})
#' @param var_src a valid variable name for timeseries data (see 
#'   \code{dplyr::select(dplyr::filter(var_src_codes, data_type=='ts'), 
#'   var_src)})
#' @param folder string for a folder location
#' @param on_remote_missing character indicating what to do if the remote file 
#'   is missing
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Alison P Appling, Corinna Gries, Jordan S Read, Luke A Winslow
#' @examples
#' \dontrun{
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300')
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300')
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300', 
#'   on_local_exists="skip")
#' }
#' @import sbtools
#' @export
download_ts <- function(var_src, site_name, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  # check inputs & session
  on_local_exists <- match.arg(on_local_exists)
  on_remote_missing <- match.arg(on_remote_missing)
  folder <- gsub("\\", "/", folder, fixed=TRUE)
  
  # combine var_src and site_name into data.frame in case they're of unequal
  # length and using the auto-replicate feature
  needs <- data.frame(
    var_src=var_src, 
    site_name=site_name, 
    item=NA,
    download_success=FALSE,
    dest=make_ts_path(site_name, make_ts_name(var_src), folder),
    stringsAsFactors=FALSE)
  # identify any items that have already been downloaded to skip if necessary
  needs$local_exists <- file.exists(needs$dest)
  
  # find item IDs for download. only make this call for rows where the local
  # file doesn't exist or will be replaced
  if(on_local_exists %in% c("stop","skip")) {
    if(length(which(!needs$local_exists))> 0) {
      needs[!needs$local_exists, "item"] <- 
        locate_ts(var_src=needs[!needs$local_exists,"var_src"], site_name=needs[!needs$local_exists,"site_name"])
    }
  } else {
    needs$item <- locate_ts(var_src=needs$var_src, site_name=needs$site_name)
  }
  
  # loop through items, downloading each file and returning a file path or NA
  # for each. collect the outputs in a character vector.
  sapply(1:nrow(needs), function(item_num) {
    item <- needs[item_num, "item"]
    item_name <- paste0(site_name[item_num], '-', var_src[item_num])
    
    # check for a local file, then download if needed
    if(needs[item_num, "local_exists"] && on_local_exists %in% c("stop","skip")) {
      # no need to download
      if(on_local_exists=="stop") 
        stop("for ", item_name, ", download destination already has a file and on_local_exists=='stop'")
      # on_local_exists=="skip"
      needs[item_num, "download_success"] <- NA
      
    } else if(!needs[item_num, "local_exists"] || on_local_exists=="replace") {
      # yep, download the file
      
      # skip or stop if item is unavailable
      if(is.na(needs[item_num, "item"])) {
        switch(
          on_remote_missing,
          "return_NA"={
            needs[item_num, "dest"] <<- NA
            needs[item_num, "download_success"] <<- NA
          },
          "stop"=stop("ts item unavailable on ScienceBase: ", item_name))
      } else {
        # find file name for download (get filename)
        file_list <- item_list_files(needs[item_num, "item"])
        
        # check how many file names are coming back; we need exactly one  
        if(nrow(file_list) != 1) {
          stop("there are ", nrow(file_list), " files in SB item: ", item_name, "; need exactly 1")
        }
        if(file_list$fname != basename(needs[item_num, "dest"])) {
          stop("expected SB filename (",file_list$fname,") to equal destination filename (",
               basename(needs[item_num, "dest"]),")")
        }
        
        # do the downloading
        needs[item_num, "download_success"] <- isTRUE(item_file_download(
          sb_id=needs[item_num, "item"], names=file_list$fname, 
          destinations=needs[item_num, "dest"], overwrite_file=TRUE) == needs[item_num, "dest"])
      }
    } else {
      stop("unexpected destination file condition or on_local_exists value")
    }
    
    # return the file path if we successfully downloaded or skipped the download
    if(!isTRUE(needs[item_num, "download_success"]) && !is.na(needs[item_num, "download_success"])) {
      stop("download failed for ", item_name)
    }
  }) 
  needs$dest
}