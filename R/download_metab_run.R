#' Download metabolism run data data to local file destination
#' 
#' Download all files stored in a metabolism run SB item to a user-specified (or
#' temp file) location. A new folder will be created within that location to
#' store the downloaded files.
#' 
#' @param title the title of the metabolism run (date, tag, and strategy 
#'   separated by spaces)
#'   @param files NA for all files, or a vector of specific filenames (character) to download
#' @inheritParams download_item_files
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Alison P Appling
#' @examples
#' \dontrun{
#' download_ts(var_src = 'doobs_nwis', site_name = 'nwis_06893300')
#' }
#' @import sbtools
#' @export
download_metab_run <- function(title, files=NA, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before downloading a metab_run")
  
  on_remote_missing <- match.arg(on_remote_missing)
  on_local_exists <- match.arg(on_local_exists)
  
  ids <- locate_metab_run(title)
  
  c(sapply(1:length(title), function(i) {
  
    if(is.na(ids[i])) {
      switch(
        on_remote_missing,
        "return_NA"=return(as.character(NA)),
        "stop"=stop("metab_run unavailable on ScienceBase: ", title[i]))
    }
    
    # make inner folder for this run, if permitted
    inner_folder <- file.path(folder, title[i])
    if(!dir.exists(inner_folder)) {
      dir.create(inner_folder, showWarnings=TRUE)
    }
    if(isTRUE(is.na(files))) {
      files <- list_metab_run_files(title, out="filename")
    }
    file_dests <- file.path(inner_folder, files)
    if(any(local_exists <- file.exists(file_dests))) {
      message("destination file[s] ", paste0(file_dests[local_exists],collapse=" & "), " already exist")
      switch(
        on_local_exists,
        'stop'=stop("file[s] exist and on_local_exists=='stop'"),
        'skip'=if(any(!local_exists)) files[local_exists] <- NA else return(file_dests),
        'replace'={
          file.remove(file_dests[local_exists])
        }
      )
    }
    
    # download files into the inner folder
    download_item_files(
      item_ids=ids[i], item_names=title[i], files=na.omit(files), folder=inner_folder, 
      on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
    
    # return the folder path
    file_dests
  }))
}