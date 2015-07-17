#' Download metabolism run data data to local file destination
#' 
#' Download all files stored in a metabolism run SB item to a user-specified (or
#' temp file) location. A new folder will be created within that location to
#' store the downloaded files.
#' 
#' @param title the title of the metabolism run (date, tag, and strategy 
#'   separated by spaces)
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
download_metab_run <- function(title, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  ids <- locate_metab_run(title)
  
  sapply(1:length(title), function(i) {
    
    # make inner folder for this run, if permitted
    inner_folder <- file.path(folder, title[i])
    if(dir.exists(inner_folder)) {
      switch(
        on_local_exists,
        'stop'=stop("destination folder ", title[i]," already exists within folder ", folder, " and on_local_exists=='stop'"),
        'skip'=return(NA),
        'replace'={
          file.remove(inner_folder)
        }
      )
    }
    dir.create(inner_folder, showWarnings=TRUE)
    
    # download files into the inner folder
    download_item_files(
      item_ids=ids[i], item_names=title[i], files=NA, folder=inner_folder, 
      on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
    
    # return the folder path
    inner_folder
  })
}