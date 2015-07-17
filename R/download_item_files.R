#' Download metadata data to local file destination
#' 
#' Download a timeseries file to a user-specified (or temp file) location
#' 
#' @param item_ids the item id[s]
#' @param item_names character names by which to refer to the respective items 
#'   in messages
#' @param folder string for a folder location where files should be saved
#' @param files list of character vector[s] of file names to download, one list 
#'   element per item_ids. if the default is used, all files from each site will
#'   be downloaded. may also be passed as a single character vector to be
#'   replicated once for each item_id
#' @param on_remote_missing character indicating what to do if the
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Alison P Appling, Corinna Gries, Jordan S Read, Luke A Winslow
#' @examples
#' \dontrun{
#' download_item_files(item_id, on_local_exists="replace")
#' }
#' @import sbtools
#' @export
download_item_files <- function(
  item_ids, item_names, files=NA, folder = tempdir(), 
  on_remote_missing=c("stop","return_NA"), on_local_exists=c("stop","skip","replace")) {
  
  # check inputs & session
  on_local_exists <- match.arg(on_local_exists)
  on_remote_missing <- match.arg(on_remote_missing)
  folder <- gsub("\\", "/", folder, fixed=TRUE)
  if(!is.list(files)) files <- rep(list(files),length(item_ids))
  if(length(unique(c(length(item_ids), length(item_names), length(files)))) != 1)
    stop("item_ids, item_names, files/list(files), and num_expected must all have the same length")
  
  # loop through items, downloading each file and returning a file path or NA
  # for each. collect the outputs in a character vector.
  sapply(seq_along(item_ids), function(arg_num) {
    item_id <- item_ids[arg_num]
    item_name <- item_names[arg_num]
    file_vec <- files[[arg_num]]
    
    # skip or stop if item is unavailable
    if(is.na(item_id)) {
      switch(
        on_remote_missing,
        "return_NA"=return(as.character(NA)),
        "stop"=stop("item unavailable on ScienceBase: ", item_name))
    }
    
    # find file name for download (get filename)
    file_list = item_list_files(item_id)
     
    # skip or stop if files are unavailable
    if(nrow(file_list) == 0) {
      switch(
        on_remote_missing,
        "return_NA"=return(as.character(NA)),
        "stop"=stop("couldn't find any files in item ", item_name, " (", item_id, ")"))
    }
    
    # look for specific files
    if(!isTRUE(is.na(file_vec))) {
      if(!all(found_file <- file_vec %in% file_list$fname)) 
        stop("couldn't find file[s] in item ", item_name, " (", item_id, "): ", paste0(file_vec[!found_file], collapse=", "))
    } else {
      file_vec <- file_list$fname
    }
    
    # do the downloading
    destination  = file.path(folder, file_vec)
    out_destination <- 
      if(file.exists(destination) && on_local_exists %in% c("stop","skip")) {
        switch(
          on_local_exists,
          "stop"=stop("for ", item_name, ", download destination already has a file and on_local_exists=='stop'"),
          "skip"=NA )
      } else if(!file.exists(destination) || on_local_exists=="replace") {
        item_file_download(id=item_id, names=file_vec, destinations=destination, overwrite_file=TRUE)
      } else {
        stop("unexpected destination file condition or on_local_exists value")
      }
    
    # return the file path if we successfully downloaded or skipped the download
    if(isTRUE(out_destination) || is.na(out_destination)) {
      return(destination)
    } else {
      stop("download failed for ", item_name, " (", item_id, ")")
    }
  })
}