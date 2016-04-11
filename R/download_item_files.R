#' Download file[s] to local destination
#' 
#' Download one or more files to a user-specified (or temp file) location
#' 
#' @param item_ids the item id[s], or NA if an id could not be found. an item_id
#'   will be ignored in the very special (but potentially common and speedy) 
#'   case that \code{files} is specified (not NA), all specified files for an 
#'   item are already available locally, and on_local_exists=='skip'.
#' @param item_names character names by which to refer to the respective items 
#'   in messages
#' @param folder string for a folder location where files should be saved, or a 
#'   vector of as many folders as there are item_ids
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
download_item_files <- function(
  item_ids, item_names, files=NA, folder = tempdir(), 
  on_remote_missing=c("stop","return_NA"), on_local_exists=c("stop","skip","replace")) {
  
  # check inputs & session
  on_local_exists <- match.arg(on_local_exists)
  on_remote_missing <- match.arg(on_remote_missing)
  folder <- gsub("\\", "/", folder, fixed=TRUE)
  if(length(folder)==1) folder <- rep(folder, length(item_ids))
  if(!is.list(files)) files <- rep(list(files),length(item_ids))
  if(length(unique(c(length(item_ids), length(item_names), length(files), length(folder)))) != 1)
    stop("item_ids, item_names, files, and folder (after replicated) must all have the same length")
  
  # loop through items, downloading each file and returning a file path or NA
  # for each. collect the outputs in a character vector.
  unlist(lapply(seq_along(item_ids), function(arg_num) {
    item_id <- item_ids[arg_num]
    item_name <- item_names[arg_num]
    file_vec <- files[[arg_num]]
    dest_fold <- folder[arg_num]
        
    # return right away if files are all already known to be present and
    # on_local_exists is 'skip' or 'stop'
    if(!isTRUE(is.na(file_vec)) && on_local_exists %in% c("stop","skip")) {
      destination <- file.path(dest_fold, file_vec)
      known_to_exist <- file.exists(destination)
      if(any(known_to_exist) && on_local_exists=='stop')
        stop("for ", item_name, ", download destination already has a file and on_local_exists=='stop'")
      if(all(known_to_exist) && on_local_exists=='skip')
        return(destination)
    }
    
    # skip or stop if item is unavailable
    if(is.na(item_id)) {
      switch(
        on_remote_missing,
        "return_NA"=return(NA_character_),
        "stop"=stop("item unavailable on ScienceBase: ", item_name))
    }
    
    # find filename[s] for download
    file_list <- item_list_files(item_id)
     
    # skip or stop if files are unavailable
    if(nrow(file_list) == 0) {
      switch(
        on_remote_missing,
        "return_NA"=return(NA_character_),
        "stop"=stop("couldn't find any files in item ", item_name, " (", item_id, ")"))
    }
    
    # look for specific files
    if(!isTRUE(is.na(file_vec))) {
      if(!all(found_file <- file_vec %in% file_list$fname)) {
        msg <- paste0("found item but not file[s] in item ", item_name, " (", item_id, "): ", paste0(file_vec[!found_file], collapse=", "))
        switch(
          on_remote_missing,
          "return_NA"={warning(msg); return(NA_character_)},
          "stop"=stop(msg))
      }
    } else {
      file_vec <- file_list$fname
    }
    
    # do the downloading
    destination <- file.path(dest_fold, file_vec)
    out_destination <- sapply(1:length(destination), function(d) {
      if(file.exists(destination[d]) && on_local_exists=="skip") {
        destination[d]
      } else {
        item_file_download(sb_id=item_id, names=file_vec[d], destinations=destination[d], overwrite_file=TRUE)
      }
    })
    
    # return the file path if we successfully downloaded or skipped the download
    if(isTRUE(all(out_destination == destination)) || is.na(out_destination)) {
      return(destination)
    } else {
      stop("download failed for ", item_name, " (", item_id, ")")
    }
  }))
}