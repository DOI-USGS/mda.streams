#' Download metabolism run data data to local file destination
#' 
#' Download files stored in a metabolism run SB item to a user-specified (or 
#' temp file) location. A new folder will be created within that location to 
#' store the downloaded files. The default is to download all available files,
#' but specific files may be specified with the \code{files} argument.
#' 
#' @param title the title of the metabolism run (date, tag, and strategy 
#'   separated by spaces)
#' @param files NA for all files, or a list of vectors of specific filenames 
#'   (character) to download, one vector per title
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
#' @importFrom stats na.omit
#' @export
download_metab_run <- function(title, files=NA, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  
  sb_require_login("stop")
  
  on_remote_missing <- match.arg(on_remote_missing)
  on_local_exists <- match.arg(on_local_exists)
  
  # prepare the item download specs
  ids <- locate_metab_run(title=title)
  if(any(is.na(ids))) stop("could not locate the metab_run; check title")
  if(is.na(files)) {
    files <- lapply(ids, function(id) item_list_files(id)$fname)
  } else {
    if(!is.list(files) || any(sapply(files, function(f) class(f)[1] != 'character'))) 
      stop("files should be a list of character vectors")
    if(length(files) != length(title)) 
      stop("length(files) should equal length(title)")
  }
  item_names <- title

  # make inner folder[s] for this run
  inner_folder <- file.path(folder, title)
  for(infold in inner_folder) {
    dir.create(infold, showWarnings = FALSE)
  }
  
  download_item_files(
    item_ids=ids, item_names=item_names, files=files, folder=inner_folder, 
    on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
  
}