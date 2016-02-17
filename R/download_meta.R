#' Download metadata data to local file destination
#' 
#' Download a metadata file to a user-specified (or temp file) 
#'   location
#' 
#' @param type the metadata type
#' @param folder string for a folder location
#' @param on_remote_missing character indicating what to do if the
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @author Corinna Gries, Jordan S Read, Luke A Winslow, Alison P Appling
#' @examples
#' \dontrun{
#' download_meta(type="basic", on_local_exists="replace")
#' }
#' @import sbtools
#' @import tools
#' @export
download_meta <- function(type, folder = tempdir(), 
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  # mda.streams:::sb_require_login("continue", verbose=TRUE) # no login is fine for site data (meta)
  
  on_remote_missing <- match.arg(on_remote_missing)
  on_local_exists <- match.arg(on_local_exists)
  
  files <- dests <- '.dplyr.var'
  params <- 
    data_frame(
      files = make_meta_path(type),
      folder = folder,
      type = type) %>%
    mutate(
      dests = file.path(folder, files),
      need = if(on_local_exists %in% c('stop','skip')) !file.exists(dests) else TRUE,
      ids = 'local_exists'
    )
  
  # only query SB for those ids we actually need
  if(length(which(params$need)) > 0) {
    params$ids[params$need] <- locate_meta(params$type[params$need])
  }
  
  # download. this function will skip over ids we don't need
  download_item_files(
    item_ids=params$ids, item_names=params$type, files=as.list(params$files), folder=params$folder, 
    on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
}
