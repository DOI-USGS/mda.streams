#' Download timeseries data to local file destination
#' 
#' Download a timeseries file to a user-specified (or temp file) location
#' 
#' @param site_name a valid mda.streams site (see \link{list_sites})
#' @param var_src a valid variable name for timeseries data (see 
#'   \code{dplyr::select(dplyr::filter(var_src_codes, data_type=='ts'), 
#'   var_src)})
#' @param folder string for a folder location
#' @param version character string indicating whether you want to download the 
#'   ts as a .tsv or .RData
#' @param on_remote_missing character indicating what to do if the remote file 
#'   is missing
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @return file handle (character path) for the downloaded file, or NA if the 
#'   timeseries is unavailable on ScienceBase
#'   
#' @import dplyr
#'   
#' @author Alison P Appling, Corinna Gries, Jordan S Read, Luke A Winslow
#' @examples
#' \dontrun{
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300')
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300')
#' download_ts(var_src = c('doobs_nwis','baro_nldas'), site_name = 'nwis_06893300', 
#'   on_local_exists="skip")
#' }
#' @export
download_ts <- function(var_src, site_name, folder = tempdir(), version=c('tsv','rds','RData'),
                        on_remote_missing=c("stop","return_NA"), 
                        on_local_exists=c("stop","skip","replace")) {
  
  # mda.streams:::sb_require_login("continue", verbose=TRUE) # no login is fine for tses
  
  version <- match.arg(version)
  on_remote_missing <- match.arg(on_remote_missing)
  on_local_exists <- match.arg(on_local_exists)
  
  files <- dests <- '.dplyr.var'
  params <- 
    data_frame(
      var_src = var_src,
      site_name = site_name,
      files = make_ts_path(site_name, make_ts_name(var_src), version=version),
      folder = folder) %>%
    mutate(
      dests = file.path(folder, files),
      need = if(on_local_exists %in% c('stop','skip')) !file.exists(dests) else TRUE,
      item_names <- sapply(files, function(f) strsplit(f, ".", fixed=TRUE)[[1]][1], USE.NAMES=FALSE), # strip the file extension
      ids = 'local_exists'
    )
  
  # only query SB for those ids we actually need
  if(length(which(params$need)) > 0) {
    params$ids[params$need] <- locate_ts(params$var_src[params$need], params$site_name[params$need])
  }
  
  # download. this function will skip over ids we don't need
  download_item_files(
    item_ids=params$ids, item_names=params$item_names, files=as.list(params$files), folder=params$folder, 
    on_remote_missing=on_remote_missing, on_local_exists=on_local_exists)
}
