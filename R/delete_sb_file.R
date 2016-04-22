#' Delete a single file from a ScienceBase item
#' 
#' Light wrapper for \code{\link[sbtools]{item_rm_files}}: loops over multiple
#' sb_id,file pairs if given. For timeseries files, sb_id and file can be
#' determined using summarize_ts_files(). See examples
#' 
#' @param sb_id character sciencebase id
#' @param files character filename[s] to delete
#' @param verbose logical. Should status messages be given?
#' @examples
#' \dontrun{
#' oops <- summarize_ts_files('dosat_calcGGbts') %>% 
#'   filter(upload_date > as.POSIXct('2016-02-18'), 
#'     upload_date < as.POSIXct('2016-02-20'), 
#'     version=='rds') %>%
#'   select(sb_id=ts_item, files=file_name)
#' mda.streams:::delete_sb_file(oops$sb_id, oops$files)
#' }
#' @keywords internal
delete_sb_file <- function(sb_id, files, verbose=TRUE) {
  params <- data.frame(sb_id=sb_id, files=files, success=NA, errors=NA, stringsAsFactors=FALSE)
  for(i in 1:nrow(params)) {
    if(verbose) message('deleting file ', params[i,'files'], ' from item ', params[i,'sb_id'])
    params[i,'success'] <- tryCatch({
      item_rm_files(sb_id=params[i,'sb_id'], files=params[i,'files'])
      TRUE
    }, error=function(e) {
      message("  deletion failed; see output$errors for details")
      params[i,'errors'] <- e$message 
      FALSE
    })
  }
  params
}