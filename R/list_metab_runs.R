#' List the available metabolism runs
#' 
#' @return a character vector of titles of the metab_runs posted on SB
#' @import sbtools
#' @export
list_metab_runs = function(){
  sb_require_login("stop")
  run_items <- query_item_identifier(scheme=get_scheme(), type='metab_run', limit = 10000)
  if(length(run_items) > 0) {
    return(sapply(run_items, function(item) item$title))
  } else {
    return(character())
  }
}

#' List the files stored in a metabolism runs
#' 
#' @param title a metab_run title
#' @param out the columns to return
#' @return dataframe of file names and sizes within a metab_run
#' @import sbtools
#' @importFrom stats setNames
#' @export
list_metab_run_files <- function(title, out=c("filename","size_bytes","url")) {
  sb_require_login("stop")
  out <- lapply(title, function(t) {
    run_id <- locate_metab_run(t)
    filedf <- setNames(item_list_files(run_id),c("filename","size_bytes","url"))
    filedf[,out]
  })
  if(length(out)==1) return(out[[1]]) else return(out)
}
