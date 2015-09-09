#' Repair a ts item that is missing its identifier tags
#' 
#' Sometimes ts items/files get posted but the identifiers don't get updated, 
#' making it harder to search for the ts item. This function gives ScienceBase 
#' another chance to add tags.
#' 
#' @param var_src one or more var_src strings, e.g., "doobs_nwis"
#' @param site_name one or more site names, e.g., "nwis_040871488"
#' @param limit the maximum number of items to return in the SB query to find 
#'   all listed var_src:site_name combinations
#'   
#' @import sbtools
#' @importFrom stats setNames
#' @export
#' 
#' @examples 
#' \dontrun{
#' repair_ts("wtr_nwis", "nwis_01374019")
#' }
repair_ts <- function(var_src, site_name, limit=5000) {

  # warn if the var_src shouldn't be there
  verify_var_src(var_src, on_fail=warning)
  
  # check the session; we'll need write access
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    var_src=var_src, site_name=site_name, 
    var_src_site=paste0(var_src, "-", site_name),
    ts_id_tag=locate_ts(var_src=var_src, site_name=site_name, by='tag', limit=limit),
    ts_id_dir=locate_ts(var_src=var_src, site_name=site_name, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$ts_id_dir))) {
    warning("couldn't find the ts for\n", 
            paste(query_args[bad_rows,'var_src_site'], collapse=" or\n"),
            ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$ts_id_dir), function(arg) {
    # unpackage the df row
    var_src <- query_args[arg, "var_src"]
    site_name <- query_args[arg, "site_name"]
    ts_id_tag <- query_args[arg, "ts_id_tag"]
    ts_id_dir <- query_args[arg, "ts_id_dir"]
    
    # if we found the ts by tags, we're already good to return
    if(!is.na(ts_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type=make_ts_name(var_src), scheme=get_scheme(), key=site_name)
    tryCatch(
      item_update_identifier(id=ts_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(ts_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_ts('", var_src, "', '", site_name, ")")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}