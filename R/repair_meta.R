#' Repair a meta item that is missing its identifier tags
#' 
#' Sometimes ts items/files get posted but the identifiers don't get updated, 
#' making it harder to search for the item. This function gives ScienceBase 
#' another chance to add tags.
#' 
#' @param type one or more meta types, e.g., "basic"
#' @param limit the maximum number of items to return in the SB query to find 
#'   all listed var_src:site_name combinations
#'   
#' @import sbtools
#' @export
#' 
#' @examples 
#' \dontrun{
#' repair_meta("basic")
#' }
repair_meta <- function(type, limit=5000) {
  
  # check the session; we'll need write access
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    type=type, 
    meta_type=paste0("meta_", type),
    meta_id_tag=locate_meta(type=type, by='tag', limit=limit),
    meta_id_dir=locate_meta(type=type, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$meta_id_dir))) {
    warning("couldn't find the metadata file for\n", 
            paste(query_args[bad_rows,'meta_type'], collapse=" or\n"),
            ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$meta_id_dir), function(arg) {
    # unpackage the df row
    type <- query_args[arg, "type"]
    meta_type <- query_args[arg, "meta_type"]
    meta_id_tag <- query_args[arg, "meta_id_tag"]
    meta_id_dir <- query_args[arg, "meta_id_dir"]
    
    # if we found the metafile by tags, we're already good to return
    if(!is.na(meta_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type="sites_meta", scheme=get_scheme(), key=meta_type)
    tryCatch(
      item_update_identifier(id=meta_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(meta_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_meta('", type, "')")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}