#' Repair a metab_model item that is missing its identifier tags
#' 
#' Sometimes items/files get posted but the identifiers don't get updated, 
#' making it harder to search for the ts item. This function gives ScienceBase 
#' another chance to add tags.
#' 
#' @param model_name one or more model_name strings, e.g., "nwis_05515500-35-150729 0.1.2 compare_par_srces"
#' @param verbose should status messages be given?
#' @param limit the maximum number of items to return in the SB query
#'   
#' @import sbtools
#' @importFrom stats setNames
#' @export
repair_metab_model <- function(model_name, verbose=FALSE, limit=5000) {
  
  # check the session; we'll need write access
  sb_require_login("stop")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    model_name=model_name,
    mm_id_tag=locate_metab_model(model_name=model_name, by='tag', limit=limit),
    mm_id_dir=locate_metab_model(model_name=model_name, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$mm_id_dir))) {
    warning("couldn't find the metab_model item for\n", 
            paste(query_args[bad_rows,'model_name'], collapse=" or\n"),
            ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$mm_id_dir), function(arg) {
    # unpackage the df row
    model_name <- query_args[arg, "model_name"]
    mm_id_tag <- query_args[arg, "mm_id_tag"]
    mm_id_dir <- query_args[arg, "mm_id_dir"]
    
    # if we found the ts by tags, we're already good to return
    if(!is.na(mm_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type='metab_model', scheme=get_scheme(), key=model_name)
    if(verbose) message(
      'setting identifiers for item ', mm_id_dir, ': \n',
      '  scheme = "', idlist$scheme, '"\n  type   = "', idlist$type, '"\n  key    = "', idlist$key, '"')
    tryCatch(
      item_update_identifier(sb_id=mm_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(mm_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_metab_model('", model_name, "')")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}