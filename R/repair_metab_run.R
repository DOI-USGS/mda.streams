#' Repair a metab_run item that is missing its identifier tags
#' 
#' Sometimes metab_run items/files get posted but the identifiers don't get
#' updated, making it harder to search for the item. This function gives
#' ScienceBase another chance to add tags.
#' 
#' @param title one or more metab titles, e.g., "150714 0.0.2 local_makefile_run"
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
repair_metab_run <- function(title, limit=5000) {
  
  # check the session; we'll need write access
  if(is.null(current_session()))
    stop("log in to repair data. see authenticate_sb()")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    run_title=title,
    run_id_tag=locate_metab_run(title=title, by='tag', limit=limit),
    run_id_dir=locate_metab_run(title=title, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$run_id_dir))) {
    warning("couldn't find the metab_run item for\n", 
            paste(query_args[bad_rows,'run_title'], collapse=" or\n"),
            ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$run_id_dir), function(arg) {
    # unpackage the df row
    run_title <- query_args[arg, "run_title"]
    run_id_tag <- query_args[arg, "run_id_tag"]
    run_id_dir <- query_args[arg, "run_id_dir"]
    
    # if we found the run by tags, we're already good to return
    if(!is.na(run_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type="metab_run", scheme=get_scheme(), key=run_title)
    tryCatch(
      item_update_identifier(id=run_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(run_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_metab_run('", type, "')")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}