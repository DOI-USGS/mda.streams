#' Create/modify the meta_indy file
#' 
#' @param rows a data frame with one or more rows, having the same columns as 
#'   the existing meta_indy (if there is one). These rows will be inserted 
#'   according to their site_name values. Columns with names that overlap with 
#'   other metadata tables, as prefixed by the table name (e.g., 'basic.lat'), 
#'   will be copied to those tables when the corresponding stage_meta_... 
#'   function is called (except that this copying is currently only implemented 
#'   for the 'basic' metadata table).
#' @inheritParams stage_meta_styx
#' @export
stage_meta_indy <- function(
  rows=u(data.frame(
    site_name="indy_xxxx", long_name=as.character(NA), 
    lat=u(as.numeric(NA),'degN'), lon=u(as.numeric(NA),'degE'), #coord_datum=as.character(NA), 
    alt=u(as.numeric(NA),'ft'), #, alt_datum=as.character(NA), nhdplus_id=as.numeric(NA), nhdplus_id_confidence=as.character(NA)
    info=as.character(NA))), 
  on_exists=c("stop","replace"), folder = tempdir(), verbose = FALSE) {
  
  if(!is.unitted(rows)) stop("rows must be unitted")
  rows <- u(
    as.data.frame(lapply(rows, function(col) if(is.factor(col)) as.character(col) else col), stringsAsFactors=FALSE),
    get_units(rows))
  
  if('indy' %in% list_metas()) {
    # get existing metadata
    old_meta <- read_meta(download_meta('indy', on_local_exists = 'replace'))
    
    # quick & unhelpful compatibility check
    if(!all.equal(names(old_meta), names(rows))) 
      stop("column names of old and new metadata don't match")
    
    # check for conflicting rows and stop or replace
    replacements <- old_meta$site_name[which(old_meta$site_name %in% rows$site_name)]
    if(length(replacements) > 0) {
      on_exists <- match.arg(on_exists)
      if(on_exists=="stop") stop("these sites are already in meta_indy: ", paste0(replacements, collapse=", "))
      old_meta[match(replacements, old_meta$site_name), ] <- rows[match(replacements, rows$site_name), ]
      rows <- rows[!(rows$site_name %in% replacements),]
    }
    
    # add truly new rows
    new_meta <- rbind.unitted(old_meta, rows)
  } else {
    new_meta <- rows
  }
  
  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(new_meta)
  } else {
    fpath <- make_meta_path(type='indy', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(new_meta, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(fpath)
  }
  
}