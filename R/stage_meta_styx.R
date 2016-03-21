#' Create/modify the meta_styx file
#' 
#' @param rows a data frame with one or more rows, having the same columns as 
#'   the existing meta_ file (if there is one). These rows will be inserted 
#'   according to their site_name values.
#' @param on_exists character indicating how to handle new rows whose site_name 
#'   is already present in the current meta_ file If 'stop', an error will be 
#'   thrown if even one value is duplicated. If 'replace', a message will be 
#'   given and existing rows with site_name in rows$site_name will be replaced
#'   by the new rows.
#' @param folder where to store the staged file
#' @param verbose logical. give status messages?
#' @importFrom unitted u write_unitted is.unitted rbind.unitted
#' @import dplyr
#' @export
stage_meta_styx <- function(rows, on_exists=c("stop","replace"), folder = tempdir(), verbose = FALSE) {
  
  if(!is.unitted(rows)) stop("rows must be unitted")
  rows <- u(
    as.data.frame(lapply(rows, function(col) if(is.factor(col)) as.character(col) else col), stringsAsFactors=FALSE),
    get_units(rows))
  
  if('styx' %in% list_metas()) {
    # get existing metadata
    old_meta <- read_meta(download_meta('styx', on_local_exists = 'replace'))
    
    # quick & unhelpful compatibility check
    if(!all.equal(names(old_meta), names(rows))) 
      stop("column names of old and new metadata don't match")
    
    # check for conflicting rows and stop or replace
    replacements <- old_meta$site_name[which(old_meta$site_name %in% rows$site_name)]
    if(length(replacements) > 0) {
      on_exists <- match.arg(on_exists)
      if(on_exists=="stop") stop("these sites are already in meta_styx: ", paste0(replacements, collapse=", "))
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
    fpath <- make_meta_path(type='styx', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(new_meta, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(fpath)
  }
  
}