#' Archive a timeseries file
#' 
#' Archiving means renaming a file to an archive name that reflects the file 
#' creation date. The file remains attached to the same timeseries item on 
#' ScienceBase.
#' 
#' For efficiency, e.g., when calling archive_ts() from another function that 
#' has already located the SB item, the ts_id and filename may be supplied with 
#' other arguments omitted. If ts_id and filename are omitted, the var_src, 
#' site, and version of the file to archive should be supplied instead, and this
#' function will locate the corresponding file[s] on ScienceBase.
#' 
#' @param var_src the var_src for which to archive a file
#' @param site_name the site for which to archive a file
#' @param version the data storage format of the file to be archived
#' @param ts_id character. the ScienceBase ID[s] containing file[s] to archive
#' @param filename character. the filename[s] to archive
#' @param verbose logical. give status messages?
#' @import sbtools
archive_ts <- function(var_src, site_name, version=c('tsv','rds'), ts_id, filename, verbose=TRUE) {
  
  # check session
  sb_require_login("stop")
  
  # fill in values for ts_id and/or filename as needed. this is vectorized
  if(missing(ts_id)) {
    ts_id <- locate_ts(var_src=var_src, site_name=site_name)
  }
  if(missing(filename)) {
    version <- match.arg(version)
    filename <- make_ts_path(site_name=site_name, ts_name=make_ts_name(var_var_src=var_src), version=version)
  }
  # package as data.frame to replicate as needed
  files <- data.frame(ts_id=ts_id, filename=basename(filename), stringsAsFactors=FALSE)
  
  # loop through to archive each file
  new_names <- sapply(setNames(1:nrow(files), files$filename), function(i) {
    
    if(is.na(files[i,'ts_id'])) {
      warning("SB item could not be located for ", files[i,'filename'], .call=FALSE)
      return(NA)
    }
    
    # determine the filename to use for the archived file
    item_fields <- item_get_fields(files[i,'ts_id'], 'files')
    item_files <- sapply(item_fields, function(x) x$name)
    archive_index <- match(files[i,'filename'], item_files)
    if(is.na(archive_index)) {
      warning("file to archive could not be located for ", files[i,'filename'], .call=FALSE)
      return(NA)
    }
    creation_date <- as.POSIXct(item_fields[[archive_index]]$dateUploaded, format="%Y-%m-%dT%H:%M:%SZ", tz='UTC')
    archive_name <- make_ts_archive_path(files[i,'filename'], creation_date=creation_date)

    # rename the file to the archive name by updating the item fields
    if(verbose) message("archiving ", files[i,'filename'], " as ", archive_name)
    item_fields[[archive_index]]$name <- archive_name
    item_update(ts_id, info = list(files=item_fields)) # do field ids disappear?
    
    return(archive_name)
  })
  
  new_names
}