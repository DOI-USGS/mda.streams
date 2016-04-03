#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
#' @param on_exists character specifying an action when a file to post already 
#'   exists on ScienceBase
#' @param archive_existing logical. if the file already exists, should it be
#'   archived before being replaced with a new or merged file (when on_exists ==
#'   'replace' or 'merge', respectively)?
#' @param verbose logical. Should status messages be given?
#' @author Luke Winslow, Corinna Gries, Jordan S Read
#' @import sbtools
#' @importFrom lubridate tz
#' @import dplyr
#' @importFrom unitted u v get_units
#' @examples
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_05406479", "nwis_05435950", "nwis_04087119")
#' post_site(sites, on_exists="clear")
#' files <- stage_nwis_ts(sites = sites, var = "doobs", 
#'   times = c('2014-01-01 00:00','2014-01-01 05:00'), version='tsv')
#' post_ts(files)
#' locate_ts("doobs_nwis", sites) # find the newly posted data online
#' post_ts(files, on_exists="skip")
#' post_ts(files, on_exists="replace")
#' post_site(sites, on_exists="clear")
#'  
#' set_scheme("mda_streams")
#' }
#' @export
post_ts = function(files, on_exists=c("stop", "skip", "replace", "merge"), archive_existing=TRUE, verbose=TRUE){
  
  # check inputs & session
  if(is.null(files)) return(invisible(NULL))
  on_exists <- match.arg(on_exists)
  sb_require_login("stop")
  
  # loop through files, posting each and recording whether we'll need to add tags
  ts_ids <- sapply(1:length(files), function(i) {
    if(verbose) message('preparing to post file ', files[i])
    
    # parse the file name to determine where to post the file
    ts_path <- parse_ts_path(files[i], out = c('ts_name','var','src','var_src','site_name','file_name','dir_name','version'))
    # don't even try if the var_src shouldn't be there
    verify_var_src(ts_path$var_src, on_fail=stop)
    
    # look for an existing ts item+file and create the item if needed
    ts_id <- locate_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, by="either")
    if(is.na(ts_id)) {
      # find the site root
      site_root = locate_site(ts_path$site_name)
      if(is.na(site_root)) stop('no site folder available for site ', ts_path$site_name)
      
      # create the ts item and item tags
      ts_id <- item_create(site_root, title=ts_path$ts_name)$id
      repair_ts(ts_path$var_src, ts_path$site_name)
      file_exists <- FALSE
    } else {
      file_exists <- basename(files[i]) %in% sbtools::item_list_files(sb_id = ts_id)$fname 
    }
    
    # handle the actual file appropriately
    if(!file_exists) {
      if(verbose) message("posting file ", ts_path$file_name, " to ", ts_id)
      item_append_files(ts_id, files = files[i])
    } else {
      if(verbose) message("the file ", ts_path$file_name, " already exists at ", ts_id)
      switch(
        on_exists,
        "stop"={ 
          stop('file already exists and on_exists="stop"', call.=FALSE) 
        },
        "skip"={ 
          if(verbose) message("skipping timeseries file already on ScienceBase")
          return(NA)
        },
        "replace"={ 
          # proceed to the code following this switch() statement
        },
        "merge"={ 
          if(verbose) message("merging new timeseries with old")
          
          # prepare the old and new ts files. read_ts confirms that each input result is in ascending DateTime order
          pre_merge_dir <- file.path(ts_path$dir_name, "pre_merge_temp")
          dir.create(pre_merge_dir, showWarnings=FALSE)
          ts_old <- download_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, folder=pre_merge_dir, version=ts_path$version, on_local_exists="replace") %>%
            read_ts(on_invalid="stop")
          ts_new <- read_ts(files[i], on_invalid="stop")
          
          # join the old and new ts files
          . <- 'dplyr_var'
          ts_merged <- full_join(ts_old, ts_new, by=c('DateTime',ts_path$var)) %>% # will give errors on mismatched units
            as.data.frame(stringsAsFactors=FALSE) %>%
            { .[order(.$DateTime),] } # the make sure the merged result is in ascending DateTime order
          if(!isTRUE(verify_ts(ts_merged, ts_path$var, on_fail=warning)))
            stop("merge failed. try on_exists='replace'") # would also get checked in write_ts below, but this error is more informative
          if(verbose) message("num rows before & after merge: old=", nrow(ts_old), ", new=", nrow(ts_new), ", merged=", nrow(ts_merged))

          # replace the input file, but write to a nearby directory so we don't overwrite the user's file
          post_merge_dir <- file.path(ts_path$dir_name, "post_merge_temp")
          dir.create(post_merge_dir, showWarnings=FALSE)
          files[i] <- write_ts(data=ts_merged, site=ts_path$site_name, var=ts_path$var, src=ts_path$src, folder=post_merge_dir)
        })
      
      # if we reach this point, we're either in 'replace' or 'merge' and need to
      # replace an old file with a new or merged file
      if(archive_existing) {
        # archive (rename) the old file, then write the merged file
        archive_ts(ts_id=ts_id, filename=files[i], verbose=verbose)
        if(verbose) message("uploading ", switch(on_exists, replace="new", merge="merged"), " file")
        item_append_files(ts_id, files=files[i])
      } else {
        # overwrite the old file with the merged file
        if(verbose) message("replacing old file with ", switch(on_exists, replace="new", merge="merged"))
        item_replace_files(ts_id, files=files[i], all=FALSE)
      }
    }
    
    return(ts_id)
  })
  invisible(ts_ids)
  
}
