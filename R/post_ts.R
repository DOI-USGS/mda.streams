#' Post a new timeseries file to SB
#' 
#' Post a staged file from the local computer to ScienceBase
#' 
#' @param files a string vector of file paths for POSTing
#' @param on_exists character specifying an action when a file to post already
#'   exists on ScienceBase
#' @param verbose logical. Should status messages be given?
#' @author Luke Winslow, Corinna Gries, Jordan S Read
#' @import sbtools
#' @importFrom lubridate tz
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
#' set_scheme("mda_streams_")
#' }
#' @export
post_ts = function(files, on_exists=c("stop", "skip", "replace", "merge","archive"), verbose=TRUE){
  
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
        
    # find the site root
    site_root = locate_site(ts_path$site_name)
    if(is.na(site_root)){
      stop('no site folder available for site ', ts_path$site_name)
    }

    # look for an existing ts and respond appropriately
    ts_id <- locate_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, by="either")
    if (is.na(ts_id)) {
      ts_id <- item_create(site_root, title=ts_path$ts_name)$id
      repair_ts(ts_path$var_src, ts_path$site_name)
      file_exists <- FALSE
    } else{
      file_exists <- basename(files[i]) %in% sbtools::item_list_files(sb_id = ts_id)$fname 
    }
    
    if (file_exists){
      if(verbose) message('the ', ts_path$ts_name, ' timeseries for site ', ts_path$site_name, ' already exists')
      switch(
        on_exists,
        "stop"={ stop('item already exists and on_exists="stop"', call.=FALSE) },
        "skip"={ 
          if(verbose) message("skipping timeseries item already on ScienceBase: ", ts_id)
          return(NA) # na is signal that doesn't need new tags
        },
        "replace"={ 
          if(verbose) {
            message("deleting timeseries data before replacement: ", ts_id)
            message("posting file to site ", ts_path$site_name, ", timeseries ", ts_path$ts_name)
          }
          item_replace_files(ts_id, files = files[i], all=FALSE)
        },
        "merge"={ 
          if(verbose) message("merging new timeseries with old: ", ts_id)
          pre_merge_dir <- file.path(ts_path$dir_name, "pre_merge_temp")
          dir.create(pre_merge_dir, showWarnings=FALSE)
          ts_old <- read_ts(download_ts(var_src=ts_path$var_src, site_name=ts_path$site_name, folder=pre_merge_dir, version=ts_path$version, on_local_exists="replace"))
          ts_new <- read_ts(files[i])
          # join. these lines should be changed once unitted::full_join.unitted is implemented
          if(!all.equal(get_units(ts_old), get_units(ts_new))) stop("units mismatch between old and new ts files")
          ts_merged <- u(unique(rbind(v(ts_old), v(ts_new))), get_units(ts_old))
          if(!all.equal(unique(v(ts_merged$DateTime)), v(ts_merged$DateTime))) stop("merge failed. try on_exists='replace'")
          if(verbose) message("num rows before & after merge: old=", nrow(ts_old), ", new=", nrow(ts_new), ", merged=", nrow(ts_merged))
          # replace the input file, but write to a nearby directory so we don't overwrite the user's file
          post_merge_dir <- file.path(ts_path$dir_name, "post_merge_temp")
          dir.create(post_merge_dir, showWarnings=FALSE)
          files[i] <- write_ts(data=ts_merged, site=ts_path$site_name, var=ts_path$var, src=ts_path$src, folder=post_merge_dir)
          # delete the old one in preparation for overwriting
          item_replace_files(ts_id, files = files[i], all=FALSE)
        },
        "archive"={
        
         ts_fields <- item_get_fields(ts_id, 'files')
         ts_files <- sapply(ts_fields, function(x) x$name)
         # find which 
         archive_indx <- which(basename(files[i]) == ts_files)
         archive_name <- make_archive_name(basename(files[i]), file_field = ts_fields[[archive_indx]])
         ts_fields[[archive_indx]]$name <- archive_name
         if(verbose) message("archiving old timeseries as: ", archive_name)
         item_update(ts_id, info = list(files=ts_fields)) # do field ids disappear?
         if(verbose) 
           message("posting file to site ", ts_path$site_name, ", timeseries ", ts_path$ts_name)
         item_append_files(ts_id, files = files[i])
        })
    } else {
      if(verbose) 
        message("posting file to site ", ts_path$site_name, ", timeseries ", ts_path$ts_name)
      item_append_files(ts_id, files = files[i])
    }
    

    return(ts_id)
  })
  invisible(ts_ids)

}
