#' Post new metadata file[s] to SB
#' 
#' If a file with the same name already exists in Sites_meta, the
#' \code{on_exists} argument specifies how to manage the conflict.
#' 
#' @param files character one or more files to post to SB
#' @param on_exists character. what should be done when an item already exists?
#' @param verbose logical. Should status messages be given?
#' @return an item list
#' @author Alison Appling
#' @import sbtools
#' @importFrom stats setNames
#' @export
#' @examples 
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' metafile <- stage_meta(sites=list_sites(), folder="temp")
#' post_meta(metafile, on_exists="stop", verbose=TRUE)
#' set_scheme("mda_streams")
#' }
post_meta <- function(files, on_exists=c("stop", "skip", "replace", "merge"), verbose=TRUE) {
  # handle inputs
  on_exists <- match.arg(on_exists)
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  meta_ids <- sapply(setNames(files, files), function(metafile) {
    
    # look for an existing ts and respond appropriately
    metapath <- parse_meta_path(metafile)
    meta_id <- locate_meta(metapath$type, by="either")
    if(!is.na(meta_id)) {
      if(verbose) message('the metadata item ', metapath$meta_type, ' already exists on SB')
      switch(
        on_exists,
        "stop"={ 
          stop('the metadata item already exists and on_exists="stop"') },
        "skip"={ 
          if(isTRUE(verbose)) message("skipping posting of the metadata item") 
          return(NA) # na is signal that doesn't need new tags
        },
        "replace"={ 
          if(verbose) message("deleting metadata before replacement: ", meta_id)
          delete_meta(metapath$type, files_only=TRUE, verbose=verbose)
        },
        "merge"={ 
          if(verbose) message("merging new metadata with old: ", meta_id)
          stop("merge not yet implemented")
          #pre_merge_dir <- file.path(ts_path$dir_name, "pre_merge_temp")
          #dir.create(pre_merge_dir, showWarnings=FALSE)
          #meta_old <- read_meta(download_meta(var_src=ts_path$var_src, site_name=ts_path$site_name, folder=pre_merge_dir, on_local_exists="replace"))
          #meta_old <- read.table(download_meta(...))
          #meta_new <- read_meta(...)
          # join.
          #meta_merged <- full_join(meta_old, meta_new, by=names(meta_old))
          #if(verbose) message("num rows before & after merge: old=", nrow(meta_old), ", new=", nrow(meta_new), ", merged=", nrow(meta_merged))
          # replace the input file, but write to a nearby directory so we don't overwrite the user's file
          #merge_dir <- file.path(metapath$dir_name, "post_merge_temp")
          #dir.create(merge_dir, showWarnings=FALSE)
          #fpath <- file.path(metapath$file_name, merge_dir)
          #files[i] <- write.table(meta_merged, fpath, sep="\t", row.names=FALSE)
          # delete the old one in preparation for overwriting
          #delete_meta(metapath$type, files_only=TRUE, verbose=verbose)
        })
    } else {
      # create the item
      meta_id <- item_create(parent_id=locate_folder("sites_meta"), title=metapath$meta_type)
    }

    # attach data file to ts item. SB quirk: must be done before tagging with 
    # identifiers, or identifiers will be lost
    if(verbose) message("posting metadata file ", metapath$file_name)
    item_append_files(meta_id, files = metafile)

    return(meta_id)  
  })
  
  # separate loop to increase probability of success in re-tagging files when length(files) >> 1
  posted_items <- sapply(1:length(meta_ids), function(i) {
    
    # if we skipped it once, skip it again
    if(is.na(meta_ids[i])) 
      return(NA)
    else
      meta_id <- meta_ids[i]
    
    # parse (again) the file name to determine where to post the file
    metapath <- parse_meta_path(files[i])
    
    # tag item with our special identifiers. if the item already existed,
    # identifiers should be wiped out by a known SB quirk, so sleep to give time
    # for the files to be added and the identifiers to disappear so we can replace them
    for(wait in 1:100) {
      Sys.sleep(0.2)
      if(nrow(sbtools::item_list_files(meta_id)) > 0 && is.null(item_get(meta_id)$identifiers)) break
      if(wait==100) stop("identifiers didn't disappear and so can't be replaced")
    }
    if(verbose) message("adding/replacing identifiers for item ", meta_id, ": ",
                        "scheme=", get_scheme(), ", type=", "sites_meta", ", key=", metapath$meta_type)
    repair_meta(metapath$type, limit=5000)
    
    meta_id
  })
  
  invisible(posted_items)
}
