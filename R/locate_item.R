#' Find a folder or other item on ScienceBase
#' 
#' locate_item is case insensitive. This function is only guaranteed to return 
#' the first match; others may exist and may or may not be returned.
#' 
#' @param key the item key, probably identical to the folder name. Examples are 
#'   c("nwis_02322688", "project", "presentations", "proposals", "publications",
#'   "sites")
#' @param type the item type, probably either root or site_root. Examples are 
#'   c("site_root","root")
#' @param format character indicating whether the folder should be returned as 
#'   an ID or as a full URL
#' @param by character indicating how to search for the item: using tags ("tag",
#'   the default and recommended option), by scanning the parent directory for 
#'   the desired title ("dir"), or both in combination ("either")?
#' @param parent the ScienceBase ID of the parent whose children to search if 
#'   \code{by \%in\% c("dir","either")}
#' @param title the title to seek among the children if \code{by \%in\% 
#'   c("dir","either")}
#' @param limit number of items to return, as in 
#'   \code{\link[sbtools]{query_item_identifier}} or 
#'   \code{\link[sbtools]{item_list_children}}
#' @import dplyr
#' @import sbtools
#' @examples 
#' \dontrun{
#' mda.streams:::locate_item(key="sites", type="root")
#' mda.streams:::locate_item(key=c("sites","proposals","publications"), 
#'   type="root", format="folder_url")
#' mda.streams:::locate_item(key="nwis_02322688", type="site_root")
#' }
locate_item <- function(key, type, format=c("id","item_url","folder_url"), 
                        by=c("tag","dir","either"), parent, title, limit=5000) {
  
  # process arguments
  format <- match.arg(format) # only one format per call to locate_item
  by <- match.arg(by)
  query_args <- 
    switch(
      by,
      tag=data.frame(key=key, type=type, stringsAsFactors=FALSE),
      dir=data.frame(parent=parent, title=title, stringsAsFactors=FALSE),
      either=data.frame(key=key, type=type, parent=parent, title=title, stringsAsFactors=FALSE)
    ) %>%
    lapply(tolower) %>%
    as.data.frame(stringsAsFactors=FALSE)
  
  if(get_scheme() == 'mda_streams_dev' && is.null(current_session())) 
    stop("log in to use mda_streams_dev. see authenticate_sb()")

  # run the query or queries
  sapply(1:nrow(query_args), function(argnum) {
    
    # query by tag
    item <- data.frame()
    if(by %in% c("tag","either")) {
      item <- query_item_identifier(scheme = get_scheme(), type = query_args$type[argnum], key = query_args$key[argnum], limit=limit)
    }
    
    # query by dir
    if(by == "dir" || (isTRUE(nrow(item)==0) && by == "either")) {
      # find all children of the specified parent. for finding a site, it'd be
      # more efficient to only list children once rather than every iteration of
      # the sapply, but this should be a rare case.
      kids <- item_list_children(id=query_args$parent[argnum], limit=limit)
      itemnum <- NA
      for(i in seq_len(nrow(kids))) {
        kid_title <- tryCatch(tolower(item_get(id=kids[i,"id"])$title), error=function(e) NA)
        if(isTRUE(kid_title == query_args$title[argnum])) {
          itemnum <- i
          break
        }
      }
      item <- if(!is.na(itemnum)) kids[itemnum,,drop=FALSE] else data.frame()
    }    
    
    # reformat the result to our liking
    format_item(item, format)
  })
}

#' Format an item as a ScienceBase ID or URL, as requested
#' 
#' Internal helper to locate_item. Doesn't check the value of format - that's up
#' to the calling functions to do, for efficiency.
#' 
#' @param item a ScienceBase item data.frame, e.g., as returned from 
#'   query_item_identifier
#' @keywords internal
format_item <- function(item, format) {
  if(nrow(item) == 0) {
    NA
  } else {
    switch(
      format,
      id=item$id,
      item_url=paste0("https://www.sciencebase.gov/catalog/item/", item$id),
      folder_url=paste0("https://www.sciencebase.gov/catalog/folder/", item$id))
  }
}

#' Find a high-level folder on ScienceBase
#' 
#' @param folder the folder to locate
#' @inheritParams locate_item
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' locate_folder("publications", format="url")
#' testthat::expect_error(locate_folder("cvs", format="url"))
#' }
locate_folder <- function(folder=c("project","presentations","proposals","publications","sites"), 
                          format=c("id","url"), by=c("tag","dir","either"), limit=5000) {
  folder <- tolower(folder)
  folder <- match.arg(folder)
  if(folder != 'sites' && is.null(current_session())) 
    stop("session is NULL, so only the sites folder is visible. see authenticate_sb()")
  if(folder == 'project' && by %in% c("dir", "either"))
    stop("'by' must be 'tag' when searching for the project folder")
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=folder, type="root", parent=locate_folder("project"), title=folder, by=by, format=format, limit=limit)
}

#' Find a site folder on ScienceBase
#' 
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_site <- function(site_name, format=c("id","url"), by=c("tag","dir","either"), limit=5000) {
  by <- match.arg(by)
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=site_name, type="site_root", parent=locate_folder("sites", by=by), title=site_name, by=by, format=format, limit=limit)
}

#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis", for which you want the 
#'   timeseries
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want to
#'   look in
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs","wtr","disch"), "nwis_02322688")
#' locate_ts("doobs", "nwis_02322688", format="url")
#' locate_ts("doobs", "nwis_notasite", format="url")
#' testthat::expect_error(locate_ts("notavar", "nwis_notasite"))
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), by=c("tag","dir","either"), limit=5000) {
  by <- match.arg(by)
  var_src <- make_ts_name(var_src) # check that the variable name is valid and add prefix
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=site_name, type=var_src, parent=locate_site(site_name, by=by), title=var_src, by=by, format=format, limit=limit)
}


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
#' @export
#' 
#' @examples 
#' \dontrun{
#' repair_ts("wtr_nwis", "nwis_01374019")
#' }
repair_ts <- function(var_src, site_name, limit=5000) {
  
  # check the session; we'll need write access
  if(is.null(current_session()))
    stop("log in to repair data. see authenticate_sb()")
  
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
