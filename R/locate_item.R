#' Find a folder or other item on ScienceBase
#' 
#' @param key the item key, probably identical to the folder name
#' @param type the item type, probably either root or site_root
#' @param format character indicating whether the folder should be returned as
#'   an ID or as a full URL
#' @param ... args passed to \code{\link[sbtools]{session_check_reauth}}
#' @examples 
#' \dontrun{
#' mda.streams:::locate_item(key="sites", type="root")
#' mda.streams:::locate_item(key=c("sites","proposals","publications"), 
#'   type="root", format="folder_url")
#' mda.streams:::locate_item(key="nwis_02322688", type="site_root")
#' }
locate_item <- function(key=c("nwis_02322688", "project", "presentations", "proposals", "publications", "sites"), 
                        type=c("site_root","root"), format=c("id","item_url","folder_url"), ...) {
  
  # process arguments
  format <- match.arg(format) # only one format per call to locate_item
  query_args <- data.frame(key=key, type=type, stringsAsFactors=FALSE)
  session_check_reauth(...)
  
  # run the query or queries
  sapply(1:nrow(query_args), function(argnum) {
    
    # ask ScienceBase for the item
    item <- query_item_identifier(scheme = get_scheme(), type = query_args$type[argnum], key = query_args$key[argnum])
    
    # reformat the result to our liking
    format_item(item, format)
  })
}

#' Locate items using the folder structure rather than tags
#' 
#' This function is useful for ScienceBase directory maintenance, because it
#' helps locate items that have no tags along with those that do. Items without
#' tags are created on post_ts operations that fail partway through.
#' 
#' @param parent the ScienceBase ID of the parent whose children to search
#' @param title the title to seek among the children
#' @return an id, formatted as requested
#' @keywords internal
locate_item_by_dir <- function(parent, title, format=c("id","item_url","folder_url"), ...) {
  # process arguments
  format <- match.arg(format) # only one format per call to locate_item
  query_args <- data.frame(parent=parent, title=title, stringsAsFactors=FALSE)
  session_check_reauth(...)
  
  # run the query or queries
  sapply(seq_len(nrow(query_args)), function(argnum) {
    one_parent <- query_args$parent[argnum]
    one_title <- query_args$title[argnum]
    
    # find all children of the specified parent
    kids <- item_list_children(one_parent)
    
    # orphans are kids with parents and titles but not necessarily identity tags.
    is_orphan <- sapply(kids$id, function(kid) item_get(kid)$title == one_title)
    orphans <- if(length(is_orphan) > 0) kids[is_orphan,] else data.frame()
    
    # return properly formatted
    format_item(orphans, format)
  })
}

#' Format an item as a ScienceBase ID or URL, as requested
#' 
#' Internal helper to locate_item and locate_item_by_dir. Doesn't check the
#' value of format - that's up to the calling functions to do, for efficiency.
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
#' @export
#' @examples 
#' \dontrun{
#' locate_folder("publications", format="url")
#' testthat::expect_error(locate_folder("cvs", format="url"))
#' }
locate_folder <- function(folder=c("project","presentations","proposals","publications","sites"), format=c("id","url"), ...) {
  folder <- match.arg(folder)
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=folder, type="root", format=format, ...)
}

#' Find a site folder on ScienceBase
#' 
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @param by character indicating how to search for the item: using tags
#'   (the default) or by scanning the parent directory for the desired title?
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_site <- function(site_name, format=c("id","url"), by=c("tag","dir"), ...) {
  by <- match.arg(by)
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="folder_url")
  switch(
    by,
    "tag"=locate_item(key=site_name, type="site_root", format=format, ...),
    "dir"=locate_item_by_dir(parent=locate_folder("sites"), title=site_name, format=format, ...))
}

#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis"
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @param by character indicating how to search for the item: using tags
#'   (the default) or by scanning the parent directory for the desired title?
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs","wtr","disch"), "nwis_02322688")
#' locate_ts("doobs", "nwis_02322688", format="url")
#' locate_ts("doobs", "nwis_notasite", format="url")
#' testthat::expect_error(locate_ts("notavar", "nwis_notasite"))
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), by=c("tag","dir"), ...) {
  by <- match.arg(by)
  var_src <- make_ts_name(var_src) # check that the variable name is valid and add prefix
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="item_url")
  switch(
    by,
    "tag"=locate_item(key=site_name, type=var_src, format=format, ...),
    "dir"=locate_item_by_dir(parent=locate_site(site_name), title=var_src, format=format, ...))
}