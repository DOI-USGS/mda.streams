#' Find a folder or other item on ScienceBase
#' 
#' @param key the item key, probably identical to the folder name
#' @param type the item type, probably either root or site_root
#' @param format character indicating whether the folder should be returned as
#'   an ID or as a full URL
#' @param ... other args passed to \code{\link[sbtools]{session_check_reauth}}
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
  
  # run the query or queries
  session_check_reauth(...)
  sapply(1:nrow(query_args), function(argnum) {
    
    # ask ScienceBase for the item
    item <- query_item_identifier(scheme = get_scheme(), type = query_args$type[argnum], key = query_args$key[argnum])
    
    # reformat the result to our liking
    location <- if(nrow(item) == 0) {
      NA
    } else {
      switch(
        format,
        id=item$id,
        item_url=paste0("https://www.sciencebase.gov/catalog/item/", item$id),
        folder_url=paste0("https://www.sciencebase.gov/catalog/folder/", item$id))
    }
  })
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
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_site <- function(site_name, format=c("id","url"), ...) {
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=site_name, type="site_root", format=format, ...)
}

#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis"
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs","wtr","disch"), "nwis_02322688")
#' locate_ts("doobs", "nwis_02322688", format="url")
#' locate_ts("doobs", "nwis_notasite", format="url")
#' testthat::expect_error(locate_ts("notavar", "nwis_notasite"))
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), ...) {
  var_src <- make_ts_name(var_src) # check that the variable name is valid and add prefix
  site_name <- make_site_name(parse_site_name(site_name)) # check that the site name is reasonably valid
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=site_name, type=var_src, format=format, ...)
}