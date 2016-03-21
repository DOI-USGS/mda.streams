#' Delete a site and all its files.
#' 
#' @param sites one or more site IDs such as "nwis_08437710" to look up and 
#'   delete
#' @param children_only logical. If TRUE, only the child items in the site 
#'   folder will be deleted, leaving an empty site folder
#' @param verbose logical. Should status messages be given?
#' @return anything passed back from the final item_rm if children_only==FALSE,
#'   or a pointer to the remaining item if children_only==TRUE
#' @keywords internal
#' @examples
#' \dontrun{
#' login_sb()
#' set_scheme("mda_streams_dev")
#' 
#' sites <- c("nwis_00000000", "nwis_00000001", "nwis_00000002")
#' post_site(sites)
#' file_par2 <- stage_calc_ts(sites="nwis_00000000", var="par", src="simNew", verbose=TRUE,
#'   inputs=list(utctime=NA, value=unitted::u(rnorm(1, 10, 2), "umol m^-2 s^-1")))
#' post_ts(file_par2)
#' list_tses("nwis_00000000")
#' mda.streams:::delete_site(sites, children_only=TRUE)
#' locate_site(sites)
#' list_tses("nwis_00000000")
#' mda.streams:::delete_site(sites)
#' locate_site(sites)
#' 
#' set_scheme("mda_streams")
#' }
delete_site <- function(sites, children_only=FALSE, verbose=TRUE) {
  site_ids <- locate_site(sites)
  delete_item(item_ids=site_ids, item_names=sites, 
              delete_files=FALSE, delete_children=TRUE, delete_item=!children_only, verbose=verbose)
}