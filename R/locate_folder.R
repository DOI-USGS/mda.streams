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
locate_folder <- function(folder=c("project","metab_runs","metab_models","sites","sites_meta","ts_meta","ideas","presentations","proposals","publications"), 
                          format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  folder <- tolower(folder)
  folder <- match.arg(folder)
  if(!(folder %in% c('sites','sites_meta','ts_meta')) && (is.null(current_session()) || !session_validate()))
    stop("this folder is not publicly visible. call login_sb() first to see more")
  if(folder == 'project' && by %in% c("dir", "either"))
    stop("'by' must be 'tag' when searching for the project folder")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=folder, type="root", parent=locate_folder("project", by="tag"), title=folder, by=by, format=format, limit=limit, browser=browser)
}