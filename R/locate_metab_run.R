#' Find a metab_run item on ScienceBase
#' 
#' Find a metab_run item by title
#' 
#' @param title the title of the metabolism modeling run you want. this is the
#'   date, tag, and run strategy separated by spaces.
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_metab_run('150714 0.0.2 local_makefile_run')
#' }
locate_metab_run <- function(title, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  metab_run_folder <- locate_folder("metab_runs", by="tag") # do this before the locate_item to force eval & a check for auth status
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=title, type="metab_run", parent=metab_run_folder, title=title, by=by, format=format, limit=limit, browser=browser)
}