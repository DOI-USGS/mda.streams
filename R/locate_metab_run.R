#' Find a metab_run item on ScienceBase
#' 
#' Find a metab_run item by title
#' 
#' @param date the date as a character string in \%Y\%m\%d format
#' @param tag the tag as a character string
#' @param strategy the run strategy as a character string
#' @param title the title of the metabolism modeling run you want. this will be
#'   automatically constructed if date, tag, and strategy are available; if any
#'   of those is missing, title should be supplied.
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_metab_run('150714 0.0.2 local_makefile_run')
#' }
locate_metab_run <- function(date, tag, strategy, title=paste0(date, " ", tag, " ", strategy), format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  metab_run_folder <- locate_folder("metab_runs", by="tag")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key=title, type="metab_run", parent=metab_run_folder, title=title, by=by, format=format, limit=limit, browser=browser)
}