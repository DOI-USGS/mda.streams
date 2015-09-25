#' Find a metab_model item on ScienceBase
#' 
#' @param model_name the model name, e.g., "nwis_05515500-35-150729 0.1.2 compare_par_srces"
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_metab_model("nwis_02322688-147-150724 0.0.4 nighttime_k", format="url", browser=FALSE)
#' }
locate_metab_model <- function(model_name, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="folder_url")
  folder <- locate_folder("metab_models", by="tag") # do this before the locate_item to force eval & a check for auth status
  locate_item(key=model_name, type="metab_model", parent=folder, title=model_name, by=by, format=format, limit=limit, browser=browser)
}