#' Load one or more metab_model objects into R
#' 
#' Uses a previously-downloaded copy of this metab_model unless (1) that 
#' download occurred in a different R session, or (2) on_local_exists-'replace'.
#' 
#' @param model_name the name of the metab_model file
#' @inheritParams download_item_files
#' @export
#' @importFrom stats setNames
get_metab_model <- function(model_name, on_local_exists='skip') {
  mms <- lapply(setNames(model_name,model_name), function(mname) {
    file <- download_metab_model(mname, on_local_exists = on_local_exists)
    varname <- load(file)
    get(varname)
  })
  if(length(mms) == 1) {
    return(mms[[1]])
  } else {
    return(mms)
  }
}