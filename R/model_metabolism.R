#' Actually run the model as specified by the configuration arguments
#' 
#' @import streamMetabolizer
#' @import dplyr
#' @param site site name
#' @param model model name shorthand, e.g. "simple"
#' @param doobs.type doobs data type
#' @param doobs.src doobs data src
#' @param disch.type disch data type
#' @param disch.src disch data src
#' @param wtr.type wtr data type
#' @param wtr.src wtr data src
#' @param config data.frame. As an alternative to all preceding arguments, a
#'   single config argument may be passed in. config is a one-row data.frame
#'   with columns corresponding precisely to the list of preceding arguments.
#' @export
#' @examples
#' suppressWarnings(model_metabolism(config=data.frame(
#'   model="simple", site=NA, doobs.type=NA, doobs.src=NA, disch.type=NA, disch.src=NA, wtr.type=NA, wtr.src=NA)))
#' \dontrun{
#' model_metabolism(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=NULL))
#' }
model_metabolism <- function(model, site, 
                             doobs.type, doobs.src, 
                             disch.type, disch.src, 
                             wtr.type, wtr.src, 
                             config) {

  # Check the input
  if(!missing(config)) {
    args_avail <- formals(model_metabolism) %>% replace("config", NULL) %>% names()
    args_given <- match.call() %>% as.list() %>% .[-1] %>% replace("config", NULL) %>% names()
    if(any(args_given %in% args_avail)) {
      stop("in config is specified, other arguments must not be")
    }
    if(!is.data.frame(config) | dim(config)[1] != 1) {
      stop("expecting config to be a 1-row data.frame")
    }
    if(!all.equal(names(config), args_avail)) {
      extras <- setdiff(names(config), args_avail)
      missings <- setdiff(args_avail, names(config))
      if(length(missings)>0) stop("config is missing these columns: ", paste0(missings, collapse=", "))
      if(length(extras)>0) warning("config has these unexpected columns: ", paste0(extras, collapse=", "))
    }
    for(arg in args_avail) {
      assign(arg, config[[arg]])
    }
  } # else args should fail correctly in subsequent lines if misspecified
  
  # Prepare the data
  warning("still need to actually download the data")
  doobs <- locate_ts(site, "doobs", doobs.type, doobs.src)
  disch <- locate_ts(site, "disch", disch.type, disch.src)
  wtr <- locate_ts(site, "wtr", wtr.type, wtr.src)
  
  # Run the model  
  metab_fun <- get(paste0("metab_", model), envir = environment(streamMetabolizer::metab_model))
  warning("function under construction")
  # for metab_simple, expected.colnames <- c("date.time","DO.obs","DO.sat","depth","temp.water","light")
  #metab_fun(doobs, disch, wtr) # needs to match the requirements of that metab_fun
  
  # Return metabolism predictions
  return("here are your predictions")
}