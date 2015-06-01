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
#' @param config data.frame, or file/filename to read a data.frame from. As an 
#'   alternative to all preceding arguments, this single config argument may be 
#'   passed in. The data.frame columns must correspond precisely to the list of
#'   preceding arguments.
#' @param row integer. The row number of the config data.frame to use for this 
#'   particular model run.
#' @export
#' @examples
#' suppressWarnings(config_to_metab(
#'   model="metab_mle", site=NA, doobs.type=NA, doobs.src=NA, 
#'   disch.type=NA, disch.src=NA, wtr.type=NA, wtr.src=NA))
#' suppressWarnings(config_to_metab(config=data.frame(
#'   model="metab_mle", site=NA, doobs.type=NA, doobs.src=NA, 
#'   disch.type=NA, disch.src=NA, wtr.type=NA, wtr.src=NA)))
#' suppressWarnings(config_to_metab(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", filename=NULL)))
#' \dontrun{
#' config_to_metab(
#'   model="metab_mle", site=NA, doobs.type=NA, doobs.src=NA, 
#'   disch.type=NA, disch.src=NA, wtr.type=NA, wtr.src=NA)
#' config_to_metab(config=stage_metab_config(
#'   tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", filename=NULL))
#' }
config_to_metab <- function(model, site, 
                             doobs.type, doobs.src, 
                             disch.type, disch.src, 
                             wtr.type, wtr.src, 
                             config, row=1) {

  # Check the input
  cols_expected <- formals(config_to_metab) %>% replace(c("config", "row"), NULL) %>% names() # config values we expect
  cols_given <- match.call() %>% as.list() %>% .[-1] %>% replace(c("config", "row"), NULL) %>% names() # config values specified independently in this call
  cols_ok <- formals(stage_metab_config) %>% names() %>% .[1:3] # config cols that would be OK to see (b/c they're standard config file metadata)
  # create the data.frame if appropriate
  if(missing(config)) {
    config <- as.data.frame(mget(cols_expected, inherits=FALSE), stringsAsFactors=FALSE)
  } else if(any(cols_given %in% cols_expected)) {
    stop("in config is specified, other arguments must not be")
  }
  # read the data.frame if appropriate
  if(is.character(config) | is(config, "file")) {
    config <- read.table(config, sep=pkg.env$ts_delim, stringsAsFactors=FALSE)
  }
  # check the data.frame, which really should exist by now
  if(is.data.frame(config)) {
    if(!isTRUE(all.equal(names(config), cols_expected))) {
      true_extras <- setdiff(names(config), c(cols_ok, cols_expected)) # also permit tag, strategy, and model (first three config file columns)
      if(length(true_extras)>0) warning("config has these unexpected columns: ", paste0(true_extras, collapse=", "))
      missings <- setdiff(cols_expected, names(config))
      if(length(missings)>0) stop("config is missing these columns: ", paste0(missings, collapse=", "))
    }
    for(col in 1:ncol(config)) {
      if(is.factor(config[[col]])) config[,col] <- as.character(config[,col])
    }
  } else {
    stop("couldn't find or make the config data.frame")
  }
  
  # Handle 
  
  # Locate the model  
  metab_fun <- get(config$model, envir = environment(streamMetabolizer::metab_model))

  # Prepare the data
  warning("still need to actually download the data")
  data_needs <- formals(metab_fun)$data %>% eval()
  # confirm that non-needed variables are being specified as "none"-NA
  sm_name <- ".dplyr.var"
  var_codes <- get_var_codes(out=TRUE) %>% filter(sm_name %in% names(data_needs)) %>% do({rownames(.) <- .$sm_name; .[,"shortname",drop=FALSE]})
  # lapply(names(data_needs), function(need) {
  #   var_codes[need,"shortname"]
  # })
  # doobs <- locate_ts(site, "doobs", doobs.type, doobs.src)
  # disch <- locate_ts(site, "disch", disch.type, disch.src)
  # wtr <- locate_ts(site, "wtr", wtr.type, wtr.src)
  
  # Run the model
  warning("function under construction")
  # for metab_mle, expected.colnames <- c("date.time","DO.obs","DO.sat","depth","temp.water","light")
  #metab_fun(doobs, disch, wtr) # needs to match the requirements of that metab_fun
  
  # Return metabolism predictions
  return("here are your predictions")
}