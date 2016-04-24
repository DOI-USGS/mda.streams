#' Stage metabolism model outputs into ts files
#' 
#' Parse metabolism model outputs into gpp, er, and K600
#' 
#' @param metab_outs a single metab_model or a list of metab_models from which
#'   to extract GPP, ER, and K600
#' @param vars one or more strings in \code{c("gpp","er","K600")} naming the
#'   variables to extract and post
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param verbose provide verbose output (currently not implemented)
#' @return a character vector of file handles
#' @importFrom unitted u v
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom stats setNames
#' @export
stage_metab_ts <- function(metab_outs, vars=c("gpp","er","K600"), folder = tempdir(), verbose = FALSE) {
  # check inputs
  if(!is.list(metab_outs)) metab_outs <- list(metab_outs)
  
  staged <- unname(unlist(lapply(metab_outs, function(metab_mod) {
    # pull info from the model
    config_row <- get_info(metab_mod)$config
    mod_model <- config_row[[1,"model"]]
    src <- switch(
      mod_model,
      metab_night="estNight",
      "estBest"
    )
    preds <- predict_metab(metab_mod)
    site <- config_row[[1,"site"]]
    
    # add a DateTime column to preds, using noon sitetime (mean solar time) to
    # represent each corresponding Date
    coords <- get_site_coords(site)
    preds$DateTime <- convert_solartime_to_UTC(
      as.POSIXct(paste0(preds$date, " 12:00:00"), tz="UTC"), 
      longitude=coords$lon, time.type="mean solar")
    
    # extract specific columns into ts files. this section will need rewriting
    # as the range of model options expands.
    var <- ".dplyr.var"
    file_paths <- sapply(vars, function(pvar) {
      metab_var <- get_var_src_codes(var==pvar, out="metab_var")[1]
      var_units <- get_var_src_codes(var==pvar, out="units")[1]
      data <- preds[c("DateTime", metab_var)] %>%
        setNames(c("DateTime",pvar)) %>%
        u(c(NA, var_units))
      if(length(data[,2]) > 0 && length(which(!is.na(data[,2]))) > 0) {
        verify_var_src(pvar, src, on_fail=warning)
        write_ts(data=data, site=site, var=pvar, src=src, folder=folder)
      } else {
        NA
      }
    })
    file_paths[!is.na(file_paths)]
  })))
  
  staged
}