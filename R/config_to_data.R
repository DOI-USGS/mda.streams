#' COnvert config specifications into a data input table
#' 
#' Turns a single config row into a data.frame of input data for the specified 
#' metabolism modeling function
#' 
#' @param config_row a 1-row config data.frame
#' @param row_num the row number/name that this config_row held in the original 
#'   config
#' @param metab_fun a metabolism modeling function
#' @param metab_args a list of arguments (possibly just \code{list()}) that will
#'   later be passed to the metab_fun
#' @param on_error character. What to do if data cannot be acquired or merged? 
#'   If 'stop' or 'warn', the appropriate condition is thrown. If 'warn' or 
#'   'quiet', the function [also] returns a character of errors and/or a 
#'   character of warnings, attached as attributes of the output object
#' @return NA or a unitted data.frame, possibly with attributes "errors" and/or 
#'   "warnings" attached (see on_error)
#' @import dplyr
#' @importFrom unitted u
#' @import streamMetabolizer
#' @examples 
#' \dontrun{
#' depth_file <- download_ts("depth_calcDisch", "nwis_04087142", 
#'   on_local_exists="replace")
#' config <- stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", dosat=choose_data_source("dosat", "nwis_04087142", 
#'     logic="simple dosat", type="const", src="12,mg L^-1"), 
#'   depth=choose_data_source("depth", "nwis_04087142", logic="local file", 
#'     type="file", src=depth_file), filename=NULL)
#' cdat <- config_to_data(config[1,], row_num=1, metab_fun=metab_mle, metab_args=list())
#' head(cdat)
#' }
#' @export
config_to_data <- function(config_row, row_num, metab_fun, metab_args, on_error=c('stop','warn','quiet')) {

  # process inputs
  on_error <- match.arg(on_error)
  
  # get a lookup table to translate between mda.streams vars and metab vars
  var_lookup <- unique(get_var_src_codes(out=c("metab_var","var")))
  
  # get a list of vars for which we expect complete info
  data_needs <- colnames(eval(formals(metab_fun)$data))
  var_needs <- var_lookup[match(data_needs, var_lookup$metab_var),"var"]                      

  # get a list of vars and the relevant columns supplied in the config_row
  spec_suffixes <- colnames(choose_data_source(var="doobs", site=NA, logic="unused var"))
  spec_prefixes <- gsub(".src$", "", names(config_row)[grepl(".src$", names(config_row))])
  spec_data_names <- var_lookup[match(spec_prefixes, var_lookup$var),"metab_var"]
  data_specs <- lapply(setNames(spec_prefixes,spec_data_names), function(var) {
    setNames(paste0(var, ".", spec_suffixes), spec_suffixes)
  })
  
  # prepare vectors to collect any errors/warnings
  err_strs <- warn_strs <- character()
  
  # check the vars for which we expect the specs to be NAs
  data_ignored <- which(names(data_specs) %in% setdiff(names(data_specs), data_needs))
  for(ignored in data_ignored) {
    type <- config_row[[1,data_specs[[ignored]][['type']]]]
    site <- config_row[[1,data_specs[[ignored]][['site']]]]
    src <- config_row[[1,data_specs[[ignored]][['src']]]]
  
    if(is.na(type) || type != 'none') {
      warn_strs <- c(warn_strs, paste0("need type='none' for unused var ", spec_prefixes[ignored]))
    }
  }
  
  # check and acquire the vars we need to return
  data_list <- lapply(1:length(data_needs), function(needed) {
    # don't waste time if we've already hit errors
    if(length(err_strs) > 0) return(NA)
    
    var <- var_needs[needed]
    type <- config_row[[1,data_specs[[needed]][['type']]]]
    site <- config_row[[1,data_specs[[needed]][['site']]]]
    src <- config_row[[1,data_specs[[needed]][['src']]]]
    
    # run config_to_data_column (defined below) in a tryCatch block
    data <- withCallingHandlers(
      tryCatch(
        config_to_data_column(var, type, site, src),
        error=function(e) { 
          err_strs <<- c(err_strs, paste0(var,"-",type,": ",e$message))
          NA 
        }),
      warning=function(w) { 
        warn_strs <<- c(warn_strs, paste0(var,"-",type,": ",w$message))
        invokeRestart("muffleWarning")
      })
    data
  })
  # remove any elements that came back NA (i.e., with errors)
  data_list[sapply(data_list, function(d) isTRUE(is.na(d)) )] <- NULL
  
  # combine the data into a single data.frame, or return NA 
  if(length(data_list) == length(data_needs)) {
    out <- withCallingHandlers(
      tryCatch({
        combo <- do.call(combine_ts, c(data_list, list(method='approx', approx_tol=as.difftime(3, units="hours"))))
        combo %>% select_(.dots=var_needs) %>% setNames(data_needs)
      },
      error=function(e) { 
        err_strs <<- c(err_strs, paste0("combine: ",e$message))
        NA 
      }),
      warning=function(w) { 
        warn_strs <<- c(warn_strs, paste0("combine: ",w$message))
        invokeRestart("muffleWarning")
      })
  } else {
    err_strs <- c(err_strs,paste0("needed-but-missing data columns in config row ", row_num))
    out <- NA
  }

  # return the results and/or error/warning strings
  if(length(warn_strs) > 0) {
    attr(out, "warnings") <- warn_strs
    warn_str <- paste0(warn_strs,collapse="\n  ")
    if(on_error != 'quiet') warning(warn_str)
  }
  if(length(err_strs) > 0) {
    err_str <- paste0(err_strs, collapse="\n  ")
    switch(
      on_error,
      'stop'=stop(err_str),
      'warn'={
        warning(err_str)
        attr(out, "errors") <- err_strs
      },
      'quiet'={
        attr(out, "errors") <- err_strs
      })
  }
  out
}

#' Read a single column worth of data
#' 
#' For use within config_to_data
#' 
#' @inheritParams choose_data_source
#' @return a data.frame, else NA
#' @keywords internal
config_to_data_column <- function(var, type, site, src) {
  data <- NA
  switch(
    type,
    'ts'={
      num_tries <- 3
      for(i in 1:num_tries) {
        dfile <- tryCatch(
          download_ts(make_var_src(var, src), site, on_local_exists="replace", on_remote_missing="stop"),
          error=function(e) { warning(paste0("download try ",i,": ",e$message)); NULL })
        if(!is.null(dfile)) break else Sys.sleep(1)
      }
      if(is.null(dfile)) stop("failed to download file after ", num_tries, " tries")
      data <- read_ts(dfile)
    },
    'meta'={
      warning("meta type not currently implemented")
    },
    'file'={
      data <- read_ts(src)
    },
    'const'={
      const_strs <- strsplit(src, ',')[[1]]
      const_num <- as.numeric(const_strs[1])
      if(length(const_strs) != 2 || is.na(const_num)) {
        warning("value could not be interpreted as 'number,units'")
      }
      const <- u(const_num, const_strs[2])
      data <- data.frame(NA, const) %>%
        setNames(c("DateTime", var)) %>%
        u()
    },
    'none'={
      warning("need type!='none' for needed var ", var)
    },
    warning("unrecognized type for needed var ", var)
  )
  data
}