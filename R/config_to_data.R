#' Convert config specifications into a data input table
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
#' @importFrom stats setNames
#' @import streamMetabolizer
#' @examples 
#' \dontrun{
#' depth_file <- download_ts("depth_calcDischHarvey", "nwis_04087142", 
#'   on_local_exists="replace")
#' config <- stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", dosat=choose_data_source("dosat", "nwis_04087142", 
#'     logic="simple dosat", type="const", src="12,mg L^-1"), 
#'   depth=choose_data_source("depth", "nwis_04087142", logic="local file", 
#'     type="file", src=depth_file), filename=NULL)
#' cdat <- config_to_data(config[1,], row_num=1, metab_fun=metab_mle, metab_args=list())
#' names(cdat)
#' head(cdat[['data']])
#'  
#' login_sb()
#' site="nwis_01646000"
#' config <- stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   model="metab_Kvpred", site=site, filename=NULL,
#'   sitetime=choose_data_source("sitetime", site, logic="unused var"),
#'   doobs=choose_data_source("doobs", site, logic="unused var"),
#'   dosat=choose_data_source("dosat", site, logic="unused var"),
#'   depth=choose_data_source("depth", site, logic="unused var"),
#'   wtr=choose_data_source("wtr", site, logic="unused var"),
#'   par=choose_data_source("par", site, logic="unused var"),
#'   sitedate=choose_data_source("sitedate", site, logic="priority local"),
#'   K600=choose_data_source("K600", site, logic="nighttime", src="0.0.6", type="pred"),
#'   dischdaily=choose_data_source("dischdaily", site, logic="priority local"),
#'   velocdaily=choose_data_source("velocdaily", site, logic="priority local"),
#'   omit_incomplete=FALSE)
#' cdat <- config_to_data(config[1,], row_num=1, 
#'   metab_fun=streamMetabolizer::metab_Kvpred, metab_args=list())
#' }
#' @export
config_to_data <- function(config_row, row_num, metab_fun, metab_args, on_error=c('stop','warn','quiet')) {

  # process inputs
  on_error <- match.arg(on_error)
  
  # get a lookup table to translate between mda.streams vars and metab vars
  var_lookup <- unique(get_var_src_codes(out=c("metab_var","var")))
  
  # get a list of vars and the relevant columns supplied in the config_row
  spec_suffixes <- colnames(choose_data_source(var="doobs", site=NA, logic="unused var"))
  spec_prefixes <- gsub(".src$", "", names(config_row)[grepl(".src$", names(config_row))])
  spec_data_names <- var_lookup[match(spec_prefixes, var_lookup$var),"metab_var"]
  data_specs <- lapply(setNames(spec_prefixes,spec_data_names), function(var) {
    setNames(paste0(var, ".", spec_suffixes), spec_suffixes)
  })
  
  # look to formals(metab_fun) for a list of vars for which we expect complete info
  data_needs_list <- var_needs_list <- optional_list <- list() 
  data_args <- intersect(c('data','data_daily'), names(formals(metab_fun)))
  for(possible_arg in data_args) {
    arg_default <- eval(formals(metab_fun)[[possible_arg]])
    data_needs_list[[possible_arg]] <- colnames(arg_default)
    var_needs_list[[possible_arg]] <- var_lookup[match(colnames(arg_default), var_lookup$metab_var),"var"]
    optional_list[[possible_arg]] <- attr(arg_default, 'optional')
  }

  # prepare vectors to collect any errors/warnings
  err_strs <- warn_strs <- character()

  # check the vars for which we expect the specs to be NAs
  data_ignored <- which(names(data_specs) %in% setdiff(names(data_specs), c(data_needs_list[['data']],data_needs_list[['data_daily']])))
  for(ignored in data_ignored) {
    type <- config_row[[1,data_specs[[ignored]][['type']]]]
    site <- config_row[[1,data_specs[[ignored]][['site']]]]
    src <- config_row[[1,data_specs[[ignored]][['src']]]]
    
    if(is.na(type) || type != 'none') {
      warn_strs <- c(warn_strs, paste0("need type='none' for unused var ", spec_prefixes[ignored]))
    }
  }
    
  final_data_list <- lapply(data_args, function(datatype) {
    
    data_needs <- data_needs_list[[datatype]]
    var_needs <- var_needs_list[[datatype]]
    optional <- optional_list[[datatype]]
    if(length(optional)==1) {
      optional <- switch(
        optional,
        'all'=data_needs,
        'none'=NULL,
        optional)
    }
    
    # check and acquire the vars we need to return
    data_list <- lapply(1:length(data_needs), function(needed) {
      # don't waste time if we've already hit errors
      if(length(err_strs) > 0) return(NA)
      
      var <- var_needs[needed]
      data <- data_needs[needed]
      type <- config_row[[1,data_specs[[data]][['type']]]]
      site <- config_row[[1,data_specs[[data]][['site']]]]
      src <- config_row[[1,data_specs[[data]][['src']]]]
      
      # run config_to_data_column (defined below) in a tryCatch block
      data <- withCallingHandlers(
        tryCatch(
          config_to_data_column(var, type, site, src, optional=(data %in% optional)),
          error=function(e) { 
            err_strs <<- c(err_strs, paste0(var,"-",type,": ",e$message))
            NA 
          }),
        warning=function(w) { 
          warn_strs <<- c(warn_strs, paste0(var,"-",type,": ",w$message))
          invokeRestart("muffleWarning")
        })
      data
    }) %>% setNames(data_needs)
    
    # check for missing and non-optional columns
    omitted_cols <- sapply(data_list, function(d) isTRUE(is.na(d)))
    if(all(omitted_cols) && 'all' %in% optional_list[[datatype]]) {
      return(NULL) # we're allowed to return nothing in this case
    } else if(any(omitted_cols)) {
      if(all(names(which(omitted_cols)) %in% optional)) {
        # if the missing columns are all optional, then we can proceed. just
        # need to lessen our expectations
        data_needs <- data_needs[!omitted_cols]
        var_needs <- var_needs[!omitted_cols]
        data_list[omitted_cols] <- NULL
      } else {
        # these missing cols should already have been identified by warning in
        # config_to_data_column. plan a full-on error here.
        err_strs <- c(err_strs, paste0("required-but-missing data columns in config row ", row_num))
      }
    }
    
    # if something is broken, return without attempting combine_ts
    if(length(err_strs) != 0) return(NA) # NA is the flag for a bad result; won't be deleted when we pare down final_data_list
    
    # combine the data into a single data.frame
    data_df <- withCallingHandlers(
      tryCatch({
        combo <- do.call(combine_ts, c(data_list, list(
          method='approx', approx_tol=as.difftime(if(datatype=="data") 3 else 25, units="hours")
        )))
        
        # restrict dates of data if requested
        if(!is.na(start_date <- as.POSIXct(config_row[[1, "start_date"]], tz="UTC"))) {
          combo <- combo[combo$DateTime >= start_date, ]
        }
        if(!is.na(end_date <- as.POSIXct(config_row[[1, "end_date"]], tz="UTC"))) {
          combo <- combo[combo$DateTime <= end_date, ]
        }
        
        # rename
        combo %>% select_(.dots=var_needs) %>% setNames(data_needs)
      },
      error=function(e) { 
        err_strs <<- c(err_strs, paste0("combine: ",e$message))
        NA # NA is the flag for a bad result; won't be deleted when we pare down final_data_list
      }),
      warning=function(w) { 
        warn_strs <<- c(warn_strs, paste0("combine: ",w$message))
        invokeRestart("muffleWarning")
      })
    
    data_df
  }) %>% setNames(data_args)
  
  # return the results and/or error/warning strings
  if(length(warn_strs) > 0) {
    attr(final_data_list, "warnings") <- warn_strs
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
        attr(final_data_list, "errors") <- err_strs
      },
      'quiet'={
        attr(final_data_list, "errors") <- err_strs
      })
  }
  
  # remove data or data_daily if they were empty and optional (and hence NULL
  # within final_data_list)
  final_data_list[sapply(final_data_list, function(d) isTRUE(is.null(d)))] <- NULL
  final_data_list
  
}

#' Read a single column worth of data
#' 
#' For use within config_to_data
#' 
#' @inheritParams choose_data_source
#' @return a data.frame, else NA
#' @import dplyr
#' @import streamMetabolizer
#' @importFrom unitted u v get_units
#' @importFrom stats setNames
#' @keywords internal
config_to_data_column <- function(var, type, site, src, optional=FALSE) {
  data <- NA
  switch(
    type,
    'ts'={
      num_tries <- 3
      for(i in 1:num_tries) {
        dfile <- tryCatch(
          download_ts(make_var_src(var, src), site, on_local_exists="skip", on_remote_missing="stop"),
          error=function(e) { warning(paste0("download try ",i,": ",e$message)); NULL })
        if(!is.null(dfile)) break else Sys.sleep(1)
      }
      if(is.null(dfile)) stop("failed to download file after ", num_tries, " tries")
      data <- read_ts(dfile)
    },
    'meta'={
      warning("meta type not currently implemented")
    },
    'ts_file'={
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
    'pred'={
      data <- config_preds_to_data_column(var=var, site=site, model_src=src, src_type="SB")
    },
    'pred_file'={
      data <- config_preds_to_data_column(var=var, site=site, model_src=src, src_type="file")
    },
    'none'={
      if(!isTRUE(optional)) {
        stop("need type!='none' for needed var ", var)
      }
    },
    stop("unrecognized type for needed var ", var)
  )
  data
}

#' Get predictions of metabolism or DO from a model
#' 
#' Most of this code is the same whether it's a pred (the name of a model on 
#' sciencebase) or a pred_file (the name of a local file name)
#' 
#' @param var the variable name to retrieve
#' @param model_src the model_name or model file name
#' @param src_type character in c("SB","file") indicating whether model_src is a
#'   model on sciencebase or a local file name
#' @importFrom unitted u v
#' @import streamMetabolizer
#' @import dplyr
#' @keywords internal
config_preds_to_data_column <- function(var, site, model_src=src, src_type="SB") {
  mm <- if(src_type=="SB") {
    get_metab_model(src) 
  } else if(src_type=="file") {
    varname <- load(model_src)
    get(varname) %>% 
      modernize_metab_model()
  }
  local.time <- local.date <- DateTime <- DO.obs <- '.dplyr.var'
  if(var == "doobs") {
    # get key for sitetime
    sitetime_choice <- choose_data_source("sitetime", site, "priority local")
    datetime_key <- config_to_data_column(var="sitetime", type="ts", site, sitetime_choice$src)
    # get predictions
    preds <- predict_DO(mm) %>% 
      mutate(DateTime=datetime_key[match(local.time, datetime_key$sitetime), "DateTime"]) %>%
      select(DateTime, doobs=DO.obs)
    # make sure we only get one obs per local.time, even if the time ranges
    # overlap. pick the first (using match)
    preds <- preds[!is.na(preds$doobs), ]
    unique_pred_times <- unique(preds$DateTime)
    preds <- preds[match(unique_pred_times, preds$DateTime),]
  } else if(var %in% c("gpp","er","K600")) {
    # get key for sitedate
    sitedate_choice <- choose_data_source("sitedate", site, "priority local")
    datetime_key <- config_to_data_column(var="sitedate", type="ts", site, sitedate_choice$src)
    # get predictions
    preds <- predict_metab(mm) %>%
      #mutate(local.time=as.POSIXct(paste0(date, " 12:00:00"), tz="UTC")) %>%
      mutate(DateTime=datetime_key[match(local.date, datetime_key$sitedate), "DateTime"]) %>%
      select_('DateTime', toupper(var))
  }
  # add units and return
  myvar <- var # need just for following line in get_var_src_codes
  varunits <- unique(get_var_src_codes(var==myvar, out='units'))
  data <- preds %>% u(c(NA, varunits))  
  data
}
