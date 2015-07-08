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
config_to_data <- function(config_row, row_num, metab_fun, metab_args) {

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
  
  # prepare a vector to collect any warnings in
  warn_strs <- character()
  
  # check the vars for which we expect the specs to be NAs
  data_ignored <- which(names(data_specs) %in% setdiff(names(data_specs), data_needs))
  for(ignored in data_ignored) {
    type <- config_row[,data_specs[[ignored]][['type']]]
    site <- config_row[,data_specs[[ignored]][['site']]]
    src <- config_row[,data_specs[[ignored]][['src']]]
  
    if(is.na(type) || type != 'none') {
      warn_strs <- c(warn_strs, paste0("need type='none' for unused var ", spec_prefixes[ignored]))
    }
  }
  
  # check and acquire the vars we need to return
  data_list <- lapply(1:length(data_needs), function(needed) {
    var <- var_needs[needed]
    type <- config_row[,data_specs[[needed]][['type']]]
    site <- config_row[,data_specs[[needed]][['site']]]
    src <- config_row[,data_specs[[needed]][['src']]]
    
    data <- NULL
    switch(
      type,
      'ts'={
        data <- tryCatch(
          read_ts(download_ts(make_var_src(var, src), site, on_local_exists="replace")),
          error=function(e) { warn_strs <- c(warn_strs, e); NULL }
        )
      },
      'meta'={
        warn_strs <- c(warn_strs, "meta type not currently implemented")
      },
      'file'={
        data <- read_ts(src)
      },
      'const'={
        const_strs <- strsplit(src, ',')[[1]]
        const_num <- suppressWarnings(as.numeric(const_strs[1]))
        if(length(const_strs) != 2 || is.na(const_num)) {
          warn_strs <- c(warn_strs, "const could not be interpreted as 'number,units'")
        }
        const <- tryCatch(
          u(const_num, const_strs[2]),
          error=function(e) { warn_strs <- c(warn_strs, e) },
          warning=function(w) { warn_strs <- c(warn_strs, w); suppressWarnings(u(const_strs[1], const_strs[2])) }
        )
        data <- data.frame(NA, const) %>%
          setNames(c("DateTime", var)) %>%
          u()
      },
      'none'={
        warn_strs <- c(warn_strs, paste0("need type!='none' for needed var ", var))
      },
      warn_strs <- c(warn_strs, paste0("unrecognized type for needed var ", var))
    )
    data
  })
  
  # if something didn't work, announce it usefully now
  if(length(warn_strs) > 0) {
    lapply(warn_strs, warning)
    stop("could not convert config row ", row_num, " to data")
  }
  
  # combine the data into a single data.frame
  combo <- do.call(combine_ts, c(data_list, list(method='approx', approx_tol=as.difftime(3, units="hours"))))
  combo <- combo %>% select_(.dots=var_needs) %>% setNames(data_needs)
  combo
}