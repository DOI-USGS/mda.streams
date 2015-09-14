#' Select the specific data source to use for a given variable and site
#' 
#' Var and site are always required fields. logic is required by has a default 
#' value that may be used without explict specification. For every element of 
#' logic that equals one of the default options, the specified chosing algorithm
#' will be used to supply values for type and src. If logic is unrecognized, 
#' then type and src are also required and the row will be constructed from the 
#' manually specified elements of site, type, src, and logic.
#' 
#' @param var character. A single variable name as in 
#'   \code{unique(get_var_src_codes(out="var"))}.
#' @param site character. The site code or codes for which to choose data.
#' @param logic character specifying the algorithm to use, or the manual logic 
#'   that was used by the user, to choose the \code{type} and \code{src}. if an
#'   element of logic is not one of the options given in the argument
#'   definition, then the corresponding elements of \code{type} and \code{src}
#'   must be specified.
#' @param type character in \code{c("ts","meta","file","const")}.
#' @param src character indicating a src to be interpreted in the context of 
#'   \code{type}
#' @return a 4-column data.frame component of a config file
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' choose_data_source(var="baro", site="nwis_03067510", logic='priority local')
#' # expect warnings:
#' choose_data_source(var="baro", site=c("nwis_08062500","nwis_03067510"), 
#'   logic='my made-up data', type="file", src=c("myfile1.txt","myfile2.txt"))
#' choose_data_source(var="dosat", site="nwis_03067510", logic='priority local')
#' 
#' # as in default to stage_metab_config
#' site=list_sites(c("doobs_nwis","disch_nwis","wtr_nwis"))[40:49]
#' sitetime=choose_data_source("sitetime", site)
#' doobs=choose_data_source("doobs", site)
#' dosat=choose_data_source("dosat", site)
#' depth=choose_data_source("depth", site)
#' wtr=choose_data_source("wtr", site)
#' par=choose_data_source("par", site)
#' 
#' # as in verify_config
#' choose_data_source(var="doobs", site="dummy_site", logic="manual", type="ts", src="simModel")
#' 
#' # for K
#' K600=choose_data_source("K600", "nwis_08062500")
#' login_sb()
#' K600=choose_data_source(var="K600", site="nwis_08062500", logic="nighttime reg", 
#'   type="pred", src="0.0.6")
#' K600=choose_data_source(var="K600", site="nwis_08062500", logic="nighttime reg", 
#'   type="pred", src="nwis_08062500-307-150730 0.0.6 nighttime_k_plus_data")
#' }
choose_data_source <- function(var, site, logic=c('priority local', 'unused var'), type=c(NA,"ts","meta","file","const"), src) {

  # check args
  if(length(var) != 1) stop("exactly 1 var required")
  if(missing(site)) stop("site argument is required")
  if(missing(logic)) logic <- match.arg(logic)
  
  # read options from function formals
  logic_options <- eval(formals(choose_data_source)$logic)
  
  # start the partial-config df, using just site and logic until we know we need
  # type & src. creating the data.frame now will permit type, site, src, and/or
  # logic to be replicated automatically if only one value is given
  config <- data.frame(
    type=if(missing(type)) NA else type, 
    site=site, 
    src=if(missing(src)) NA else src, 
    logic=logic,
    stringsAsFactors=FALSE)
  
  # special case: if site is NA, we're just looking for an empty data.frame
  if(isTRUE(is.na(site))) {
    config$logic <- 'site=NA'
    return(config)
  }
  
  # setup operations & checks as needed
  if('pred' %in% config[,'type']) {
    metab_model_list <- list_metab_models() # will break if needed but not logged into SB, so try it early
  }
  if('priority local' %in% logic || 'ts' %in% type) {
    priority <- '.dplyr.var'
    myvar <- var # need a name that differs from the var_src_codes col name
    ranked_src <- get_var_src_codes(var==myvar, out=c("src","priority")) %>% arrange(priority)
    
    if('priority local' %in% logic) {
      # the following should ~minimize the time*number of SB queries. it creates a
      # table of T/F values where rows are sites, cols are srces, and a cell is T
      # if the src exists for that site
      site_has_ts <- data.frame(site=config[config$logic=='priority local','site'])
      for(psrc in ranked_src$src) { site_has_ts[psrc] <- FALSE }
      if(nrow(site_has_ts) > nrow(ranked_src)*2) { # loop by src if there are many sites
        for(psrc in ranked_src$src) {
          sitelist <- list_sites(make_var_src(var,psrc))
          site_has_ts[site_has_ts$site %in% sitelist, psrc] <- TRUE
        }
      } else { # loop by site if there are few sites
        for(s in site_has_ts$site) {
          varsrclist <- grep(paste0("^", var, "_"), list_datasets(site_name=s, data_type='ts'), value=TRUE)
          site_has_ts[site_has_ts$site==s, parse_var_src(varsrclist, out="src")] <- TRUE
        }
      }
    }    
  }
  if(any(!(logic %in% logic_options))) {
    if(missing(type) || missing(src)) {
      stop("when logic includes one or more unrecognized (manual) values, type and src are required")
    }
  }
  for(row in 1:length(logic)) {
    if(logic[row] %in% logic_options) {
      # require initial NAs in type and src
      if(!is.na(config[row,'type'])) stop("expected type=NA for automatic data choice in row ", row)
      if(!is.na(config[row,'src'])) stop("expected src=NA for automatic data choice in row ", row)
    }
  }
  
  # determine each config row separately, according to the logic in that row
  for(row in 1:nrow(config)) {
    switch(
      config[row,'logic'],
      
      # automatic specification of all fields
      'priority local'={ 
        # we've done most of the work above by creating a table of which srces
        # are available for each site. use that now to pick the first (leftmost)
        # available source for each site.
        bestsrc <- names(which(unlist(site_has_ts[site_has_ts$site==config[row,'site'],-1,drop=FALSE]))[1])
        if(length(bestsrc) == 1) {
          config[row,'src'] <- bestsrc
          config[row,'type'] <- 'ts'
        }
        
        # if there wasn't a good data option, tell the user
        if(is.na(config[row,'src'])) 
          warning("could not locate an appropriate ts for site ", config[row,'site'], ", row ", row)
      },
      
      # automatic specification that this var will not be used
      'unused var'={
        config[row,'type'] <- 'none'
        config[row,'site'] <- NA
        config[row,'src'] <- NA
      },
      
      # if logic is an unknown term, use manual specification of all fields.
      # type and src are already in the config for this row, so just check their
      # validity
      {
        if(is.na(config[row,'type']))
          stop("need non-NA type in row ", row)
        
        switch(
          config[row,'type'],
          ts={
            if(!(config[row,'src'] %in% ranked_src$src))
              stop("in row ", row, " need src from c(", paste0(ranked_src$src, collapse=","), ")")
          },
          meta={
            warning("meta not currently implemented")
          },
          file={
            if(!file.exists(config[row,'src']))
              warning("file in row ", row, " doesn't exist on this machine")
          },
          const={
          },
          pred={
            # determine whether the metab model has been specified by its full
            # title or just by its tag. if it's just the tag, try to find the
            # specific model
            parsed_mm_name <- parse_metab_model_name(config[row,'src'])
            if(any(is.na(c(as.matrix(parsed_mm_name))))) {
              mm_name <- grep(paste0("^",config[row,'site'],"-.*",config[row,'src']), metab_model_list, value=TRUE)
              if(length(mm_name) != 1) {
                warning("possible metab model names for site=",config[row,'site'],", src=",config[row,'src'],":\n",paste0(mm_name,collapse="\n"))
                stop(paste0("couldn't find exactly 1 metab model name in row ",row))
              } else {
                config[row,'src'] <- mm_name
              }
            } else {
              # if parsed_mm_name was complete, then we only need to confirm that config[row,'src'] refers to a real model
              if(!(config[row,'src'] %in% metab_model_list))
                warning("in row ", row, " found src that's not in list_metab_models()")
            }
          },
          none={
            if(!is.na(config[row,'site'])) stop('when type=none need site=NA')
            if(!is.na(config[row,'src'])) stop('when type=none need src=NA')
          },
          stop("type in row ", row, " is invalid: ", config[row,'type'])
        )
      }
    )
  }
  
  config
}

# type     site       src           logic
# file     NA         ~\dat\xx.tsv  hiGPPER loK
# const    NA         75000,Pa      elev mean
# ts       nwis_07    nwis          local best
# ts       nwis_08    simModel      proxy best
# meta     mwis_09    calcElev      proxy best
# pred     nwis_10    0.0.6         nighttime reg