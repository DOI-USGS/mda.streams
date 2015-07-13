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
  
  # setup operations & checks as needed
  if('priority local' %in% logic || 'ts' %in% type) {
    priority <- '.dplyr.var'
    myvar <- var # need a name that differs from the var_src_codes col name
    ranked_src <- get_var_src_codes(var==myvar, out=c("src","priority")) %>% arrange(priority)
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
        # find all available data options for this site & var
        all_ts <- list_datasets(site_name=config[row,'site'], data_type='ts')
        # look for the best of the available datasets
        for(p in 1:nrow(ranked_src)) {
          if(make_var_src(var, ranked_src[p,"src"]) %in% all_ts) {
            config[row,'type'] <- 'ts'
            config[row,'src'] <- ranked_src[p,"src"]
            break
          }
        }
        
        # if there wasn't a good data option, tell the user
        if(is.na(config[row,'type'])) warning("could not locate an appropriate ts for site ", site, ", row ", row)
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