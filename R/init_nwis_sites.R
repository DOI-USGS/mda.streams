#'@title generate nwis site_ids for given p_codes
#'@description finds all NWIS sites that meet given data requirements
#'
#'@param p_codes a character vector of NWIS p_codes
#'@return a character vector of NWIS sites, appended with 'nwis_'
#'
#'@examples
#'\dontrun{
#'init_nwis_sites(p_codes = c('00010', '00060', '00095', '00300'))
#'}
#'@import dataRetrieval
#'@export
init_nwis_sites <- function(p_codes){
  
  data(states)
  
  
  # get sites for Stets:
  
  # loop states and param list, store only sites that have all. Handle connection errors w/ retry.
  time.limit = data.frame(site_id=NULL,state=NULL,start=NULL, end=NULL)
  
  n.param <- length(p_codes)
  
  get.state.sites <- function(state,n.param, params){
    site.list.all <- list() # site list with all params
    for (i in 1:n.param){
      # first call state sites for param
      state.url <- paste("http://waterservices.usgs.gov/nwis/site/",
                         "?format=rdb",
                         "&stateCd=",state,
                         "&parameterCd=",params[i],
                         "&hasDataTypeCd=iv",sep='')
      cat(state.url)
      # need to handle 404 and other errors
      site.file <- get.site.data(state.url)
      if (any(is.na(site.file))){
        return(NA)
      }
      # this gives number of sites in this state
      n.sites <- length(site.file$site_no)
      site.list.all <- c(site.list.all, paste(site.file$site_no,collapse=','))
      cat('\n')
      
    }
    return(site.list.all)
  }
  
  
  # now find all sites that exist in all parts of the site list
  
  get.site.matches <- function(site.list.all,n.param){
    all.sites.dup <- paste(unlist(site.list.all),collapse=',') # all sites with replicates
    all.sites.vec <- strsplit(all.sites.dup,',')[[1]]
    
    unique.sites <- unique(all.sites.vec)
    n.unique <- length(unique.sites)
    use.sites <- vector(length=n.unique)
    for (i in 1:n.unique){
      if (sum(all.sites.vec==unique.sites[i])==n.param)
        use.sites[i] = TRUE
    }
    return(unique.sites[use.sites])
  }
  
  
  
  
  
  get.site.data <- function(site.url){
    siteFile <- tryCatch({
      siteFile <- read.delim(  
        site.url, 
        header = TRUE, 
        quote="\"", 
        sep='\t',
        colClasses=c('character'),
        fill = TRUE, 
        comment.char="#")
      siteFile <- siteFile[-1,]
      # assumes rdp format, and single row of headers  
    }, warning = function(war) {
      print(paste("MY_WARNING:  ",war))
      # warning handler picks up where error was generated
      siteFile <- c(NA,NA)
      return(NA)
    }, error = function(err) {
      print(paste("MY_ERROR:  ",err))
      return(NA)
    }
    )
    
    return(siteFile) # get rid of top header
  }
  
  getParamStartEndUV <- function(site.id,param){
    data.avail <- whatNWISdata(site.id,service = "uv")
    param.start <- data.avail[data.avail$parm_cd==param,]$begin_date
    param.end <- data.avail[data.avail$parm_cd==param,]$end_date
    return(data.frame("start"=param.start,"end"=param.end))
  }
  
  getTimeLimit <- function(site.id,param.cd){
    # returns the shortest time period where all data overlap. NA if none
    # params is character vector
    # site.id is a string
    stop.time <- "2100-01-01" # supposed to be GREATER than any in NWIS
    start.time <- "1700-01-01" # supposed to be LESS than any in NWIS
    for (p in 1:length(param.cd)){
      # can be 404, can be empty
      start.stop.df <- getParamStartEndUV(site.id,param.cd[p])[1,]
      # test right, test left
      if (as.Date(start.stop.df$start)>as.Date(start.time)){
        start.time = start.stop.df$start
      }
      if (as.Date(start.stop.df$end)<as.Date(stop.time)){
        stop.time = start.stop.df$end
      }
      # if any are empty, break with NA
    }
    return(data.frame("start"=start.time,'end'=stop.time))
  }
  append.times <- function(state.list,time.limit,state.nm, params){
    t.df <- data.frame(start=NULL,end=NULL)
    for (s in 1:length(state.list)){
      t.l <- getTimeLimit(site.id=state.list[s],param.cd=params)
      t.df = rbind(t.df,data.frame(start=t.l$start,end=t.l$end))
      
    }
    states <- rep(state.nm,length(state.list))
    t.df = cbind(state=states,t.df)
    t.df = cbind(site_id=state.list,t.df)
    time.limit = rbind(time.limit,t.df)
    return(time.limit)
  }
  
  state.nm <- names(states)
  for (s in 1:length(state.nm)){#
    
    state = state.nm[s]
    cat(state);cat('\n')
    
    state.list <- get.state.sites(state,n.param, p_codes)
    cat("matches\n")
    state.list <- get.site.matches(state.list,n.param)
    cat('matches complete\n')
    if (length(state.list)>0 & !is.na(state.list[1])){
      print(state.list)
      time.limit <- append.times(state.list,time.limit,state.nm=state, params = p_codes)
      print(time.limit)
    }
  }
  nwis_sites <- paste('nwis_',as.character(time.limit$site_id),sep='')
  return(nwis_sites)
}