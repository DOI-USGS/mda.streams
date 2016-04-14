#' Get metadata (site data) from ScienceBase
#' 
#' Keeps a locally cached copy to reduce the size and number of requests to SB. 
#' 
#' @param types one or more metadata types to select and merge into a single 
#'   table. see the options with list_metas(); all are returned by default.
#' @param out character vector or 'all'. if 'all', all columns from the selected
#'   types will be returned. if anything else, the selected columns will be 
#'   returned.
#' @param on_local_exists character indicating what to do if the folder already 
#'   contains a file with the intended download name
#' @return a metadata table
#' @import sbtools
#' @import dplyr
#' @importFrom lubridate with_tz
#' @importFrom unitted u
#' @export
get_meta <- function(types=list_metas(), out='all', on_local_exists=c('skip','replace')) {
  
  on_local_exists <- match.arg(on_local_exists)
  
  # check and re-download each of the requested tables as needed
  updated_data <- sapply(types, function(type) {
    
    # define pkg.env variable names
    meta_type <- paste0('meta_', type)
    meta_type_timestamp <- paste0(meta_type,'_timestamp')
    
    # determine whether a new download or refresh is needed
    needs_download <- 
      if(!exists(meta_type, envir=pkg.env)) {
        TRUE
      } else {
        # if already cached, check timestamp
        switch(
          on_local_exists,
          replace = {
            cache_timestamp <- get(x=meta_type_timestamp, envir=pkg.env)
            sb_timestamp <- item_get(locate_meta(type))$files[[1]]$dateUploaded %>%
              as.POSIXct("%Y-%m-%dT%H:%M:%S", tz="UTC")
            sb_timestamp > cache_timestamp
          },
          skip= {
            FALSE
          })
      }
    
    # download and cache if needed
    if(needs_download) {
      new_meta <- read_meta(download_meta(type, on_local_exists = 'replace'))
      new_timestamp <- with_tz(Sys.time(), 'UTC')
      assign(x=meta_type, value=new_meta, envir=pkg.env)
      assign(x=meta_type_timestamp, value=new_timestamp, envir=pkg.env)
    }
    
    needs_download
  })
  
  # either create+save or get the full metadata table
  if(any(updated_data)) {
    # determine what tables we ought to have now
    cached_types <- grep("^meta_[[:alpha:]]+$", ls(pkg.env), value=TRUE)
    cached_types <- substring(cached_types[!cached_types %in% c("meta_all","meta_delim","meta_extension")], 6)
    # load the individual metadata tables
    metas <- lapply(cached_types, function(type) {
      # always use the [updated] cached value here - quicker
      meta_type <- paste0('meta_', type)
      meta <- get(meta_type, envir=pkg.env)
      # add the type to the column names
      if(type != 'basic') {
        names(meta)[names(meta) != 'site_name'] <- paste0(type, ".", names(meta)[names(meta) != 'site_name'])
      }
      meta[[paste0('row_in.', type)]] <- u(rep(TRUE, nrow(meta)), NA)
      meta
    })
    # combine the metadata tables into a single table
    data <- do.call(
      combine_tables, 
      c(metas, list(by='site_name', fun=combine_dplyr('full_join', by='site_name'), allow_constants=FALSE)))
    assign(x='meta_all', value=data, envir=pkg.env)
  } else {
    data <- get('meta_all', envir=pkg.env)
  } 
  
  # subset by columns if requested
  if(length(out) == 1 && out == 'all') {
    # basic columns are those without '.', but if we're not returning basic, just include site_name
    basic_cols <- if('basic' %in% types) which(!grepl('.', names(data), fixed=TRUE)) else grep('site_name', names(data))
    # cols from other tables start with the corresponding 'type.'
    other_cols <- unlist(lapply(types[types!='basic'], function(type) {
      which(grepl(paste0('^',type,'\\.'), names(data)))
    }))
    # combine into one vector
    out <- c(basic_cols, other_cols)
  }
  # figure out which rows are relevant
  out_rows <- which(unname(apply(v(data)[paste0('row_in.', types)], 1, any)))
  # subset to relevant rows and requested cols
  data <- unique(data[out_rows, out, drop=FALSE])
  
  data
}