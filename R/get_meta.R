#' Get metadata from one or more meta_xxx files
#' 
#' Keeps a locally cached copy to reduce the size and number of requests to SB. 
#' If
#' 
#' @param types one or more metadata types to select and merge into a single 
#'   table. see the options with list_metas(); all are returned by default.
#' @param out character vector or 'all'. if 'all', all columns from the selected
#'   types will be returned. if anything else, the selected columns will be 
#'   returned.
#' @return a metadata table
#' @import sbtools
#' @import dplyr
#' @importFrom lubridate with_tz
#' @export
get_meta <- function(types=list_metas(), out='all') {
  
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
        cache_timestamp <- get(x=meta_type_timestamp, envir=pkg.env)
        sb_timestamp <- item_get(locate_ts("doobs_nwis", "nwis_01458500"))$files[[1]]$dateUploaded %>%
          strptime("%Y-%m-%dT%H:%M:%S", tz="UTC")
        sb_timestamp > cache_timestamp
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
    # load the individual metadata tables
    metas <- lapply(types, function(type) {
      # always use the [updated] cached value here - quicker
      meta_type <- paste0('meta_', type)
      meta <- get(meta_type, envir=pkg.env)
      # add the type to the column names
      if(type != 'basic') {
        names(meta)[names(meta) != 'site_name'] <- paste0(type, ".", names(meta)[names(meta) != 'site_name'])
      }
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
    # 
    out <- c(basic_cols, other_cols)
  }
  data <- unique(data[,out])
  
  data
}