#' Get a table of information on ts files for the specified var_src (or all 
#' var_srces)
#' 
#' The uploaded_by column is only available when you are logged in to
#' ScienceBase.
#' 
#' @param var_src one or more var_srces to include in the query. if missing, all
#'   var_src values from get_var_src_codes(out='var_src') will be included (may 
#'   be slow!)
#' @import sbtools
#' @import jsonlite
#' @import dplyr
#' @export
summarize_ts_files <- function(var_src) {
  if(missing(var_src)) var_src <- get_var_src_codes(out='var_src')
  fi_cols <- c('name','size','dateUploaded','uploadedBy')
  name <- dateUploaded <- site_name <- is_archive <- size <- upload_date <- 
    creation_date <- uploadedBy <- ts_item <- site_item <- file_name <- 
    site_name <- scheme <- type <- key <- '.dplyr.var'
  
  bind_rows(lapply(var_src, function(vs) {
    # query for tses matching the ts type for var_src. custom query gets us the
    # 'identifiers' field plus the filtering
    ts_query_tag <- list(
      fields='identifiers',
      type='json',
      filter=paste0('itemIdentifier=', jsonlite::toJSON(list('scheme'=get_scheme(), 'type'=make_ts_name(vs)), auto_unbox=TRUE))
    )
    ts_items_by_tag <- query_sb(ts_query_tag, limit=10000)
    
    # query by title - this is as close as we can get in O(1) to querying by dir
    # for all the tses. by_dir below should end up being accurate because a
    # query by dir looks for a site parent and a ts title.
    ts_query_title <- list(
      fields='identifiers',
      type='json',
      q=make_ts_name(vs)
    )
    ts_items_by_title <- query_sb(ts_query_title, limit=10000)
    
    # merge the query results
    ts_items <- union(ts_items_by_tag, ts_items_by_title)
    
    # summarize the ts query results
    sumry <- if(length(ts_items) > 0) {
      bind_rows(lapply(ts_items, function(tsi) {
        smry_items <- data_frame(
          ts_title=tsi$title,
          ts_item=tsi$id,
          site_item=tsi$parentId)
        smry_ids <- 
          if(exists('identifiers', tsi)) { 
            data_frame(
              scheme=tsi$identifiers[[1]]$scheme,
              type=tsi$identifiers[[1]]$type,
              key=tsi$identifiers[[1]]$key)
          } else as.data.frame(as.list(setNames(rep('', 3), c('scheme','type','key'))), stringsAsFactors=FALSE)
        smry_files <- 
          if(length(tsi$files) > 0) {
            bind_rows(lapply(tsi$files, function(fi) {
              missing_fi_cols <- fi_cols[!(fi_cols %in% names(fi))] 
              for(fic in missing_fi_cols) fi[[fic]] <- NA
              as.data.frame(fi[fi_cols], stringsAsFactors=FALSE) %>%
                rename(file_name=name)
            }))
          } else as.data.frame(setNames(list('',0,'',''), c('file_name','size','dateUploaded','uploadedBy')), stringsAsFactors=FALSE)
        data.frame(smry_items, smry_ids, smry_files, check.rows=FALSE, row.names=NULL, stringsAsFactors=FALSE)
      }))
    } else {
      as.data.frame(
        setNames(list('','','','','','','',0,'','',''), 
                 c('ts_title','ts_item','site_item','scheme','type','key','file_name','size','dateUploaded','uploadedBy')), 
        stringsAsFactors=FALSE)[c(),]
    } 
    
    # query for sites and match to parentIds of tses; could get this info from
    # parse_ts_path(sumry$file_name, but this way we confirm parentage such that
    # we're confident a query by dir would also find this ts)
    site_items <- query_item_identifier(scheme=get_scheme(), type='site_root', limit=10000)
    site_sumry <- bind_rows(lapply(site_items, function(si) {
      data.frame(parent_name=si$title, site_item=si$id, stringsAsFactors=FALSE)
    }))
    
    # merge the site info into the ts info
    sumry <- left_join(sumry, site_sumry, by='site_item')
    
    # filter out ts items with no files (they mess up parse_ts_path below)
    nofiles <- which(sumry$file_name == '')
    if(length(nofiles) > 0) {
      warning('ts items without files: ', paste0(sumry$parent_name[nofiles], '-', sumry$ts_title[nofiles], collapse=', '))
      sumry <- sumry[-nofiles,]
    }
    
    # parse ts path info and reformat df
    sumry <- 
      data.frame(
        sumry,
        if(nrow(sumry) > 0) {
          parse_ts_path(sumry$file_name, out=c('version','site_name','var_src','is_archive','creation_date'), use_names=FALSE)
        } else {
          list(character(), character(), character(), logical(), character()) %>%
            setNames(c('version','site_name','var_src','is_archive','creation_date')) %>%
            as.data.frame(stringsAsFactors=FALSE)
        },
        stringsAsFactors=FALSE) %>%
      mutate(upload_date=as.POSIXct(dateUploaded, format="%Y-%m-%dT%H:%M:%OSZ", tz='UTC'))
    
    # determine whether the ts should be discoverable by tag and/or dir
    sumry <- mutate(
      sumry,
      by_tag=(!is.na(scheme) & scheme==get_scheme() & 
                !is.na(type) & type==make_ts_name(vs) &
                !is.na(key) & key==site_name), 
      by_dir=(!is.na(ts_title) & ts_title==make_ts_name(vs) &
                !is.na(parent_name) & parent_name == site_name)
    )
    
    # select final columns and return
    sumry %>%
    select(var_src, site_name, version, is_archive, size, upload_date, creation_date, uploaded_by=uploadedBy, 
           ts_item, scheme, type, key, site_item, by_tag, by_dir, file_name)
  }))
}