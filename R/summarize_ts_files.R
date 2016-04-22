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
#' @import dplyr
#' @export
summarize_ts_files <- function(var_src) {
  if(missing(var_src)) var_src <- get_var_src_codes(out='var_src')
  fi_cols <- c('name','size','dateUploaded','uploadedBy')
  name <- dateUploaded <- site_name <- is_archive <- size <- upload_date <- 
    creation_date <- uploadedBy <- ts_item <- site_item <- file_name <- '.dplyr.var'
  
  bind_rows(lapply(var_src, function(vs) {
    ts_items <- query_item_identifier(scheme=get_scheme(), type=make_ts_name(vs), limit=10000)
    
    sumry <- bind_rows(lapply(ts_items, function(tsi) {
      if(length(tsi$files) > 0) {
        smry_items <- data_frame(
          ts_item=tsi$id,
          site_item=tsi$parentId)
        smry_files <- bind_rows(lapply(tsi$files, function(fi) {
          missing_fi_cols <- fi_cols[!(fi_cols %in% names(fi))] 
          for(fic in missing_fi_cols) fi[[fic]] <- NA
          as.data.frame(fi[fi_cols], stringsAsFactors=FALSE) %>%
            rename(file_name=name)
        }))
        data.frame(smry_items, smry_files, check.rows=FALSE, row.names=NULL, stringsAsFactors=FALSE)
      } else {
        NULL
      }
    }))
    data.frame(
      sumry,
      parse_ts_path(sumry$file_name, out=c('version','site_name','var_src','is_archive','creation_date'), use_names=FALSE),
      stringsAsFactors=FALSE) %>%
      mutate(upload_date=as.POSIXct(dateUploaded, format="%Y-%m-%dT%H:%M:%OSZ", tz='UTC')) %>%
    select(var_src, site_name, version, is_archive, size, upload_date, creation_date, uploaded_by=uploadedBy, ts_item, site_item, file_name)
  }))
}