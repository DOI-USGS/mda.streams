#' Seek specific file types within a ts sbitem
#' 
#' Converts a list of ts sbitems into a vector of T/Fs for whether the item as a
#' file meeting the criteria
#' 
#' @param ts_items a list of timeseries sbitems
#' @param with_ts_version one or more of \code{c('tsv','rds')} to limit the 
#'   dataset extension to anything in with_ts_version (if the dataset is a ts)
#' @param with_ts_archived one or more of \code{c(TRUE,FALSE)} to limit the list
#'   to sites that have a ts that's archived, not archived, or either
#' @param with_ts_uploaded_after POSIXct, or convertible to POSIXct, giving date
#'   after which a ts must have been uploaded to count
#' @keywords internal
ts_has_file <- function(ts_items, with_ts_version=c('rds','tsv'), 
                        with_ts_archived=c(TRUE, FALSE), with_ts_uploaded_after='2015-01-01') {
  with_ts_version <- match.arg(with_ts_version, several.ok=TRUE)
  out <- sapply(ts_items, function(item) {
    filenames <- sapply(item$files, function(file) file$name)
    uploaddates <- as.POSIXct(sapply(item$files, function(file) file$dateUploaded), format="%Y-%m-%dT%H:%M:%OSZ", tz='UTC')
    if(length(filenames) > 0) {
      parsed <- parse_ts_path(filenames)
      any(parsed$version %in% with_ts_version & 
            parsed$is_archive %in% with_ts_archived &
            uploaddates > as.POSIXct(with_ts_uploaded_after, tz='UTC'))
    } else {
      FALSE
    }
  })
  if(length(out) > 0) out else c()
}
