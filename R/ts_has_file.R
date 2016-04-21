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
#' @keywords internal
ts_has_file <- function(ts_items, with_ts_version=c('rds','tsv'), with_ts_archived) {
  with_ts_version <- match.arg(with_ts_version, several.ok=TRUE)
  out <- sapply(ts_items, function(item) {
    filenames <- sapply(item$files, function(file) file$name)
    if(length(filenames) > 0) {
      parsed <- parse_ts_path(filenames)
      any(parsed$version %in% with_ts_version & 
            parsed$is_archive %in% with_ts_archived)
    } else {
      FALSE
    }
  })
  if(length(out) > 0) out else c()
}
