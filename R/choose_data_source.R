#' Select the specific data source to use for a given variable and site
#' 
#' @param var character. The variable name, e.g. "doobs", "disch", or "wtr"
#' @return a two-column, one-row data.frame with variable-specific column names
#'   and cell values indicating the source type (e.g., "local", "proxy", "model") and
#'   specific source (e.g., "nwis_01474000", "bird")
#' @export
#' @examples
#' choose_data_source("doobs", "nwis_03067510")
choose_data_source <- function(var, site) {
  data.frame("local", site) %>%
    setNames(paste0(c("type", "src")))
}