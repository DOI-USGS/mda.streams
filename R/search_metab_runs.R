#' Search the metab_runs
#' 
#' @param metab_run_title text to search for within the entire metab_run_title
#' @param date text to search for within the date portion of the model_name
#' @param tag text to search for within the tag portion of the model_name
#' @param strategy text to search for within the strategy portion of the 
#'   model_name
#' @param metab_runs vector of metab_run_titles to search. The default is to
#'   pull this list from ScienceBase
#' @inheritParams search_dataframe
#' @import dplyr
#' @import tibble
#' @export
#' @examples 
#' \dontrun{
#' search_metab_runs(strategy='PR_FIXED')
#' search_metab_runs(date='^1512', fixed=FALSE)
#' search_metab_runs(tag='(0)\\.(0)\\.(1)[2-4]', fixed=FALSE)
#' }
search_metab_runs <- function(metab_run_title=NA, date=NA, tag=NA, strategy=NA, 
                              metab_runs=list_metab_runs(), match_case=FALSE, fixed=TRUE) {
  . <- '.dplyr.var'
  constraints <- 
    c('metab_run_title','date','tag','strategy') %>% setNames(.,.) %>%
    sapply(function(x) eval(as.symbol(x)))
  
  parsed <- parse_metab_run_title(metab_runs) %>% rownames_to_column('metab_run_title')
  
  matches <- search_dataframe(parsed, constraints, match_case=match_case, fixed=fixed)
  
  metab_runs[matches]
}
