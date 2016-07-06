#' Search the metab_models
#' 
#' @param model_name text to search for within the entire model_name
#' @param title text to search for within the title portion of the model_name
#' @param row text to search for within the row portion of the model_name
#' @param site text to search for within the site portion of the model_name
#' @param date text to search for within the date portion of the model_name
#' @param tag text to search for within the tag portion of the model_name
#' @param strategy text to search for within the strategy portion of the
#'   model_name
#' @param metab_models vector of model_names to search. The default is to pull
#'   this list from ScienceBase
#' @inheritParams search_dataframe
#' @import dplyr
#' @import tibble
#' @export
#' @examples 
#' \dontrun{
#' search_metab_models(site='nwis_08062500')
#' search_metab_models('indy', row="68.", fixed=FALSE)
#' search_metab_models(strategy='NIGHT', site='411955088280601')
#' }
search_metab_models <- function(model_name=NA, title=NA, row=NA, site=NA, date=NA, tag=NA, strategy=NA, 
                                metab_models=list_metab_models(), match_case=FALSE, fixed=TRUE) {
  . <- '.dplyr.var'
  constraints <- 
    c('model_name','title','row','site','date','tag','strategy') %>% setNames(.,.) %>%
    sapply(function(x) eval(as.symbol(x)))

  parsed <- parse_metab_model_name(metab_models) %>% rownames_to_column('model_name')
  
  matches <- search_dataframe(parsed, constraints, match_case=match_case, fixed=fixed)
  
  metab_models[matches]
}
