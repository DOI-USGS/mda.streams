#' Verify that a config file row has the requisite info
#' 
#' Checks the config row for valid src-site-logic pairs with respect to a given 
#' model
#' 
#' @param config a config df to verify
#' @param checks a character vector of tests to run
#' @param on_fail the function to apply to the error message[s] if a test fails
#' @return TRUE if is valid, FALSE if not
#' @export
#' @examples
#' egconfig <- suppressWarnings(
#'   stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'     site="nwis_04087142", filename=NULL))
#' verify_config(egconfig)
verify_config <- function(config, checks=c('names'), on_fail=warning) {
  
  tests <- list(
    'names' = function(x,...) {
      # determine which columns should exist based on stage_metab_config
      expected_colnames <- names(suppressWarnings(
        stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=NULL, omit_incomplete=FALSE)))
      if(!isTRUE(all.equal(names(x), expected_colnames))) {
        extras <- setdiff(names(x), expected_colnames)
        if(length(extras)>0) 
          warning("config has these unexpected columns: ", paste0(extras, collapse=", "))
        missings <- setdiff(expected_colnames, names(x))
        if(length(missings)>0) 
          warning("config is missing these columns: ", paste0(missings, collapse=", "))
        FALSE
      } else {
        TRUE
      }
    })
  
  pass <- TRUE
  for (check in checks){
    if (!tests[[check]](x=config)) {
      on_fail("verify_ts failed on test for ", check)
      pass <- FALSE
    }
  }
  return(pass)
}