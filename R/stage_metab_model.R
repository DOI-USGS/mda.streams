#' Stage metabolism model outputs into individual metab_model .RData files
#' 
#' Parse a list of metab_outs into individual .RData files
#' 
#' @param title the title of the metab modeling run whose results are in
#'   metab_outs
#' @param metab_outs a single metab_model or a list of metab_models from which
#'   to extract the metab_model objects
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param verbose provide verbose output (currently not implemented)
#' @return a character vector of file handles
#' @importFrom unitted u v
#' @import streamMetabolizer
#' @import dplyr
#' @export
stage_metab_model <- function(title, metab_outs, folder = tempdir(), verbose = FALSE) {
  # check inputs
  if(!is.list(metab_outs)) metab_outs <- list(metab_outs)
  
  staged <- unname(unlist(lapply(1:length(metab_outs), function(out_num) {
    
    # select the list element
    mm <- metab_outs[[out_num]]
    
    # pull info from the model
    config_row <- get_info(mm)
    site <- config_row[[1,"site"]]
    
    # check that info and element number match
    row_num <- as.numeric(rownames(config_row))
    if(out_num != row_num) warning("couldn't reconcile out_num with rownames(get_info(mm)); using rownames")
    
    # save mm to its own .RData file
    file_path <- make_metab_model_path(model_name=make_metab_model_name(title, row_num, site), folder=folder)
    save(mm, file=file_path)
    file_path
    
  })))
  
  staged
}