#' Create/modify the meta_metabinput file
#'
#' @param model the model from which to determine the input dataset column needs
#' @param model_args the model args to use, if applicable, in deriving the input dataset
#' @param folder where to store the staged file
#' @param verbose logical. give status messages?
#' @importFrom unitted u
#' @import dplyr
#' @export
stage_meta_metabinput <- function(model='metab_mle', model_args='list()', folder = tempdir(), verbose = FALSE) {
  
  # get metadata for those sites that have enough data to try. don't bother 
  # including other sites, since these can be merged with the full list in
  # get_meta()
  if(verbose) message("generating config_file")
  config_path <- file.path(folder, "config.tsv")
  if(!file.exists(config_path)) {
    config_sites <- list_sites(list(
      "sitetime_calcLon", 
      "doobs_nwis", 
      any=c("dosat_calcGGbts","dosat_calcGGbconst"),
      "depth_calcDisch",
      "wtr_nwis",
      any=c("par_nwis","par_calcLat")), 
      logic="all")
    config_file <- stage_metab_config(
      tag='0.0.0', strategy='stage_meta_metabinput', 
      model=model, model_args=model_args,
      site=config_sites, filename=config_path)
  } else {
    if(verbose) message("using existing config_file; to replace change the tag, strategy, or file")
    config_file <- config_path
  }  
  meta_mi <- summarize_metab_inputs(config_file)
  names(meta_mi)[1] <- "site_name"
  
  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(meta_mi)
  } else {
    fpath <- make_meta_path(type='metabinput', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(meta_mi, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(invisible(fpath))
  }
  
}