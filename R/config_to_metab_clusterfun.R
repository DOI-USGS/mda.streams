#' Create a function to pass to cluster nodes
#' 
#' Returns a self-contained function (closure) with all the necessary 
#' information to run \code{\link{config_to_metab}} or 
#' \code{\link{config_to_metab_repeat}} on computer cluster nodes via 
#' \code{\link[parallel]{clusterApply}} or 
#' \code{\link[parallel]{clusterApplyLB}}
#' 
#' @param config a data.frame containing the configuration information, or the 
#'   name of a config file on the computer cluster node.
#' @param return_value character. What should be returned when the function 
#'   completes? "nothing" may be appropriate for many-model runs or large model 
#'   objects.
#' @param repeat_times either NA for no repetition, or an integer number of 
#'   times to repeat the fit for each config row (using config_to_metab_repeat)
#' @param post_metab logical. Should the metab model be staged and posted?
#' @param stage_folder character. The folder name on the computer cluster node 
#'   where the metab_model should be staged before posting (only applies if 
#'   post_metab=TRUE). If NULL, the local tempdir() will be used.
#' @param sb_user the ScienceBase username, only used if post_metab=TRUE
#' @param sb_password the ScienceBase password, only used if post_metab=TRUE
#' @param verbose logical. Should status messages be given?
#' @examples 
#' \dontrun{
#' # might need to run this before calling clusterApplyLB, but might work all in one line
#' clusterApplyLB(c1, 1:nrow(config), config_to_metab_clusterfun(config, rows))
#' }
#' @export
config_to_metab_clusterfun <- function(
  config, return_value=c("metab_model","file_name","nothing"), 
  repeat_times=NA,
  post_metab=TRUE, stage_folder=NULL, sb_user, sb_password, 
  verbose=TRUE) {
  
  # check inputs
  return_value <- match.arg(return_value)
  if(missing(sb_user) || missing(sb_password)) {
    if(isTRUE(post_metab)) {
      stop("if post_metab=T, sb_user and sb_password must be specified")
    }
    if("pred" %in% unlist(config[,grepl("type", names(config))])) {
      stop("if inputs include 'pred', sb_user and sb_password must be specified")
    }
  }
  
  # force evaluation of each argument in this context (return_value already forced)
  force(config)
  force(repeat_times)
  force(post_metab)
  force(stage_folder)
  force(sb_user)
  force(sb_password)
  force(verbose)
  
  function(row_num) {
    # calls to this function should break immediately if at all; no point in even trying if we can't run them
    library(mda.streams)
    if(length(row_num) != 1) stop("expecting exactly 1 row_num per call to clusterfun")
    if("pred" %in% config[row_num,grepl("type", names(config))]) {
      authenticate_sb(sb_user, sb_password)
    }

    # model metabolism & return results or error
    metab_out <- tryCatch({
      if(is.na(repeat_times)) {
        if(verbose) message("running config_to_metab for config row ", row_num, ": starting at ", Sys.time())
        config_to_metab(config=config, rows=row_num, verbose=verbose)[[1]]
      } else {
        if(verbose) message("running config_to_metab_repeat for config row ", row_num, ": starting at ", Sys.time())
        config_to_metab_repeat(config=config, row=row_num, times=repeat_times, verbose=verbose)
      }
    }, 
    error=function(e){e})
    
    # stage & post the output & return filename or error
    file_out <- tryCatch({
      if(isTRUE(post_metab) || return_value=="file_name") {
        # stage/save
        if(verbose) message("saving metab_model for config row ", row_num, ": starting at ", Sys.time())
        stage_title <- make_metab_run_title(
          format(as.Date(config[row_num,'date']), '%y%m%d'), config[row_num,'tag'], config[row_num,'strategy'])
        staged <- stage_metab_model(
          title=stage_title, 
          metab_outs=metab_out, 
          folder=if(is.null(stage_folder)) tempdir() else stage_folder, 
          verbose=verbose)
        
        # post
        if(isTRUE(post_metab)) {
          if(verbose) message("posting metab_model for config row ", row_num, ": starting at ", Sys.time())
          authenticate_sb(sb_user, sb_password)
          post_metab_model(staged)    
        }

        # keep the filename
        staged
      }    
    },
    error=function(e){e})
    
    # return the requested value
    switch(
      return_value,
      metab_model=metab_out,
      file_name=file_out,
      nothing=invisible())
  }
}