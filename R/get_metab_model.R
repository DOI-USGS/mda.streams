#' Load one or more metab_model objects into R
#' 
#' Uses a previously-downloaded copy of this metab_model unless (1) that 
#' download occurred in a different R session, or (2) on_local_exists-'replace'.
#' 
#' @param model_name the name of the metab_model file
#' @inheritParams download_item_files
#' @param version character. which version of the model should be returned - 
#'   ('original') the original, or ('modern') one that's been updated to work 
#'   with the current streamMetabolizer version? 'modern', the default, is 
#'   recommended.
#' @param update_sb logical. May we take some time to update ScienceBase if the
#'   most modern version isn't up there yet?
#' @export
#' @importFrom stats setNames
#' @import dplyr
get_metab_model <- function(model_name, on_local_exists='skip', version=c('modern','original'), update_sb=TRUE) {
  version <- match.arg(version)
  mms <- lapply(setNames(model_name,model_name), function(mname) {
    
    if(version=='original') {
      file <- download_metab_model(mname, on_local_exists = on_local_exists, version='original')
      varname <- load(file)
      mm <- get(varname)
      return(mm)
    }

    file <- tryCatch({
      download_metab_model(mname, on_local_exists = on_local_exists, version='modern')
    }, error=function(e) {
      download_metab_model(mname, on_local_exists = on_local_exists, version='original')
    })
    varname <- load(file)
    mm <- get(varname)
    
    # update the SB version if it's not fully modern
    tag_modern <- packageVersion("streamMetabolizer")
    tag_mm <- package_version(strsplit(get_version(mm), split=" ")[[1]][1])
    if(tag_modern > tag_mm) {
      file <- download_metab_model(mname, on_local_exists = on_local_exists, version='original')
      varname <- load(file)
      mm <- get(varname)
      mm <- modernize_metab_model(mm)
      if(isTRUE(update_sb)) {
        tryCatch({
          mm_staged <- stage_metab_model(title=paste0("m", mname), metab_outs=mm, verbose=FALSE)
          stop("not ready to post modernized models yet, but staged to ", mm_staged)
          post_metab_model(mm_staged, on_exists="replace_file")
        }, error=function(e) warning("tried and failed to post updated file. error: ", e),
        wraning=function(w) warning("tried and failed to post updated file. warning: ", w))
      }
    }
    
    mm
  })
  if(length(mms) == 1) {
    return(mms[[1]])
  } else {
    return(mms)
  }
}