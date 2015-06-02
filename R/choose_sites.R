#' Identify sites with sufficient data to run a metabolism model
#' 
#' We will need to be more flexible later about which variables are available,
#' since the file names will be variants on the core variable names (e.g.,
#' wtr.local, wtr.model, etc.)
#' 
#' @param required_vars character vector of variable names that are all required
#'   for a site to qualify
#' @export
#' @examples
#' choose_sites()
choose_sites <- function(required_vars=c("disch","doobs","wtr")) {
  required_vars <- make_ts_name(required_vars)
  var_sites <- lapply(required_vars, function(var) { 
    get_sites(with_child_key=var)
  })
  good_sites <- var_sites[[1]]
  if(length(required_vars) > 1) {
    for(i in 2:length(var_sites)) {
      good_sites <- intersect(good_sites, var_sites[[i]])
    }
  }
  good_sites
}
