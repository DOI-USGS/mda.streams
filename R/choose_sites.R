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
#' \dontrun{
#' choose_sites()
#' }
choose_sites <- function(required_vars=c("disch_nwis","doobs_nwis","wtr_nwis")) {
  # this should soon be an intersection of several calls to 
  # find_sites(c("light.nldas","light.model"), logic="any"), etc.
  list_sites(with_var_src = required_vars, logic="all")
}
