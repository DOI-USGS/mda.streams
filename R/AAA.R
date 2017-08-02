#' Define a package environment for storing data specific to a project during an
#' R session
#' 
#' \code{scheme} - string identifying the ScienceBase scheme where this SB project
#'   resides, accessed by \code{\link{set_scheme}} and \code{\link{get_scheme}}.
#' @return the package environment
define_pkg_env <- function() {
  pkg.env <- new.env()
  pkg.env$scheme <- "mda_streams"
  pkg.env$ts_prefix <- "ts_"
  pkg.env$ts_delim <- "\t"
  pkg.env$meta_extension <- "tsv"
  pkg.env$meta_delim <- "\t"
  pkg.env$metab_model_prefix <- "mm_"
  pkg.env$metab_model_extension <- "RData"
  pkg.env$rds_compression <- "gzip"
  pkg.env$archive_prefix <- 'ARCHIVE_'
  return(pkg.env)
}
pkg.env <- define_pkg_env()

#' Check whether this package is up to date
#' 
#' @rdname onAttach
#' @param libname	a character string giving the library directory where the
#'   package defining the namespace was found.
#' @param pkgname	a character string giving the name of the package.
#' @importFrom utils available.packages contrib.url packageVersion
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap("USGS Support Package: https://owi.usgs.gov/R/packages.html#support"), collapse='\n'))
  packageStartupMessage(paste(strwrap(
    paste("Funding for", pkgname, "expires summer 2017, after which bugfixes & new features will be minimal")), collapse='\n'))
  
  GRAN_update_code <- paste0(
    '  update.packages(oldPkgs=c(\n',
    '    "mda.streams","dataRetrieval","geoknife","sbtools","smwrQW","streamMetabolizer","unitted",\n',
    '    "dplyr","foreign","stringr","lubridate","jsonlite","httr","lazyeval"),\n',
    '    dependencies=TRUE, repos=c("https://owi.usgs.gov/R", "https://cran.rstudio.com"))')
  github_owner <- 'USGS-R'
  github_branch <- 'develop'
  github_pkg_ref <- paste0(github_owner,'/',pkgname,'@',github_branch)
  github_update_code <- paste0(
    '  devtools::install_github("',github_pkg_ref,'")')
  
  tryCatch({
    GRAN_pkg <- utils::available.packages(utils::contrib.url("https://owi.usgs.gov/R"))
    GRAN_version <- package_version(GRAN_pkg[[pkgname, 'Version']])
    local_version <- utils::packageVersion(pkgname)
    if(local_version < GRAN_version) {
      packageStartupMessage(
        'Time to update to ', pkgname, ' version ', GRAN_version, '! You have ', local_version, '. Get stable updates with\n',
        GRAN_update_code)
    }
  }, error=function(e) {
    packageStartupMessage("Can't check GRAN for new package versions just now. We'll try again next time.")
  })
  
  if(requireNamespace('devtools', quietly=TRUE)) {
    tryCatch({
      github_ref <- devtools:::github_resolve_ref(
        devtools::github_release(), 
        devtools:::parse_git_repo(github_pkg_ref))$ref
      github_version <- package_version(gsub('v', '', github_ref))
      if(local_version < github_version) {
        packageStartupMessage(
          'New development version of ', pkgname, ' (', github_version, ') is ready! You have ', local_version, '. Get dev updates with\n',
          github_update_code)
      }
    }, error=function(e) {
      packageStartupMessage("Can't check GitHub for new package versions just now. We'll try again next time.")
    })
  }
}