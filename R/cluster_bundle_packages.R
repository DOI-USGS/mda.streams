#' Build a zip file of bundled packages
#' 
#' To use this file, unpack it. It contains a top-level folder called packages.
#' To install a package from this bundle, run
#' \code{install.packages('mypackagename', repos='file:packages', type='source')}
#' 
#' @param packages character. the package or packages whose dependencies (and 
#'   the packages themselves) should be obtained
#' @param bundlefile file.path to save the bundled packages
#' @param pkgdir directory path where the downloaded and written packages should
#'   be saved
#' @param repos character vector of repositories with info on the package 
#'   dependencies
#' @importFrom utils available.packages
#' @importFrom tools package_dependencies write_PACKAGES
#' @export
cluster_bundle_packages <- function(packages='mda.streams', github='USGS-R/mda.streams@develop', bundlefile='./cluster_packages.zip', pkgdir='temp/pkgs', repos=union(getOption('repos'),'http://owi.usgs.gov/R')) {
  # get the database of pkgs in CRAN and GRAN. db should include current or 
  # recent versions of all packages of interest. if we run into the case where 
  # we've just added a package to a GitHub repo, haven't pushed the repo to 
  # GRAN, and still want this function to discover its new dependencies, we'll 
  # need to add/revise rows in db to reflect that change
  db <- available.packages(type='source', repos=repos)
  
  # get the complete vector of package & dependency names to download
  deps <- sort(unique(unlist(lapply(packages, function(package) {
    c(package, tools::package_dependencies(packages=package, db=db, recursive=TRUE)[[1]])
  }))))
  if(!all(pkg_in_db <- packages %in% deps)) 
    warning("packages not in available.packages(): ", paste(packages[!pkg_in_db], collapse=', '), ". dependencies for these are ignored")
  basepkgs <- c('base','compiler','datasets','graphics','grDevices','grid','methods','parallel','splines','stats','stats4','tcltk')
  deps <- setdiff(deps, basepkgs) # exclude base packages
  
  # partition deps into those to install from github versus those to install
  # from CRAN/GRAN
  ghdeps <- sapply(github, function(ghr) devtools:::github_remote(ghr)$repo)
  cgdeps <- setdiff(deps, ghdeps)
                      
  # prepare src/contrib directory where we'll download package files and create
  # PACKAGES and PACKAGES.gz
  srccontribdir <- file.path(pkgdir, 'packages/src/contrib')
  if(!dir.exists(srccontribdir)) dir.create(srccontribdir, recursive=TRUE)
  
  # download the CRAN/GRAN packages and write the PACKAGES files
  download.packages(cgdeps, destdir=srccontribdir, type='source', repos=repos)
  tools::write_PACKAGES(dir=srccontribdir, type='source', unpacked=FALSE)
  PACKAGES_cgran <- readLines(file.path(srccontribdir, 'PACKAGES'))
  PACKAGES.gz_cgran <- readLines(file.path(srccontribdir, 'PACKAGES.gz'))
  
  # download the GitHub packages and write the PACKAGES files
  sapply(github, function(ghpkg) {
    pkgzip <- devtools:::remote_download(devtools:::github_remote(ghpkg))
    unzip(pkgzip, exdir=srccontribdir)
  })
  tools::write_PACKAGES(dir=srccontribdir, type='source', unpacked=TRUE, addFiles=TRUE)
  PACKAGES_gh <- readLines(file.path(srccontribdir, 'PACKAGES'))
  PACKAGES.gz_gh <- readLines(file.path(srccontribdir, 'PACKAGES.gz'))
  
  # write unified PACKAGES and PACKAGES.gz files
  out <- file(file.path(srccontribdir, "PACKAGES"), "wt")
  cat(c(PACKAGES_cgran, PACKAGES_gh), sep='\n', file=out)
  close(out)
  outgz <- gzfile(file.path(srccontribdir, "PACKAGES.gz"), "wt")
  cat(c(PACKAGES.gz_cgran, PACKAGES.gz_gh), sep='\n', file=outgz)
  close(outgz)
  
  # zip up the archive[s] into the bundlefile
  absbundlefile <- normalizePath(bundlefile, mustWork=FALSE)
  oldwd <- getwd()
  setwd(pkgdir)
  zip(absbundlefile, 'packages')
  setwd(oldwd)
}
