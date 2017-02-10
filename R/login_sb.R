#' Log into ScienceBase with your myUSGS credentials
#' 
#' BE CAREFUL NOT TO POST OR SHARE YOUR PASSWORD! Even better, set up your 
#' ~/.R/stream_metab.yaml with fields for sb_user and sb_password so you never 
#' again have to bring your password into the R global environment. If that file
#' exists and you call \code{login_sb()} with no arguments, the file contents 
#' will be used to log you in.
#' 
#' Use the same username and password you set up via the ScienceBase interface 
#' or your government employer. This function is equivalent to 
#' \code{sbtools::\link[sbtools]{authenticate_sb}} but easier to type.
#' 
#' @param username Your ScienceBase/myUSGS username, usually an email address
#' @param filename The file path to a yaml file where fields for sb_user and
#'   sb_password can be found. This file will be ignored if \code{username} is
#'   given.
#' @import sbtools
#' @export
login_sb <- function(username, filename='~/.R/stream_metab.yaml') {
  # if username is provided, assume the user wants to log in manually
  if(!missing(username)) {
    return(authenticate_sb(username))
  }
  
  # next possibility is that yaml pkg is available and a profile exists. putting
  # the yaml in ~/.R is preferred. putting the yaml in the current working
  # directory is also an option (useful for cluster runs) but not widely
  # advertised because it increases the risk of accidentally committing a
  # password file to a versioning system
  if(file.exists(filename)) {
    if(!requireNamespace('yaml', quietly=TRUE)) stop("need the yaml package to log in by file")
    profile <- yaml::yaml.load_file(filename)
    if(exists('sb_user', profile) && exists('sb_password', profile)) {    
      return(authenticate_sb(profile$sb_user, profile$sb_password))
    }
  }
  
  # last resort is to log in manually w/ both username and password
  return(authenticate_sb())
}

#' Throw error or warning if SB login is inactive
#' 
#' Default is an error
#' 
#' @param on_nonauth character giving the action to take (stop or warn) if the 
#'   session is not currently authorized
#' @import sbtools
#' @keywords internal
sb_require_login <- function(on_nonauth=c("stop","warn","continue","login"), verbose=FALSE) {
  isauth <- is_logged_in()
  if(!isauth) {
    switch(
      match.arg(on_nonauth),
      "stop"=stop("need ScienceBase authentication; call login_sb() first"),
      "warn"=warning("may need ScienceBase authentication; call login_sb() first if needed"),
      "continue"=if(isTRUE(verbose)) message("not authenticated on ScienceBase\n"),
      "login"={
        if(isTRUE(verbose)) message("need ScienceBase authentication")
        login_sb()
        isauth <- TRUE}
    ) 
  } else {
    if(isTRUE(verbose)) message("confirmed: authenticated on ScienceBase\n")
  }
  invisible(isauth)
}

#' If needed, renew SB login
#' 
#' Uses username and password to log in if session is expired or was never
#' authorized. Otherwise, pings SB to keep the session fresh.
#' 
#' @param username Your ScienceBase/myUSGS username, usually an email address
#' @param password Your ScienceBase/myUSGS password
#' @import sbtools
#' @keywords internal
sb_renew_login <- function(username, password, verbose=FALSE) {
  if(isTRUE(verbose)) {
    if(!is_logged_in()) { 
      message("re-authenticating with ScienceBase\n")
    } else {
      message("renewing ScienceBase login\n")
    }
  }
  session_renew(password, username=username)
  invisible(TRUE) # to be parallel with sb_require_login
}