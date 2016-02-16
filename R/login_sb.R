#' Log into ScienceBase with your myUSGS credentials
#' 
#' BE CAREFUL NOT TO POST OR SHARE YOUR PASSWORD! Use the same username and 
#' password you set up via the ScienceBase interface or your government 
#' employer. This function is equivalent to 
#' \code{sbtools::\link[sbtools]{authenticate_sb}} but easier to type.
#' 
#' @param username Your ScienceBase/myUSGS username, usually an email address
#' @import sbtools
#' @export
login_sb <- function(username) {
  if(missing(username)) authenticate_sb() else authenticate_sb(username)
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