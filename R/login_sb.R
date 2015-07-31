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