#@param ... Additional parameters supplied to \code{\link[sbtools]{session_check_reauth}}
get_title <- function(id, ...){
  
	session_check_reauth(...)
	
  item_json <- item_get(id)
  
  title = item_json[['title']]
  return(title)
}
