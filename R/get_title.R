get_title <- function(id, session = NULL){
  
  item_json <- item_get(id, session = session)
  
  title = item_json[['title']]
  return(title)
}
