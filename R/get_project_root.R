get_project_id = function(session){
  item <- query_item_identifier(scheme = 'mda_streams',type = 'project_root', key = 'uber', session = session)
  return(item$id)
}