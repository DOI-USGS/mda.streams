get_project_id = function(session){
  item <- query_item_identifier(scheme = get_scheme(),type = 'project_root', key = 'uber', session = session)
  return(item$id)
}

get_presentation_id = function(session){
  item <- query_item_identifier(scheme = get_scheme(),type = 'project_presentations', key = 'uber', session = session)
  return(item$id)
}


get_publication_id = function(session){
  item <- query_item_identifier(scheme = get_scheme(),type = 'project_publications', key = 'uber', session = session)
  return(item$id)
}