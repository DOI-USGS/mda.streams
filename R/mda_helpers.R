make_ts_variable <- function(variable){
  ts_prefix <- get_ts_prefix()
  ts_variable = paste0(ts_prefix, variable)
  return(ts_variable)
}

get_ts_prefix <- function(){
  ts_prefix = 'ts_'
  return(ts_prefix)
}

get_ts_extension <- function(){
  ts_extension = 'tsv'
  return(ts_extension)
}

get_ts_delim <- function(){
  return('\t')
}