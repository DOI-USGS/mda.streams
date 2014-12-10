# runs on package build. Need to fix for windows...
if (file.exists('~/Documents/R/mda.streams/inst/init_data.R')){
  filePath <- system.file("inst", "int_data.R", package="mda.streams")
  source(filePath)
}
