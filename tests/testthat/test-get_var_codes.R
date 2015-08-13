context("get_var_src_codes")

library(dplyr)

test_that("get_var_src_codes filters and selects as requested", {
  
  # there used to be a data object named var_src_codes, but now we always,
  # always have to get it with the function
  var_src_codes <- get_var_src_codes()
  
  # get_var_src_codes() with minimal arguments
  expect_equal(get_var_src_codes()$var %>% unique(), unique(var_src_codes$var))
  expect_equal(get_var_src_codes(out='units') %>% unique(), var_src_codes$units %>% unique())
  
  # filter by var and/or type
  expect_equivalent(get_var_src_codes(var=='wtr', out=names(var_src_codes)), var_src_codes[var_src_codes$var=='wtr',])
  expect_equal(0, nrow(get_var_src_codes(var%in%c('disch','par'), out=names(var_src_codes), data_type==c("watershed"))))

  # select and filter
  expect_equivalent(get_var_src_codes(data_type=="ts", metab_var=="DO.obs", out=c("p_code","var")), 
                    var_src_codes[var_src_codes$data_type=="ts" & var_src_codes$var=="doobs", c("p_code","var")])
    
})