context("get_var_codes")

library(dplyr)

test_that("get_var_codes filters, selects, and names as requested", {
  
  # get_var_codes() with minimal arguments
  expect_equal(get_var_codes()$var %>% unique(), var_codes$var)
  expect_equal(get_var_codes(out='units') %>% unique(), var_codes$units %>% unique())
  
  # filter by var and/or type
  expect_equivalent(get_var_codes('wtr', out=names(var_codes)), var_codes[var_codes$var=='wtr',])
  expect_error(get_var_codes(c('disch','par'), out=names(var_codes), type=c("watershed")), "not found")
  
  # drop/keep names
  expect_equal(get_var_codes('doobs', 'p_code'), '00300')
  expect_equal(get_var_codes('doobs', 'p_code', use_names=TRUE), c(doobs_nwis='00300'))
  expect_equal(names(get_var_codes(out='src')), get_var_codes()$var_src)
  expect_equal(names(get_var_codes(out='src', use_names=FALSE)), NULL)
  expect_equal(rownames(get_var_codes(out=c("var_src","p_code")))[1:2], c("1","2"))
  expect_equal(rownames(get_var_codes(out=c("var_src","p_code"), use_names=TRUE)), get_var_codes(out="var_src"))
  
})