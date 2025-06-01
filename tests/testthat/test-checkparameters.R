test_that("Check all parameters in alldat are in meta", {

  chk <- setdiff(alldat$parameter, meta$parameter)
  
  expect_true(length(chk) == 0)

})
