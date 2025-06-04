test_that("Check sample location in alldat", {
  
  chk <- alldat |> 
    pull(location) |> 
    table()
  
  expect_true(all(names(chk) %in% c('bott', 'surf')))
  expect_true(nrow(alldat) == sum(chk))
  
})
