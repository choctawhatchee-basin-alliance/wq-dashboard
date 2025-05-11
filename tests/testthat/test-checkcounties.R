test_that("Test counties in alldat are as expected", {
  
  alldatco <- alldat |> 
    pull(county) |> 
    unique() |> 
    sort()
  
  expect_equal(alldatco, counties)
    
})
