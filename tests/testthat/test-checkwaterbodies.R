test_that("Test waterbodies in alldat are as expected", {
  
  alldatwb <- alldat |> 
    pull(waterbody) |> 
    unique() |> 
    sort()
  
  expect_equal(alldatwb, waterbodies)
  
})
