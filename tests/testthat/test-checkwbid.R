test_that("Test wbid in stas are as expected", {
  
  staswbid <- stas |> 
    pull(WBID) |> 
    unique() |> 
    sort()
  
  expect_equal(staswbid, wbid)
  
})
