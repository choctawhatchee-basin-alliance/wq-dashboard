test_that("Test wbid in stas are as expected", {
  
  staswbid <- stas |> 
    pull(WBID) |> 
    unique() |> 
    sort()
  
  expect_equal(staswbid, wbid)
  
})

test_that("Check wbid in alldat in cbawbid",{
  
  chk <- stas |> 
    pull(WBID) |> 
    unique() |> 
    setdiff(cbawbid$WBID)
  
  expect_true(length(chk) == 0)
  
})
