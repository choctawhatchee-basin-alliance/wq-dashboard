test_that("Test wbid in stas are as expected", {
  
  chk <- stas |> 
    pull(WBID) |> 
    unique() |> 
    setdiff(wbid)
  
  expect_true(length(chk) == 0)
  
})

test_that("Check wbid in alldat in cbawbid",{
  
  chk <- stas |> 
    pull(WBID) |> 
    unique() |> 
    setdiff(cbawbid$WBID)
  
  expect_true(length(chk) == 0)
  
})
