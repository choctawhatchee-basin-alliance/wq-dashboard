test_that("Test wbid in stas are as expected", {
  
  chk <- stas |> 
    pull(WBID) |> 
    unique() |> 
    na.omit() |> 
    setdiff(wbid)
  
  expect_true(length(chk) == 0)
  
})

test_that("Check wbid in alldat in cbawbid",{
  
  chk <- stas |> 
    pull(WBID)

  # should be two without WBID (not in FL)
  expect_equal(sum(is.na(chk)), 2)
  
  # should be no WBIDs in stas that aren't in cbawbid
  expect_true(setdiff(na.omit(chk), cbawbid$WBID) |> length() == 0)
  
})
