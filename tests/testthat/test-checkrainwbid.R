test_that("Test rain wbids in rainstas are as expected", {
  
  chk <- rainstas |> 
    pull(WBID) |> 
    unique() |> 
    na.omit() |> 
    setdiff(rnwbid)
  
  expect_true(length(chk) == 0)
  
})

test_that("Check wbid in raindat in rainwbid",{
  
  chk <- rainstas |> 
    pull(WBID)

  # should be one without WBID (not in FL)
  expect_equal(sum(is.na(chk)), 1)
  
  # should be no WBIDs in rainstas that aren't in rainwbid
  expect_true(setdiff(na.omit(chk), rainwbid$WBID) |> length() == 0)
  
})
