test_that("Check date ranges in alldat", {
  
  rng <- alldat |> 
    pull(date) |> 
    range()
  
  chk <- rng[1] < dtrng[1] | rng[2] > dtrng[2]
  
  expect_false(chk)
  
})
