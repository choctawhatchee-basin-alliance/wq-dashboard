test_that("Test rain values all greater than zero", {
  
  chk <- raindat |> 
    pull(precip_inches)
  
  expect_true(all(chk >= 0))
  
})

test_that("Test date range of rain data",{

  chk <- raindat |> 
    pull(date)

  expect_true(min(chk) >= as.Date('1990-01-01'))
  expect_true(max(chk) <= Sys.Date())

})
