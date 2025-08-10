test_that("Test rain stations are in rain data", {
  
  chk <- raindat |> 
    pull(station) |> 
    unique() |> 
    setdiff(rainstas$station)
  
  expect_true(length(chk) == 0)
  
})