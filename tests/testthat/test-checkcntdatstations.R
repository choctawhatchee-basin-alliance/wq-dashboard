test_that("Check all continuous data waterbody, station combos from alldat in stas", {
  
  dat <- cntdat |>
    select(waterbody, station) |> 
    unique()
  chk <- anti_join(dat, stas, by = c('waterbody', 'station'))
  
  expect_true(nrow(chk) == 0)
  
})
