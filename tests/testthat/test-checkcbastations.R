test_that("Check all CBA waterbody, station combos from alldat in stas", {
  
  dat <- alldat |>
    filter(type == 'physical') |>
    select(waterbody, station) |> 
    unique()
  chk <- anti_join(dat, stas, by = c('waterbody', 'station'))
  
  expect_true(nrow(chk) == 0)
  
})