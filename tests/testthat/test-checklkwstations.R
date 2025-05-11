test_that("Check all lakewatch waterbody, station combos from alldat in stas", {
  
  dat <- alldat |>
    filter(type == 'discrete') |>
    select(waterbody, station) |> 
    unique()
  chk <- anti_join(dat, stas, by = c('waterbody', 'station'))
  
  expect_true(nrow(chk) == 0)
  
})
