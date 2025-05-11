test_that("Check stas in appropriate bounding box", {

  chk <- st_intersects(stas, stasbbox, sparse = FALSE)
  
  expect_true(all(chk))

})