test_that("Test all waterbody and stations are in alldat for nncdat", {
  
  alldatchk <- alldat |> 
    select(waterbody, station) |> 
    distinct() |> 
    unite('chk', waterbody, station, sep = '_') |> 
    pull(chk)
  
  nncdatchk <- nncdat |>
    select(waterbody, station) |> 
    distinct() |> 
    unite('chk', waterbody, station, sep = '_') |> 
    pull(chk)
  
  expect_true(all(alldatchk %in% nncdatchk), 
              info = "Not all waterbody and station combinations alldat are present in nncdat")
  
})

test_that("Check all WBID in nncdat in cbawbid WBID", {
  
  cbawbidchk <- cbawbid |> 
    select(WBID) |> 
    distinct() |> 
    pull(WBID)
  
  nncdatwbidchk <- nncdat |>
    select(WBID) |> 
    distinct() |> 
    pull(WBID)
  
  expect_true(all(nncdatwbidchk %in% cbawbidchk), 
              info = "Not all WBID in nncdat are present in cbawbid WBID")
  
})
