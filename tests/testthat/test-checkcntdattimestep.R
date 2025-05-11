test_that("Check time step is ascending in cntdat", {
  
  wbst <- cntdat |> 
    select(waterbody, station) |> 
    unique()
  
  for(i in seq_along(wbst)){
    
    chk <- cntdat |> 
      filter(waterbody == wbst$waterbody[i], station == wbst$station[i]) |>
      pull(timestamp) |> 
      
      diff() |> 
      unique()
  
    expect_true(all(chk > 0))
    
  }
  
})
