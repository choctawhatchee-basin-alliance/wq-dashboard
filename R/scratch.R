library(sf)
library(tidyverse)

data("lkwdat")
data("cbadat")
data("stas")

# lakewatch waterbody, stations not in CBA waterbody, stations --------------------------------

cbastas <- cbadat |> 
  select(waterbody, station) |> 
  unique()
lkwstas <- lkwdat |>
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(lkwstas, cbastas, by = c('waterbody', 'station'))

# CBA waterbody, stations not in stations -----------------------------------------------------

cbastas <- cbadat |> 
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(cbastas, stas, by = c('waterbody', 'station'))