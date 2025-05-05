library(sf)
library(tidyverse)

data("alldat")
data("stas")

# CBA waterbody, stations not in stations -----------------------------------------------------

cbastas <- alldat |>
  filter(type == 'physical') |>
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(cbastas, stas, by = c('waterbody', 'station'))

write.csv(tmp, '~/Desktop/CBA_waterbody_stations_not_in_stations.csv', row.names = F)

# lakewatch waterbody stations, not in stations -----------------------------------------------

lkwstas <- alldat |> 
  filter(type == 'discrete') |>
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(lkwstas, stas, by = c('waterbody', 'station'))

write.csv(tmp, '~/Desktop/lakewatch_waterbody_stations_not_in_stations.csv', row.names = F)

