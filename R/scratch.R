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
tmp <- inner_join(lkwstas, cbastas, by = c('waterbody', 'station'))

# CBA waterbody, stations not in stations -----------------------------------------------------

cbastas <- cbadat |> 
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(cbastas, stas, by = c('waterbody', 'station'))

write.csv(tmp, '~/Desktop/CBA_waterbody_stations_not_in_stations.csv', row.names = F)

# lakewatch waterbody stations, not in stations -----------------------------------------------

lkwstas <- lkwdat |> 
  select(waterbody, station) |> 
  unique()
tmp <- anti_join(lkwstas, stas, by = c('waterbody', 'station'))

write.csv(tmp, '~/Desktop/lakewatch_waterbody_stations_not_in_stations.csv', row.names = F)

# compare -------------------------------------------------------------------------------------

waterbody <- 'CBA Destin-1'
station <- '1'

toplo1 <- lkwdat |> 
  filter(waterbody == !!waterbody, station == !!station)
toplo2 <- cbadat |> 
  filter(waterbody == !!waterbody, station == !!station)

sort(unique(toplo1$date))
sort(unique(toplo2$date))
