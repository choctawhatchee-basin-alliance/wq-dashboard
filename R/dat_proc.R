# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(here)

# need to do this because read only, won't work in non-interactive session
gs4_auth(scope = "https://www.googleapis.com/auth/spreadsheets.readonly")

# all files https://drive.google.com/drive/u/1/folders/1x51X6p60KOKpC3UEStIkuAWRhOH-8FHS

# station locations ---------------------------------------------------------------------------

rawdat <- read_sheet('13ob5pYoKnYMTMn-jqKFFT6e0QyrDPXmBK9QtcB0gnrw')

stas <- rawdat |> 
  mutate(
    WBID = unlist(WBID)
  ) |> 
  rename(
    Longitude = Latitude, 
    Latitude = Longitude
  ) |> 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

save(stas, file = here('data', 'stas.RData'))
