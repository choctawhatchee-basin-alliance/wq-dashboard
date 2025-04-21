# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(here)
library(janitor)

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
    Latitude = Longitude, 
    waterbody = `CBA Waterbody Name`, 
    station = `CBA Station #`,
    name = `Monitoring Location Name`
  ) |> 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

save(stas, file = here('data', 'stas.RData'))

# discrete samples ----------------------------------------------------------------------------

# physical data
rawdat1 <- read_sheet('16_B7XLMDDgL-4RDz4UaFE4Gk569tYi2xaf1f96mAauY', na = c('.'))

dat1 <- rawdat1 |> 
  clean_names() |> 
  rename(
    waterbody = water_body,
    station = station_number, 
    date = date_month_day_year, 
    time = time_24hr,
    temp_surf_f = temperature_surface_f,
    temp_bott_f = temperature_bottom_f,
    do_surf_psat = dissolved_oxygen_percent_surface_percent_sat,
    do_bott_psat = dissolved_oxygen_percent_bottom_percent_sat,
    do_surf_mgl = dissolved_oxygen_surface_mg_l,
    do_bott_mgl = dissolved_oxygen_bottom_mg_l,
    cond_surf_mscm = specific_conductivity_m_s_cm_surface, 
    cond_bott_mscm = specific_conductivity_m_s_cm_bottom,
    sal_surf_ppt = salinity_surface_ppt,
    sal_bott_ppt = salinity_bottom_ppt,
    ph_surf_su = p_h_surface,
    ph_bott_su = p_h_bottom,
    turb_surf_ntu = turbidity_surface_ntu,
    turb_bott_ntu = turbidity_bottom_ntu,
    depth_surf_ft = depth_surface_ft,
    depth_bott_ft = depth_bottom_ft
  ) |> 
  mutate(
    date = as.Date(date),
    county = case_when(
      county == 'okaloosa' ~ 'Okaloosa',
      county == 'walton' ~ 'Walton',
      T ~ county
    ), 
    waterbody = gsub('creek', 'Creek', waterbody),
    waterbody = gsub('beach', 'Beach', waterbody),
    waterbody = gsub('FT\\.', 'Ft.', waterbody),
    waterbody = gsub('GAP', 'Gap', waterbody), 
    waterbody = gsub('walton', 'Walton', waterbody), 
    waterbody = gsub('Tky', 'TKY', waterbody),
    waterbody = gsub('Redfish', 'Red Fish', waterbody), 
    waterbody = gsub('Cba', 'CBA', waterbody), 
    waterbody = gsub('^Bass\\sLake$', 'Bass', waterbody),
  ) |> 
  select(-month, -day, -year, -time)

cbadat <- dat1

save(cbadat, file = here('data', 'cbadat.RData'))

# nutrient data
rawdat2 <- read_sheet('1h4yvi9AnISVFbH_AvBw7wDx7s5-4VIOdqD-VToExmvg', na = c('NA', ''))

dat2 <- rawdat2 |> 
  clean_names() |> 
  rename(
    waterbody = lake, 
    tp_mgl = tp_mg_l, # us labelled as mg with clean_names, to convert
    tn_mgl = tn_mg_l, # us labelled as mg with clean_names, to convert
    chluncorr_ugl = chl_mg_l_uncorrected, # us labelled as mg with clean_names, no convert
    chlcorr_ugl = chl_mg_l_corrected, # us labelled as mg with clean_names, no convert
    secchi_onbott = secchi_2,
    color_ptco = color_pt_co_units,
    cond_uscm = cond_m_s_cm, # us
    cond_mscm = cond_m_s_cm_2 # ms
  ) |> 
  mutate(
    date = as.Date(date),
    station = unlist(station), 
    tp_mgl = tp_mgl / 1000,
    tn_mgl = tn_mgl / 1000, 
    cond_mscm = case_when(
      is.na(cond_mscm) & !is.na(cond_uscm) ~ cond_uscm / 1000, 
      T ~ cond_mscm
    ), 
    secchi_ft = case_when(
      is.na(secchi_ft) & !is.na(secchi_onbott) ~ as.numeric(gsub('^.*\\(|\\).*$|Weeds|Bottom', '', secchi_onbott)), 
      T ~ secchi_ft
    ), 
    secchi_onbott = grepl('Weeds|Bottom', secchi_onbott)
  ) |> 
  select(-month, -day, -year, -cond_uscm) |> 
  select(county, waterbody,  station,  date, everything())

lkwdat <- dat2

save(lkwdat, file = here('data', 'lkwdat.RData'))
