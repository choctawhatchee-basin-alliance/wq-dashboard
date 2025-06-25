# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(here)
library(janitor)
library(readxl)

# need to do this because read only, won't work in non-interactive session
gs4_auth(scope = "https://www.googleapis.com/auth/spreadsheets.readonly")

# all files https://drive.google.com/drive/u/1/folders/1x51X6p60KOKpC3UEStIkuAWRhOH-8FHS

# combine cba (physical) and lakewatch (discrete) ---------------------------------------------

# physical data - CBA
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
    dosat_surf_psat = dissolved_oxygen_percent_surface_percent_sat,
    dosat_bott_psat = dissolved_oxygen_percent_bottom_percent_sat,
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
    station = as.character(station),
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

# nutrient data - lakewatch
rawdat2 <- read_sheet('1h4yvi9AnISVFbH_AvBw7wDx7s5-4VIOdqD-VToExmvg', na = c('NA', ''))

# inactive lakewatch stations to remove
torm <- read_excel(here('data-raw', 'Lakewatch inactive_kw.xlsx'))
                 
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
    secchi_onbott = grepl('Weeds|Bottom', secchi_onbott), 
    waterbody = gsub('^CBA\\sGAP', 'CBA Gap', waterbody)
  ) |> 
  select(-month, -day, -year, -cond_uscm) |> 
  select(county, waterbody,  station,  date, everything()) |> 
  anti_join(torm, by = c('waterbody', 'station')) |> 
  filter(!(waterbody == 'Campbell' & station == '2 Deep')) # only one instance of this, not in stations

lkwdat <- dat2

# cba long format
cbadatlng <- cbadat |> 
  pivot_longer(
    cols = -c(county, waterbody, station, date), 
    names_to = 'var',
    values_to = 'val'
  ) |>
  separate(
    col = var, 
    into = c('parameter', 'location', 'units'), 
    sep = '_'
  ) |> 
  mutate(
    notes = NA_character_
  )

# lakewatch long format
secchi <- lkwdat |> 
  select(county, waterbody, station, date, val = secchi_ft, notes = secchi_onbott) |> 
  mutate(
    parameter = 'secchi',
    notes = ifelse(notes == T, 'on bottom', NA_character_)
  ) |> 
  unique()
lkwdatlng <- lkwdat |> 
  select(-secchi_onbott) |>
  pivot_longer(
    cols = -c(county, waterbody, station, date), 
    names_to = 'var',
    values_to = 'val'
  ) |>
  separate(
    col = var, 
    into = c('parameter', 'units'), 
    sep = '_'
  ) |> 
  mutate(
    location = 'surf'
  ) |> 
  left_join(
    secchi, 
    by = c('county', 'waterbody', 'station', 'date', 'parameter', 'val')
  )

alldat <- list(
    physical = cbadatlng,
    discrete = lkwdatlng
  ) |> 
  enframe(name = 'type') |> 
  unnest('value') |> 
  filter(!is.na(val))

save(alldat, file = here('data', 'alldat.RData'))

# station locations and wbid ------------------------------------------------------------------

rawdat <- read_sheet('13ob5pYoKnYMTMn-jqKFFT6e0QyrDPXmBK9QtcB0gnrw')

# this has wbid but it's from an older wbid layer, not all are in the current fdep layer
stas <- rawdat |> 
  rename(
    Longitude = Latitude, 
    Latitude = Longitude, 
    waterbody = `CBA Waterbody Name`, 
    station = `CBA Station #`,
    name = `Monitoring Location Name`,
    WBIDorig = WBID
  ) |> 
  mutate(
    station = as.character(station), 
    WBIDorig = unlist(WBIDorig)
  ) |> 
  select(-`GPS abbr.`) |> 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

allwbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson') |> 
  st_make_valid() |> 
  select(WBID)
cbawbid <- allwbid[stas,] 

save(cbawbid, file = here('data', 'cbawbid.RData'))

# add updated wbid
# those in more than one wbid, use original
stas <- st_join(stas, cbawbid, join = st_intersects, left = TRUE) |> 
  mutate(isdup = n() > 1, .by = c(waterbody, station)) |> 
  filter(!(isdup & WBID != WBIDorig)) |> 
  select(-isdup, -WBIDorig)

# add date range to stations
load(file = here('data/alldat.RData'))
dts <- alldat |> 
  select(waterbody, station, date) |> 
  summarise(
    datestr = min(date), 
    dateend = max(date), 
    .by = c(waterbody, station)
  )
stas <- left_join(stas, dts, by = c('waterbody', 'station'))

# get huc12
# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/State/Shape/
hucall <- st_read('~/Desktop/NHD_H_Florida_State_Shape/Shape/WBDHU12.shp')
cbahuc <- hucall |> 
  st_transform(crs = st_crs(stas))
cbahuc <- cbahuc[stas,] |> 
  dplyr::select(huc12)

save(cbahuc, file = here('data', 'cbahuc.RData'))

# add huc12 to stas
stas <- st_join(stas, cbahuc, join = st_intersects, left = TRUE) |> 
  mutate(huc12 = ifelse(is.na(huc12), NA, huc12))

save(stas, file = here('data', 'stas.RData'))

# metadata file -------------------------------------------------------------------------------

data("alldat")

meta <- alldat |> 
  select(type, units, location, parameter, date, val) |> 
  filter(!is.na(val)) |> 
  mutate(
    datestr = min(date, na.rm = T), 
    dateend = max(date, na.rm = T),
    .by = c(type, units, location, parameter)
  ) |> 
  select(-date, -val) |>
  unique() |> 
  mutate(
    label = case_when(
      parameter == 'temp' ~ 'Temperature (F)',
      parameter == 'dosat' ~ 'Dissolved Oxygen (% Sat)',
      parameter == 'do' & units == 'mgl' ~ 'Dissolved Oxygen (mg/L)',
      parameter == 'cond' ~ 'Conductivity (mS/cm)',
      parameter == 'sal' ~ 'Salinity (ppt)',
      parameter == 'ph' ~ 'pH (su)',
      parameter == 'turb' ~ 'Turbidity (NTU)',
      parameter == 'secchi' ~ 'Secchi Depth (ft)',
      parameter == 'chlcorr' ~ 'Chl-a (ug/L, corrected)',
      parameter == 'chluncorr' ~ 'Chl-a (ug/L, uncorrected)',
      parameter == 'tp' ~ 'Total Phosphorus (mg/L)',
      parameter == 'tn' ~ 'Total Nitrogen (mg/L)',
      parameter == 'color' ~ 'Color (pt-co)',
      parameter == 'depth' ~ 'Depth (ft)'
    )
  )
  
save(meta, file = here('data', 'meta.RData'))

# nnc references ------------------------------------------------------------------------------

nncraw <- read_sheet('1VQWzS-GMOzc-Xs0XoTa8Ifz8QyG_6WhU8THJPjRqWBs', 
                     col_types = 'cccccddd')  

nncdat <- nncraw |> 
  select(
    waterbody = `CBA Waterbody Name`, 
    station = `CBA Station #`, 
    WBID, 
    tp = `Total Phosphorus (mg/L)`,
    tn = `Total Nitrogen (mg/L)`,
    chl = `Chlorophyll a (μg/L)` # needs to apply to both corr and uncorr
  ) |> 
  pivot_longer(
    cols = c(tp, tn, chl), 
    names_to = 'parameter', 
    values_to = 'value'
  ) |> 
  filter(!is.na(value))

# correct those in differing WBIDs based on manual check (959F and 46 not in cbawbid)
nncdat <- nncdat |> 
  mutate(
    WBID = case_when(
      WBID == '959F' ~ '959C', # Oyster 4, 959F and 959C has same NNC for TP, TN, Chl-a
      WBID == '46' ~ '49F', # CBA Pea River-1 1, CBA River-9 1, 46 and 49F has same NNC for TP, TN
      T ~ WBID
    )
  )

save(nncdat, file = here('data', 'nncdat.RData'))
  
# create dummy file for continuous data -------------------------------------------------------

# two years of data
strdate <- "2023-01-01"
enddate <- "2024-12-31"

# create data
cntdat <- crossing(
    waterbody = c('Little Red Fish', 'Western'),
    station = c('1', '2'), 
    type = c('temp_f', 'sal_ppt'), 
  ) |> 
  group_nest(type) |> 
  mutate(
    amp = c(0, 10), 
    base_value = c(20, 80),
    noise_sd = c(3, 5)
  ) |> 
  unnest('data') |> 
  rowwise() |> 
  mutate(
    value = pmap(list(amp, base_value, noise_sd), ~cntdat_fun(
      start_date = strdate, 
      end_date = enddate,
      base_value = base_value,
      amplitude = amp,
      noise_sd = noise_sd
    ))
  ) |> 
  ungroup() |> 
  select(waterbody, station, type, value) |> 
  unnest(value) |>
  pivot_wider(
    names_from = type,
    values_from = value
  ) |> 
  mutate(
    sal_ppt = pmax(sal_ppt, 0)
  )

save(cntdat, file = here('data', 'cntdat.RData'))
