library(sf)
library(markdown)
library(shinyWidgets)

# data
load(file = 'data/alldat.RData')
load(file = 'data/cntdat.RData')
load(file = 'data/meta.RData')
load(file = 'data/stas.RData')
load(file = 'data/nncdat.RData')
load(file = 'data/cbawbid.RData')
load(file = 'data/cntdat.RData')

# selections
prmsdf <- meta |> 
  dplyr::select(parameter, labelnouni) |> 
  dplyr::distinct() |> 
  dplyr::arrange(parameter)
prms <- setNames(prmsdf$parameter, prmsdf$labelnouni)
cntprms <- cntdat |> 
  dplyr::select(-waterbody, -station, -timestamp) |> 
  dplyr::rename_with(~ gsub('\\_.*$', '', .x)) |> 
  names()
cntprms <- meta |> 
  dplyr::filter(parameter %in% cntprms) |> 
  dplyr::select(parameter, labelnouni) |> 
  dplyr::distinct() |> 
  dplyr::arrange(parameter)
cntprms <- setNames(cntprms$parameter, cntprms$labelnouni)
dtrng <- c(min(meta$datestr), max(meta$dateend))
locs <- list('Surface' = 'surf', 'Bottom' = 'bott')
dtchc <- datechoice_fun(alldat)
wtbds <- sort(unique(alldat$waterbody))
stationprmsel <- stationprmsel_fun(dtrng)

# value boxes
nsmp <- format(nrow(alldat), big.mark = ",", scientific = FALSE)
nprm <- length(unique(alldat$parameter))
nsta <- nrow(stas)
nwbd <- length(unique(alldat$waterbody))
dtrg <- paste(lubridate::year(dtrng), collapse = ' to ')
valbx <- list(
  bslib::value_box(
    title = "Number of samples", value = nsmp,
    theme = 'primary', showcase = bsicons::bs_icon("graph-up-arrow"),
    showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
    height = NULL
  ),
  bslib::value_box(
    title = "Number of parameters", value = nprm,
    theme = 'primary', showcase = bsicons::bs_icon("file-spreadsheet"),
    showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
    height = NULL
  ),
  bslib::value_box(
    title = "Number of stations", value = nsta,
    theme = 'primary', showcase = bsicons::bs_icon("geo-alt"),
    showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
    height = NULL
  ),
  bslib::value_box(
    title = "Number of waterbodies", value = nwbd,
    theme = 'primary', showcase = bsicons::bs_icon('water'), 
    showcase_layout = 'left center', full_screen = FALSE, fill = TRUE, 
    height = NULL
  ),
  bslib::value_box(
    title = "Date range", value = dtrg,
    theme = 'primary', showcase = bsicons::bs_icon("calendar2"),
    showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
    height = NULL
  )
)
