library(sf)

# data
load(file = here::here('data/alldat.RData'))
load(file = here::here('data/cntdat.RData'))
load(file = here::here('data/meta.RData'))
load(file = here::here('data/stas.RData'))
load(file = here::here('data/cbawbid.RData'))

# selections
prms <- meta |> 
  dplyr::select(parameter, label) |> 
  dplyr::distinct() |> 
  dplyr::arrange(parameter)
prms <- setNames(prms$parameter, prms$label)
dtrng <- range(alldat$date)

# value boxes
nsmp <- format(nrow(alldat), big.mark = ",", scientific = FALSE)
nprm <- length(unique(alldat$parameter))
nsta <- nrow(stas)
nwbd <- length(unique(alldat$waterbody))
dtrg <- paste(lubridate::year(range(alldat$date)), collapse = ' to ')
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
