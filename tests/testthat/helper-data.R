library(here)
library(dplyr)
library(tidyr)
library(sf)

load(file = here('app/data/alldat.RData'))
load(file = here('app/data/stas.RData'))
load(file = here('app/data/cntdat.RData'))
load(file = here('app/data/meta.RData'))
load(file = here('app/data/cbawbid.RData'))
load(file = here('app/data/nncdat.RData'))

dtrng <- c(as.Date('1992-11-08'), Sys.Date())
stasbbox <- c(xmin = -86.6675, ymin = 30.2815, xmax = -85.68101, ymax = 31.10925) |> 
  st_bbox() |> 
  st_set_crs(4326) |> 
  st_as_sfc() |> 
  st_as_sf(crs = 4326) |> 
  st_buffer(dist = 0.01)
counties <- c("Bay", "Holmes", "Okaloosa", "Walton", "Washington")
waterbodies <- c("Allen", "Alligator", "Bass", "Big Red Fish", "Camp Creek", 
                 "Campbell", "CBA Destin-1", "CBA Destin-2", "CBA Destin-3", "CBA Destin-4", 
                 "CBA Destin-5", "CBA Destin-6", "CBA Destin-7", "CBA Destin-8", 
                 "CBA Freeport-1", "CBA Freeport-10", "CBA Freeport-2", "CBA Freeport-3", 
                 "CBA Freeport-4", "CBA Freeport-5", "CBA Freeport-6", "CBA Freeport-7", 
                 "CBA Freeport-8", "CBA Freeport-9", "CBA Ft. Walton Beach-1", 
                 "CBA Ft. Walton Beach-10", "CBA Ft. Walton Beach-16", "CBA Ft. Walton Beach-19", 
                 "CBA Ft. Walton Beach-2", "CBA Ft. Walton Beach-3", "CBA Ft. Walton Beach-4", 
                 "CBA Ft. Walton Beach-5", "CBA Ft. Walton Beach-6", "CBA Ft. Walton Beach-7", 
                 "CBA Ft. Walton Beach-8", "CBA Ft. Walton Beach-9", "CBA Gap-1", 
                 "CBA Gap-2", "CBA Gap-3", "CBA Holmes Creek-1", "CBA Holmes Creek-2", 
                 "CBA Holmes Creek-3", "CBA Mullet Creek-1", "CBA Mullet Creek-2", 
                 "CBA Niceville-1", "CBA Niceville-10", "CBA Niceville-2", "CBA Niceville-3", 
                 "CBA Niceville-4", "CBA Niceville-5", "CBA Niceville-6", "CBA Niceville-7", 
                 "CBA Niceville-8", "CBA Niceville-9", "CBA Pea River-1", "CBA River-3", 
                 "CBA River-4", "CBA River-5", "CBA River-7", "CBA River-8", "CBA River-9", 
                 "CBA Santa Rosa Beach-1", "CBA Santa Rosa Beach-10", "CBA Santa Rosa Beach-2", 
                 "CBA Santa Rosa Beach-3", "CBA Santa Rosa Beach-4", "CBA Santa Rosa Beach-5", 
                 "CBA Santa Rosa Beach-6", "CBA Santa Rosa Beach-7", "CBA Santa Rosa Beach-8", 
                 "CBA Santa Rosa Beach-9", "CBA Santa Rosa Sound-1", "CBA Santa Rosa Sound-2", 
                 "CBA Santa Rosa Sound-3", "CBA TKY-1", "CBA TKY-2", "CBA TKY-3", 
                 "Deer", "Draper", "Eastern", "Eastern North", "Fuller", "Grayton", 
                 "Little Red Fish", "Morris", "Oyster", "Powell", "Roberts", "Stallworth", 
                 "Swift Creek-1", "Swift Creek-2", "Swift Creek-3", "Swift Creek-4", 
                 "Tresca", "Western", "Western Northeast")
wbid <- c("959H", "959E", "786A", "959I", "1027A", "959A", "906D", "917", 
          "778B", "778C", "731", "778D", "789", "843", "917A", "778A", 
          "786", "915", "843A", "59D", "49C", "712", "722", "692",
          "49B", "679", "570", "49", "49F", "881A", "742", "957", "944", 
          "937", "495A", "1040A", "959D", "1037", "959G", "1009A", "959J", 
          "959", "959C", "1055A", "568A", "959B", "568", "1040D")
