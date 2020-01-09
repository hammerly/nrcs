library(daff)
library(readr)

IA105_orig <- read_csv("D:/Cloud/Annum_Data/FY2019/Region 11/SSO/WAV/MLRA/spatial_data/IA105_orig.csv")

IA105_edit <- read_csv("D:/Cloud/Annum_Data/FY2019/Region 11/SSO/WAV/MLRA/spatial_data/IA105_edits.csv")

dtadiff <- diff_data(IA105_orig, IA105_edit)

render_diff(dtadiff)

library(aqp)
library(soilDB)
library(dplyr)
library(sf)

p <- fetchNASIS()

s <- site(p)

s_o <- s

ph <- horizons(p)

ph$peiid <- as.character(ph$peiid)

s_pnts <- s %>% left_join(ph, by = "peiid")

s_pnts <- s_pnts %>% left_join(pl, by = "phiid")

s <- filter(s_pnts, !is.na(x_std) & !is.na(y_std))

coordinates(s) <- ~ x_std + y_std
proj4string(s) <- '+proj=longlat +datum=WGS84'

t <- st_as_sf(s, coords = c("x", "y"), crs = '+proj=longlat +datum=WGS84')

st_write(t, "D:/Cloud/Annum_Data/FY2019/Region 11/SSO/WAV/MLRA/spatial_data/Tripoli_pedons2.shp")
