# NASIS QUERY FOR KSAT PEDON HORIZON

# FROM site
# INNER JOIN siteobs by default
# INNER JOIN pedon by default
# INNER JOIN nasisuser on useriid = phorizon.recuseriidref
# WHERE ksatpedon IS NOT NULL AND username <> "Steinert, Andy" AND username <> "Moore, Mike"

library(soilDB)
library(aqp)
library(rgdal)
library(sp)
library(dplyr)

# 1610 sites
s <- get_site_data_from_NASIS_db()

s_pnts <- s[, c('site_id', "x_std", "y_std")]

# 1572 site with std lat/long
s_pnts <- filter(s_pnts, !is.na(x_std) & !is.na(y_std))
coordinates(s_pnts) <- ~ x_std + y_std
proj4string(s_pnts) <- '+proj=longlat +datum=WGS84'
writeOGR(s_pnts, "D:/temp", "s_pnts3", driver = "ESRI Shapefile")

# NASIS QUERY FOR SUMMARY TABLE

# FROM site
# INNER JOIN siteobs by default
# INNER JOIN pedon by default
# INNER JOIN phorizon by default
# INNER JOIN phksatsummary by default
# WHERE phksatsummary.sathydcondmean  IS NOT NULL

# 96 sites
s <- get_site_data_from_NASIS_db()

s_pnts <- s[, c('site_id', "x_std", "y_std")]

# 95 with std lat/long
s_pnts <- filter(s_pnts, !is.na(x_std) & !is.na(y_std))
coordinates(s_pnts) <- ~ x_std + y_std
proj4string(s_pnts) <- '+proj=longlat +datum=WGS84'
writeOGR(s_pnts, "D:/temp", "s_pnts_sum", driver = "ESRI Shapefile")

# NASIS QUERY FOR INFILTRATION TABLE

# FROM site
# INNER JOIN siteobs by default
# INNER JOIN pedon by default
# INNER JOIN peinfiltrationsummary BY default
# WHERE infiltrationmean IS NOT NULL

# 179 sites
s <- get_site_data_from_NASIS_db()

s_pnts <- s[, c('site_id', "x_std", "y_std")]

# 172 sites with std lat/long
s_pnts <- filter(s_pnts, !is.na(x_std) & !is.na(y_std))
coordinates(s_pnts) <- ~ x_std + y_std
proj4string(s_pnts) <- '+proj=longlat +datum=WGS84'
writeOGR(s_pnts, "D:/temp", "s_pnts_inf", driver = "ESRI Shapefile")

# NASIS QUERY FIELD MEASURED PROPERTY (modified from Jason N.)

# FROM site
# INNER JOIN siteobs by default
# INNER JOIN pedon by default
# INNER JOIN phorizon by default
# INNER JOIN phfmp BY default AND CASE WHEN fmpname LIKE '%KSAT%' THEN 1
# WHEN fmpname LIKE '%Infiltration%' THEN 1
# WHEN fmpname LIKE '%infultration%' THEN 1
# WHEN fmpname LIKE '%perm%' THEN 1
# ELSE 2 END = 1

s <- get_site_data_from_NASIS_db()

s_pnts <- s[, c('site_id', "x_std", "y_std")]

s_pnts <- filter(s_pnts, !is.na(x_std) & !is.na(y_std))
coordinates(s_pnts) <- ~ x_std + y_std
proj4string(s_pnts) <- '+proj=longlat +datum=WGS84'
writeOGR(s_pnts, "D:/temp", "s_pnts_fmp", driver = "ESRI Shapefile")
