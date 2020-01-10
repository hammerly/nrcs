ustermap <- function (data, area) {
  
  library(ggplot2)
  library(sf)
  library(readr)
  library(dplyr)
  library(soilDB)
  library(aqp)
  
  us <- st_read(area, stringsAsFactors = FALSE)
  us <- st_transform(us, 4326)
  aevi <- read_csv(data, col_types = cols(lat = col_double(), long = col_double()))
  
  t <- na.omit(aevi)
  
  tf <- t
  
  coordinates(tf) <- ~ long + lat
  
  proj4string(tf) <- '+proj=longlat +datum=WGS84'
  
  ts <- st_as_sf(tf)
  
  ts <- ts %>% st_join(us)
  
  ts <- ts %>% filter(!is.na(STATE))
  
  ust <- us
  
  ust$geometry <- NULL
  
  conus <- us
  
  conus <- conus %>% mutate(fipsno = as.numeric(FIPS))
  
  conus <- conus %>% filter(fipsno < 57)
  
  conus <- conus %>% filter(NAME != "Alaska" & NAME != "Hawaii")
  
  pconus <- ts %>% filter(NAME != "Alaska" & NAME != "Hawaii")
  
  ak <- us %>% filter(NAME == "Alaska")
  pak <- ts %>% filter(NAME == "Alaska")
  
  hi <- us %>% filter(NAME == "Hawaii")
  phi <- ts %>% filter(NAME == "Hawaii")
  
  gu <- us %>% filter(NAME == "Guam")
  pgu <- ts %>% filter(NAME == "Guam")
  
  as <- us %>% filter(NAME == "American Samoa")
  pas <- ts %>% filter(NAME == "American Samoa")
  
  mp <- us %>% filter(NAME == "Northern Marianas")
  pmp <- ts %>% filter(NAME == "Northern Marianas")
  
  asc <- st_cast(as, "POLYGON")
  
  asc1 <- asc[1:17,]
  
  asc2 <- asc[18:23,]
  
  mmp <- ggplot() +
    geom_sf(data = mp, fill = "#e5f5f9") +
    geom_sf(data = pmp, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)   
  
  mas1 <- ggplot() +
    geom_sf(data = asc1, fill = "#e5f5f9") +
    geom_sf(data = pas, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)
  
  mas2 <- ggplot() +
    geom_sf(data = asc2, fill = "#e5f5f9") +
    geom_sf(data = pas, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)
  
  pr <- us %>% filter(NAME == "Puerto Rico" | NAME == "Virgin Islands")
  ppr <- ts %>% filter(NAME == "Puerto Rico" | NAME == "Virgin Islands")
  
  mconus <- ggplot() +
    geom_sf(data = conus, fill = "#e5f5f9") +
    geom_sf(data = pconus, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(2163), xlim = c(-2750000, 2750000), ylim = c(-2500000, 750000))
  
  mak <- ggplot() +
    geom_sf(data = ak, fill = "#e5f5f9") +
    geom_sf(data = pak, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(3467), datum = NA, expand = FALSE)  
  
  makmhi <- ggplot() +
    geom_sf(data = hi, fill = "#e5f5f9") +
    geom_sf(data = phi, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)
  
  mgu <- ggplot() +
    geom_sf(data = gu, fill = "#e5f5f9") +
    geom_sf(data = pgu, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)
  
  mas <- ggplot() +
    geom_sf(data = as, fill = "#e5f5f9") +
    geom_sf(data = ts, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(4135), datum = NA)
  
  mpr <- ggplot() +
    geom_sf(data = pr, fill = "#e5f5f9") +
    geom_sf(data = ppr, size = .1, alpha = .1, color = "#2ca25f") +
    coord_sf(crs = st_crs(6307), datum = NA)
  
  mconus + annotation_custom(
    grob = ggplotGrob(mak),
    xmin = -2950000,
    xmax = -1000000,
    ymin = -2750000,
    ymax = -1300000
  ) +
    annotation_custom(
      grob = ggplotGrob(makmhi),
      xmin = -1000000,
      xmax = -1000000 + (-154 - (-161))*120000,
      ymin = -2575000,
      ymax = -2575000 + (23 - 18)*120000
    ) +
    annotation_custom(
      grob = ggplotGrob(mpr),
      xmin = 1900000,
      xmax = 2900000,
      ymin = -2600000,
      ymax = -2200000
    ) +
    annotation_custom(
      grob = ggplotGrob(mgu),
      xmin = -2950000,
      xmax = -2500000,
      ymin = -1000000,
      ymax = -1800000
    ) +
    annotation_custom(
      grob = ggplotGrob(mas1),
      xmin = -2500000,
      xmax = -2000000,
      ymin = -1850000,
      ymax = -900000
    ) +
    annotation_custom(
      grob = ggplotGrob(mas2),
      xmin = -2000000,
      xmax = -1500000,
      ymin = -1850000,
      ymax = -900000
    )  +
    annotation_custom(
      grob = ggplotGrob(mmp),
      xmin = -3000000,
      xmax = -2500000,
      ymin = -1000000,
      ymax = 800000    
    )
}

