#### THIS SCRIPT IS FOR GENERATING PROJECT IMPACT SCORES FOR AN INDIVIDUAL SSO IN REGION 11 FOR A GIVEN FISCAL YEAR

# Load libraries
library(soilDB)
library(sp)
library(plyr)
library(sf)
library(dplyr)
library(stringr)
library(cdlTools)
library(tidyverse)
library(tidycensus)
library(purrr)

### STEP 1. GATHER DATA FOR ANALYSIS

# a. Get Projects from LIMS
pi <- parseWebReport("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_project_from_LIMS", args = list(p_mlrassoarea = "11-CLI", p_fy = "2018"))

# b. Get MUKEYS from LIMS
polys <- parseWebReport('https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-PROJECT_MUKEY_BY_GOAL_YEAR', args = list(msso = "11-CLI", fy="2018", asym='%', proj='0'))

# c. Get Population Data from US Census
census_api_key("INSERTKEY")

us <- unique(fips_codes$state)[1:51]

totalpop <- map_df(us, function(x) {
  get_acs(geography = "county", variables = "B01003_001", 
          state = x)
})

# d. Get Spatial Data (can only handle one SSO at a time)
ssmu <- read_sf(dsn = "E:/geodata/project_data/11CLI/RTSD_MLRA_11-CLI_FY18.gdb", layer = "MUPOLYGON")

# e. Get custom CSV with SSO and Region 11 totals
tsso <- read.csv("impact_data.csv")


### STEP 2. CLEAN DATA

## CLEAN MUKEY DATA FROM LIMS (polys)

# a. rename columns
names(polys) <- make.names(names(polys))

# b. remove description column (it's full of NAs anyway since we specifically asked not to return it in the above query)
polys$Description <- NULL

# c. make an index for removing unwanted project types
idx <- grep("EVAL|SDJR|MLRA", polys$Project.Type.Name)

# d. keep only the rows matching the index
polys <- polys[idx, ]

# e. rename column 10 FIPS (there was a duplicate Area.Symbol name generated earlier)
colnames(polys)[10] <- "FIPS"

# f. make an index for removing unwanted FIPS
idx4 <- grep("-", polys$FIPS, invert = TRUE)

# g. keep only the rows matching the index
polys <- polys[idx4, ]

# f. make a new column of GEOID (state and county fips) using FIPS column and fips function
polys$GEOID <- paste0(fips(substr(polys$FIPS, 1,2)), str_sub(polys$FIPS, -3, -1))

# g. make an index for removing unwanted population data
idx3 <- grep(paste(polys$GEOID, collapse = "|"), totalpop$GEOID)

# h. keep only the rows matching the index
totalpop <- totalpop[idx3, ]

# i. convert to data.frame
totalpop <- as.data.frame(totalpop)

# j. join polys data and population data by GEOID
polypop <- left_join(polys, totalpop, by = "GEOID")

# k. sum the total population for each project
prpop <- aggregate(polypop$estimate, by = list(projectname = polypop$Project.Name), FUN = sum)

# l. rename column for joining later on
colnames(prpop)[2] <- "population"


## CLEAN SPATIAL DATA (ssmu)

# a. make an index for removing unwanted polygons
idx2 <- grep(paste(polys$mukey, collapse = "|"), ssmu$MUKEY)

# b. keep only the rows matching the index
ssmu <- ssmu[idx2, ]

# c. rename column for joining
colnames(ssmu)[3] <- "mukey"

# d. join project names with polygons in spatial data
ssmu_poly <- merge(ssmu, polys[, c("mukey", "Project.Name")], by = "mukey")

# e. remove geometry (we don't need it)
ssmu_poly$geometry <- NULL

# f. return a count of the polygons in each project
sm_ssmu_poly <- count(ssmu_poly, Project.Name)

# g. rename columns in the combined polygons data (ssmu) for joining later on
sm_ssmu_poly$projectname <- sm_ssmu_poly$Project.Name
sm_ssmu_poly$polygons <- sm_ssmu_poly$n


### STEP 3. COMBINE PROJECT DATA TO SSO TOTALS DATA

# a. rename column in SSO totals (tsso) for joining
tsso$mlrassoarea <- as.character(tsso$Office.Area)

# b. join project data and totals
pd_tls <- merge(pi, tsso[, c("mlrassoarea", "Total.Area.SYMS", "Total.NATSYMS", "Total.Polygons", "Total.Acres", "total.population")], by = "mlrassoarea")
 
# c. join spatial data and project data together
sm_ssmu_poly <- merge(sm_ssmu_poly, prpop[, c("projectname","population")], by = "projectname")

# d. join totals data and sso data together
imp_d <- merge(pd_tls, sm_ssmu_poly[, c("projectname", "polygons", "population")], by = "projectname")

# e. calculate weighted impact factors
imp_d$im_asym <- (imp_d$n_areasymbol / imp_d$Total.Area.SYMS) * 20
imp_d$im_natsym <- (imp_d$n_nationalmusym / imp_d$Total.NATSYMS) * 20
imp_d$im_poly <- ( imp_d$polygons / imp_d$Total.Polygons) * 20
imp_d$im_acres <- (imp_d$acre_landcat / imp_d$Total.Acres) * 20
imp_d$im_popu <- (imp_d$population / imp_d$total.population) * 20

# f. sum the individual factors
imp_d$impact_score <- imp_d$im_asym + imp_d$im_natsym + imp_d$im_poly + imp_d$im_acres + imp_d$im_popu

# g. keep only the columns needed
final_d <- imp_d[, c(2, 1, 34,35,36,37,38,39)]

# h. round values for easier reading
final_d[, c(-1,-2)] <- round(final_d[, c(-1, -2)], 2)

# i. save file to .csv
write.csv(final_d, paste0(final_d$mlrassoarea[1], "_imp_score", ".csv"))
