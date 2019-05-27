qa_ksat <- function(d) {

library(dplyr)

# check if dataset has required data columns

rc <- c("boreholedepth",
        "hzdept",
        "hzdepb",
        "boreholewaterlevelinit",
        "boreholewaterlevelfinal")
  
    if(any(!(rc %in% colnames(d)))){
      
    stop("Dataset does not contain required data", call. = FALSE)
    }
      
# Calculate top and bottom of test zone
d <- mutate(d, "TTZ" = d$boreholedepth - ((d$boreholewaterlevelinit + d$boreholewaterlevelfinal) / 2))
d <- mutate(d, "BTZ" = d$boreholedepth)

# Calculate distances to upper and lower horizons
d <- mutate(d, "DUH" = d$TTZ - d$hzdept)
d <- mutate(d, "DLH" = d$hzdepb - d$BTZ)

# Check for potential data issues

# Negative DUH or DLH indicate the test zone bridges across horizons, ksat data may not be reliable
if(any(d$DUH < 0 )){  
d_ue <- filter(d, DUH < 0)
d_ue <- filter(d_ue, !is.na(DUH))
d_ue <- select(d_ue, pedon_id, hzname, repnum, DUH)
d_ue <- distinct(d_ue)

hzn_u_errs <- d_ue
}

if(any(d$DLH < 0)){
  d_le <- filter(d, DLH < 0)
  d_le <- filter(d_le, !is.na(DLH))
  d_le <- select(d_le, pedon_id, hzname, repnum, DLH)
  d_le <- distinct(d_le)
  
hzn_l_errs <- d_le
}

# Test zones near the top and/or bottom of a horizon could also potentially be an issue, but it is not as serious as bridging horizons
if(any(d$DLH < 2*((d$boreholewaterlevelinit + d$boreholewaterlevelfinal) / 2))){
  d_len <- filter(d, DLH < 2*((d$boreholewaterlevelinit + d$boreholewaterlevelfinal) / 2))
  d_len <- filter(d_len, !is.na(DLH))
  d_len <- select(d_len, pedon_id, hzname, repnum, DLH)
  d_len <- distinct(d_len)
  
hzn_l_warn <- d_len
}

if(any(d$DUH < 2)){
  d_uen <- filter(d, DUH < 2)
  d_uen <- filter(d_uen, !is.na(DUH))
  d_uen <- select(d_uen, pedon_id, hzname, repnum, DUH)
  d_uen <- distinct(d_uen)
  
hzn_u_warn <- d_uen
}

qa_results <- list(hzn_u_errs, hzn_l_errs, hzn_u_warn, hzn_l_warn)

names(qa_results) <- c("Upper Horizon Errors", "Lower Horizon Errors", "Upper Horizon Warnings", "Lower Horizon Warnings")

return(qa_results)

}
