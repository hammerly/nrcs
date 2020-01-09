library(readxl)
library(dplyr)

d <- read_excel("D:/Cloud/Annum_Data/FY2019/Region 11/SSO/CLI/MLRA/Minelands/acres/2mo159_acres.xlsx")

dg <- d %>% group_by(MUSYM) %>% summarise(ac = sum(ACRES))

dg$ac <- dg$ac*(439360/439215)

dg$ac <- round(dg$ac, 0)


sum(dg$ac)

439360/439215

write.table(dg, "D:/Cloud/Annum_Data/FY2019/Region 11/SSO/CLI/MLRA/Minelands/acres/3mo159_acres.csv", sep = "|", quote = FALSE, row.names = FALSE)
