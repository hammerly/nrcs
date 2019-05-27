library(soilDB)
library(lubridate)

tss <- parseWebReport('https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_tss_progress_from_LIMS', args = list(p_nasissitename = "%11%", p_fy="2017"))

tss2 <- read.csv("r11_tss.csv")

tss2$Scheduled.Start.Date <- as.Date(tss2$Scheduled.Start.Date, format = "%m/%d/%Y")
tss2$Scheduled.Completion.Date <- as.Date(tss2$Scheduled.Completion.Date, format = "%m/%d/%Y")
tss2$Date.Started <- as.Date(tss2$Date.Started, format = "%m/%d/%Y")
tss2$Date.Completed <- as.Date(tss2$Date.Completed, format = "%m/%d/%Y")

tss3 <- tss2[tss2$Date.Completed >= "2018-05-01" & tss2$Date.Completed <= "2018-05-31",]
