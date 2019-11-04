#Data Management
library(readxl)
d<-read_excel("../Data/All Years 1997-2016 PermPlot-Entire.xlsx",sheet = "97-2016 EX1")

####Data Processing####
d$Transect_id <- tolower(d$Transect_id)

length(unique(d$Transect_id))
unique(d$Transect_id)
clipr::write_clip(unique(d$Transect_id))
