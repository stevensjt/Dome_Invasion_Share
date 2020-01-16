#calculating mullen totals, B. Gonzalez
library(readxl)
library(dplyr)
library(ggplot2)

#### import data ####
setwd('/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/Dome_Analysis/Dome_Invasion_Share')
d_old <- read_excel("./Data/All_Years_1997_2019_PermPlot_Entire.xlsx", sheet = "97-2019 EX1") #Old data through 2013

#calculate % of mullen per year and % of mullen overall in all transects from 1997-2019

#calculate % mullen all transects

d_veth<-d_old %>% 
  filter(`Species code`=="veth")

d_old %>% 
  unique()

# group by year and transect ID so can see total sum in cm of VETH over the years
veth_dist<-d_old %>% 
  group_by(year, Transect_id) %>% 
  filter(`Species code`=="veth") %>% 
  summarise(sum=sum(Canopy_cm) + sum(Basal_cm)) %>% 
  mutate(prop = (sum/5000)*100) %>% 
  arrange(desc(prop)) 

# would be interesting to look at the veth growing rates in the area
head(veth_dist)

# looks like first four values are above 15% of the transect is veth in the area. 

# need to count number of instances where veth is greater than 30 cms on the transect (at canopy) 
# and see if overlapping with other vegetation



# proximity to roads, soil type, hiking trail distance,
# elevation profile, aspect -- all different factors we can look at when deciding where 
# invasives grow best and over the years how they are spreading geographically
# possibly can map the location of veth and see distribution in the landscape 

# 2HBS1, 3MBS1, 3HBU2 and 2MAS1 are all transects where I should look at the spacing of veth
# over the transect and see if its' more than .6m in canopy or basal cover in one area



# if in august, veth0 could be veth from this year just dry, so can still use imagery from June/July to identify
# or can use imagery from august...  or can have two signatures of veth dead and veth alive

# distribution of VETH over the years is important as well

ggplot(data=d_old, mapping = aes(year)) + geom_bar()

# normalize by number of transects




  
  