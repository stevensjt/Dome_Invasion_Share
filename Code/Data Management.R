##Data Management.R
##Authors: Jens Stevens (stevensjt@gmail.com)
##The purpose of this script is to process and clean data from different years to generate a 
##single master data file for analysis. Among other things, it reconciles differences in species
##identities across years and assigns native/exotic origins.

####0. Load libraries####
library(readxl)

####1. Read data####
d_13 <- #Old data through 2013
  #These data have been assigned an origin (native vs exotic), by Rebecca Oertel?
  #The main point of reading in this file is to extract the origin data.
  read_excel("./Data/dome_invasion_data.xlsx", sheet = "data") 
d <- #Data through 2019
  #These data contain measurements from 2016 and 2019 that are absent from the previous file
  #These data will become the template for the master data file
  read_excel("./Data/All Years 1997-2019 PermPlot-Entire.xlsx", sheet = "97-2019 EX1") 
d_19_origins <- #Origins data for the new 2019 species
  #These origin data were assigned by Taylor Edwards and Bianca Gonzalez in 2020
  read.csv("./Data/Species/Dome_newspp_origins.2019.csv")

####2. Process data####
d_13$Origin[d_13$Origin=="NA"] <- NA #Recode NA from text to NA

#Define exotics and native species list, and assign to d:
exotics <- unique(d_13$Code[which(d_13$Origin=="exotic")])
exotics <- c(exotics, 
             unique(d_19_origins$Code[which(d_19_origins$Origin=="exotic")]))
natives <- unique(d_13$Code[which(d_13$Origin=="native")])
natives <- c(natives, 
             unique(d_19_origins$Code[which(d_19_origins$Origin=="native")]))
d$Origin[d$Code%in%exotics] <- "exotic" 
d$Origin[d$Code%in%natives] <- "native"

#Convert transect ID's to be consistently lower-case
d$Transect_id <- tolower(d$Transect_id)

#Remove transect only sampled in one yr.
d <- d[-which(d$Transect_id=="3mbu2a"),] 

#Burn severity issues
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3LAU1)
d[which(d$Transect_id=="3LAU1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"
#Investigate multiple burn severity issue 
#(CHECKME_JTS this was a comment from earlier, not sure if resolved)

####3. Deal with ID issues (simplify)####

#Identify anything that's not a plant:
not_plant <- c("Gravel", "Soil", "Litter", "Dead wood", "Rock", "Tree root, exposed, living",
               "Animal feces", "combined soil,cryp,pumc,grav,rock", "combined soil,pumc,grav,rock",
               "Pumice soil", "Moss", "Mushroom", "Fungus", "Cryptogram", "pine cone", 
               "Human debris, inorganic", "Ant hill")
d$Plant <- "y"
d$Plant[d$Full_name%in%not_plant] <- "n" 

#Consolidate anything that's not a plant:
View(d[d$Full_name == "combined soil,cryp,pumc,grav,rock", ])
d[d$Full_name== "Dead wood", "Full_name"] <- "Wood"
d[d$Full_name== "Tree root, exposed, living", "Full_name"] <- "Root"
d[d$Full_name== "combined soil,cryp,pumc,grav,rock", "Full_name"] <- "Bare"
d[d$Full_name== "combined soil,pumc,grav,rock", "Full_name"] <- "Bare"
d[d$Full_name== "Soil", "Full_name"] <- "Bare"
d[d$Full_name== "Bare", "Code"] <- "bare"
d[d$Full_name== "pine cone", "Code"] <- "litt"
d[d$Full_name== "pine cone", "Full_name"] <- "litter" #CHECKME_JTS: Kay are pine cones called litter now?

#Flag plants that don't have origin. Develop a reconciliation document for these:
d$RowNum <- c(1:nrow(d)) #for eventual sorting
#write.csv(d, "Data/Tmp/Reference1.csv")
issues <- d[is.na(d$Origin) & d$Plant=="y",
            c("RowNum","Transect_id", "year", "Total_cm", "Code", "Full_name", "Origin")]
issues$Code_new <- "unchanged"
issues$Full_name_new <- "unchanged"
write.csv(issues, "Data/Tmp/Reconciliation1.csv")
