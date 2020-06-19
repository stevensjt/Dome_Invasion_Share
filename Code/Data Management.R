##Data Management.R
##Authors: Jens Stevens (stevensjt@gmail.com) Bianca Gonzalez (bianca.glez94@gmail.com)
##The purpose of this script is to process and clean data from different years to generate a 
##single master data file for analysis. Among other things, it reconciles differences in species
##identities across years and assigns native/exotic origins.

####0. Load libraries####
#library(vctrs)
#library(stringr) #for word(); version 1.4.0 deprecated?
library(readxl) #For read_excel; version 1.3.1
library(readr) #For read_csv; version 1.3.1
library(dplyr) #for pipes; version 0.8.3
options(warn = -1) #A lot of warnings pop up because of read_csv; these can be safely ignored.

####1. Read data####
d_13 <- #Old data through 2013
  #These data have been assigned an origin (native vs exotic), by Rebecca Oertel?
  #The main point of reading in this file is to extract the origin data.
  read_excel("./Data/Raw/KayOriginalMaster/dome_invasion_data.xlsx", sheet = "data") 
d <- #Data through 2019.
  #These data contain measurements from 2016 and 2019 that are absent from the previous file d_13,
  #Except that species that appeared in the dataset for the first time in 2019, 
    #either because they were newly identified or because their name changed,
    #had their origin data assigned by Taylor Edwards and Bianca Gonzalez in 2020.
    #Apparently there were no new species in 2016
  #As with the raw data file file All Years 1997-2019 PermPlot-Entire.xlsx, 
    #the transect 3mbu2a was sampled only in 2002 and according to Kay is irrelevant data,
    #and has been deleted.
  #These data will become the template for the master data file
  read_csv("./Data/Raw/Dome_newspp_origins.2019.csv", col_types = cols())

####2. Process data 2013 and 2019 Origins ####
##"Origins" refers to exotic vs native.

#Recode NA from text to NA:
d_13$Origin[d_13$Origin=="NA"] <- NA 

#Define exotics and native species list, and assign to d:
exotics <- 
  c(unique(d_13$Code[which(d_13$Origin=="exotic")]), #1997-2013
    unique(d$Code[which(d$Origin=="exotic")])) #2016-2019
natives <- 
  c(unique(d_13$Code[which(d_13$Origin=="native")]), #1997-2013
    unique(d$Code[which(d$Origin=="native")])) #2016-2019
d$Origin[d$Code%in%exotics] <- "exotic" 
d$Origin[d$Code%in%natives] <- "native"
rm(d_13) #done with the 2013 data frame.

#Convert transect ID's to be consistently lower-case:
d$Transect_id <- tolower(d$Transect_id)

#Address burn severity issues:
#Only one transect burned at low severity in the Dome Fire, 
#so we combined that with "Unburned".
d[which(d$Transect_id=="3lau1"),"RevBI"] <- "Unburned"


####3. Simplify non-living codes####

#Identify anything that's not a plant:
not_plant <- c("Gravel", "Soil", "Litter", "Dead wood", "Rock", "Tree root, exposed, living",
               "Animal feces", "combined soil,cryp,pumc,grav,rock", "combined soil,pumc,grav,rock",
               "Pumice soil", "Moss", "Mushroom", "Fungus", "Cryptogram", "pine cone", 
               "Human debris, inorganic", "Ant hill")
d$Plant <- "y"
d$Plant[d$Full_name%in%not_plant] <- "n" 

#Consolidate anything that's not a plant:
#View(d[d$Full_name == "combined soil,cryp,pumc,grav,rock", ])
d[d$Full_name== "Dead wood", "Full_name"] <- "Wood"
d[d$Full_name== "Tree root, exposed, living", "Full_name"] <- "Root"
d[d$Full_name== "combined soil,cryp,pumc,grav,rock", "Full_name"] <- "Bare"
d[d$Full_name== "combined soil,pumc,grav,rock", "Full_name"] <- "Bare"
d[d$Full_name== "Soil", "Full_name"] <- "Bare"
d[d$Full_name== "Bare", "Code"] <- "bare"
d[d$Full_name== "pine cone", "Code"] <- "litt" #Confirmed with Kay that pine cones = litter
d[d$Full_name== "pine cone", "Full_name"] <- "litter" 
#DONE

####4. Deal with name changes####
#Bianca, Kay and Taylor developed this file identifying species whose name changed:
name_changes <- #Import name change data;
  read_excel("./Data/Intermediate/Species/new_sci_names_DOME_plants.xlsx", trim_ws = T)
d[which(d$Full_name%in%name_changes$Old),"Code"] <- #This one must go first
  name_changes$new_code[na.exclude(pmatch(d$Full_name,name_changes$Old, duplicates.ok = TRUE))]
d[which(d$Full_name%in%name_changes$Old),"Full_name"] <-
  name_changes$New[na.exclude(pmatch(d$Full_name,name_changes$Old, duplicates.ok = TRUE))]
d$RowNum <- c(1:nrow(d)) #for eventual reconciliation of issues identified below.


####5. Identify unknown orgins and other issues####
##Flag plants that don't have origin. Develop a reconciliation document for these:

#issues <- d[is.na(d$Origin) & d$Plant=="y",
#            c("RowNum","Transect_id", "year", "Total_cm", "Code", "Full_name", "Origin")]
#issues$code_new <- "unchanged"
#issues$fullname_new <- "unchanged"
#write.csv(issues, "Data/issues.csv") #DON'T DO THIS, HAS BEEN DONE

##IMPORTANT: At this point Bianca did a bunch of manual checking of the data in the issues document
##This document "issues.csv" above ended up being named "issues-jens.csv". 
##The reason "issues-jens" has 1107 rows and the "issues" doc created here only has 994 is that
##the doc created in R doesn't include the new 2019 species (because Origin is not NA) but 
##when Jens created the earlier version that became "issues-jens", the version of d used to create
##the object "issues' above somehow didn't have the 2019 Origins added in. That was Jens' mistake somehow.
##In any case, the "issues" doc will be reconciled with "d" below.


####6. Incorporate the fixed issues into "d"####
issues_fixed <- #Need to use "read_csv" rather than "read.csv" to keep columns as characters.
  read_csv("./Data/Intermediate/BiancaReconciliation/issues-Jens.csv") 
issues_fixed <- #IMPORTANT: Because this document may have been sorted alphabetically, 
  #it needs to be re-sorted by RowNum.
  issues_fixed[order(issues_fixed$RowNum),] 
issues_fixed$Code <- #Migrated "code_new" into "Code" in issues doc, IF it changed
  ifelse(issues_fixed$code_new=="unchanged",issues_fixed$Code,issues_fixed$code_new)
issues_fixed$Full_name <- #Migrated "fullname_new" into "Code" in issues doc, IF it changed
  ifelse(issues_fixed$fullname_new=="unchanged",issues_fixed$Full_name,issues_fixed$fullname_new)


#START HERE: Assign origins to unknown species where appropriate
#Everything should be good in issues_fixed, I solved whatever the problem was with Phacelia.
#unk_genera <- unique(issues_fixed[which(is.na(issues_fixed$Origin)),c("Full_name", "Origin")]) #Done? Deprecate?
#copy this list into new_sci_names_DOME_plants_notes.xlsx and edit manually with help from Kay. #Done? Deprecate?
#Maybe still need to manually update issues-jens to include nativity information? #Done? Deprecate?
#Then do the nativity merge below (double check code). #Do this once more for good measure, I think.

issues_fixed$Origin <- #Migrated "fullname_new" into "Code" in issues doc, IF it changed
  ifelse(is.na(issues_fixed$Origin),issues_fixed$Origin_new,issues_fixed$Origin)

#Todo: Manually double check all remaining unidentified species with known genera (e.g. euphorbia) to simplify.
tmp_d <- d #Temporary data frame to do the integration, this will eventually get written to file but leave that for Jens to do.
tmp_d[which(tmp_d$RowNum%in%issues_fixed$RowNum),c("Code","Full_name","Origin")] <- 
  issues_fixed[,c("Code","Full_name","Origin")]
#Now compare d to tmp_d for quality control to make sure the three columns in question were changed appropriately. 
#Looks good

##Todo: 
#Develop comprehensive species list by year to see if there were any new species in 2016 and what happened to their origins data.
tmp_plants <- tmp_d[tmp_d$Plant=="y",]
unique_sp_list <- 
  unique(tmp_plants[tmp_plants$year == 1997,"Full_name"])
unique_sp_list$year = 1997
for(y in unique(tmp_plants$year)[(2:6)]){
  vector_unique <- unique(tmp_plants[tmp_plants$year == y,"Full_name"])[[1]]
  index_new <- which(!(vector_unique %in% unique_sp_list$Full_name))
  spp_new <- vector_unique[index_new]
  df_new <- unique(tmp_plants[tmp_plants$Full_name %in% spp_new,"Full_name"])
  df_new$year = y
  unique_sp_list <- rbind(unique_sp_list, df_new)
    
}

  
#Origins need to be imported from issues_fixed for all row matches into d.
#Check for duplicate species from the same transect-year
#There is no Frovaria sp ANYWHERE ON THE INTERNET. Fragaria?
#Cryptogram should not be a plant.
#At the end, Kay should check for genera that must be native even if unknown (like Carex).

