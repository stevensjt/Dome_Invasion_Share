##Data Management.R
##Authors: Jens Stevens (stevensjt@gmail.com) Bianca Gonzalez (bianca.glez94@gmail.com)
##The purpose of this script is to process and clean data from different years to generate a 
##single master data file for analysis. Among other things, it reconciles differences in species
##identities across years and assigns native/exotic origins.

####0. Load libraries####
library(vctrs)
library(readxl)
library(dplyr)
library(readr)

####1. Read data####
d_13 <- #Old data through 2013
  #These data have been assigned an origin (native vs exotic), by Rebecca Oertel?
  #The main point of reading in this file is to extract the origin data.
  read_excel("./Data/KayOriginalMaster/dome_invasion_data.xlsx", sheet = "data") 
d <- #Data through 2019.
  #These data contain measurements from 2016 and 2019 that are absent from the previous file d_13,
  #Except that species that appeared in the dataset for the first time in 2019, 
    #either because they were newly identified or because their name changed,
    #had their origin data were assigned by Taylor Edwards and Bianca Gonzalez in 2020.
    #Apparently there were no new species in 2016
  #As with the raw data file file All Years 1997-2019 PermPlot-Entire.xlsx, 
    #the transect 3mbu2a was sampled only in 2002 and according to Kay is irrelevant data,
    #and has been deleted.
  #These data will become the template for the master data file
  #read_excel("./Data/All Years 1997-2019 PermPlot-Entire.xlsx", sheet = "97-2019 EX1") #Deprecate
#d_19_origins <- #Origins data for the new 2019 species Deprecate
  read_csv("./Data/Dome_newspp_origins.2019.csv", col_types = cols())

####2. Process data 2013 and 2019 exotics ####
d_13$Origin[d_13$Origin=="NA"] <- NA #Recode NA from text to NA

#Define exotics and native species list, and assign to d:
exotics <- unique(d_13$Code[which(d_13$Origin=="exotic")])
exotics <- c(exotics, 
             unique(d$Code[which(d$Origin=="exotic")]))

natives <- unique(d_13$Code[which(d_13$Origin=="native")])
natives <- c(natives, 
             unique(d$Code[which(d$Origin=="native")]))
d$Origin[d$Code%in%exotics] <- "exotic" 
d$Origin[d$Code%in%natives] <- "native"
rm(d_13) #done with the 2013 data frame.

#Convert transect ID's to be consistently lower-case
d$Transect_id <- tolower(d$Transect_id)

#Address burn severity issues
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3lau1)
d[which(d$Transect_id=="3lau1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"


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
d[d$Full_name== "pine cone", "Code"] <- "litt" #Confirmed with Kay that pine cones are now called litter
d[d$Full_name== "pine cone", "Full_name"] <- "litter" 
#DONE

####4. Deal with name changes####
## BG did this part
#Create list of name changes (code_old, fullname_old, code_new, fullname_new)

# Shortlist (already converted to excel from txt file)
short_lst <- read_excel("./Data/Species/new_sci_names_DOME_plants.xlsx", trim_ws = T)

#Duplicate columns
d$fullname_old <- as.character(tolower(d$Full_name))
d$code_old <- as.character(tolower(d$Code))

# let's make sure shortlist doesn't have variations of a plant name
short_lst$first <- stringr::word(short_lst$New, 1)
short_lst$last <- stringr::word(short_lst$New, 2)

list_firstnames = list()
list_secondnames = list()

#### check out other possible name combos from the old names in shortlist  
for(i in 1:nrow(short_lst)){
  
  #make sure there are no mispellings or variations of plant from shortlist -- correct if there are
  list_firstnames[[i]] <- d[grep(short_lst[i,"first"], d$Full_name, ignore.case = T),] %>% select("Full_name") %>% unique() 
  list_secondnames[[i]] <- d[grep(short_lst[i,"last"], d$Full_name, ignore.case = T),] %>% select("Full_name") %>% unique() 
  }

# interested in multiple variations of a name for first/second words in shortlist 
list_firstnames <- list_firstnames %>% 
  purrr::discard(function(x) nrow(x) <= 1)

list_secondnames <- list_secondnames %>% 
  purrr::discard(function(x) nrow(x) <= 1)

# Insert new names
#Add new name to species where old name is flagged to have changed:
d$fullname_new <- tolower(short_lst$New)[pmatch(d$fullname_old,tolower(short_lst$Old), 
                                                       duplicates.ok = TRUE)] 

# gives indices of short_lst$old that match the old ones and returns -- when we search that index []
# in the new short_list$new -- they correspond & hence can replace w new vals 

d$fullname_new[is.na(d$fullname_new)] <- d$fullname_old[is.na(d$fullname_new)]

# first two letters replace in code_new
d$code_new<- paste(substr(d$fullname_new, start = 1, stop = 2), 
      substr(sub("^\\S+\\s+", '', d$fullname_new), start = 1, stop = 2), sep = "")

# if nonplant - replace new code
d<- d %>% mutate(code_new= ifelse(d$Plant =='y', d$code_new, d$code_old))



####5. Deal with unknown orgins and other issues####

#Flag plants that don't have origin. Develop a reconciliation document for these:
d$RowNum <- c(1:nrow(d)) #for eventual sorting

# could be new plants or non plants -- or only identified to genus -- 
#no_origins <- d[is.na(d$Origin),]


issues <- d[is.na(d$Origin) & d$Plant=="y",
            c("RowNum","Transect_id", "year", "Total_cm", "Code", "Full_name", "Origin")]
issues$code_new <- "unchanged"
issues$fullname_new <- "unchanged"
#write.csv(issues, "Data/BiancaReconciliation/issues.csv") #DON'T DO THIS, HAS BEEN DONE

##IMPORTANT: At this point Bianca did a bunch of manual checking of the data in the issues document
##This document "issues.csv" above ended up being named "issues-jens.csv". 
##The reason "issues-jens" has 1107 rows and the "issues" doc created here only has 994 is that
##the doc created in R doesn't include the new 2019 species (because Origin is not NA) but 
##when Jens created the earlier version that became "issues-jens", the version of d used to create
##the object "issues' above somehow didn't have the 2019 Origins added in. That was Jens' mistake somehow.
##In any case, the "issues" doc will be reconciled with "d" below.

# How to match rownums of recon ^ with the d dataframe and replace origin vals if they match
# Can you write this using pmatch and/or which? I'd like some more exposure to this kind of 
# code 
#?pmatch()
#?which()

#JTS for BG: Here is the answer to your question above:

####6. Incorporate the fixed issues into "d"####
#JTS: Start Here (and eventually deprecate the commented lines immediately preceding.)
issues_fixed <- #Need to use "read_csv" rather than "read.csv" to keep columns as characters. Requires readr
  read_csv("./Data/BiancaReconciliation/issues-Jens.csv") 
issues_fixed <- #IMPORTANT: Because this document had been sorted alphabetically, it needs to be re-sorted by RowNum.
  issues_fixed[order(issues_fixed$RowNum),] 
issues_fixed$Code <- #Migrated "code_new" into "Code" in issues doc, IF it changed
  ifelse(issues_fixed$code_new=="unchanged",issues_fixed$Code,issues_fixed$code_new)
issues_fixed$Full_name <- #Migrated "fullname_new" into "Code" in issues doc, IF it changed
  ifelse(issues_fixed$fullname_new=="unchanged",issues_fixed$Full_name,issues_fixed$fullname_new)
tmp_d <- d #Temporary data frame to do the integration, this will eventually get written to file but leave that for Jens to do.
tmp_d[which(tmp_d$RowNum%in%issues_fixed$RowNum),c("Code","Full_name","Origin")] = 
  issues_fixed[,c("Code","Full_name","Origin")]
#Now compare d to tmp_d for quality control to make sure the three columns in question were changed appropriately. 


##Todo: Develop comprehensive species list by year to see if there were any new species in 2016 and what happened to their origins data.
#Origins need to be imported from issues_fixed for all row matches into d.
#Check for duplicate species from the same transect-year
#There is no Frovaria sp ANYWHERE ON THE INTERNET. Fragaria?
#Cryptogram should not be a plant.
#At the end, Kay should check for genera that must be native even if unknown (like Carex).

