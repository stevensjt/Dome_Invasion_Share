##Data Management.R
##Authors: Jens Stevens (stevensjt@gmail.com)
##The purpose of this script is to process and clean data from different years to generate a 
##single master data file for analysis. Among other things, it reconciles differences in species
##identities across years and assigns native/exotic origins.

####0. Load libraries####
library(vctrs)
library(readxl)
library(dplyr)

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

####2. Process data 2013 and 2019 exotics ####
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

#Burn severity issues
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3LAU1)
d[which(d$Transect_id=="3LAU1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"
#Investigate multiple burn severity issue 
#(CHECKME_JTS this was a comment from earlier, not sure if resolved)


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
d[d$Full_name== "pine cone", "Code"] <- "litt"
d[d$Full_name== "pine cone", "Full_name"] <- "litter" #CHECKME_JTS: Kay are pine cones called litter now?
#DONE

####4. Deal with name changes####
## BG to finish
#Create list of name changes (code_old, fullname_old, code_new, fullname_new)

# Shortlist (already converted to excel from txt file)
short_lst <- read_excel("./Data/new_sci_names_DOME_plants.xlsx", trim_ws = T)

#Duplicate columns
d$fullname_old <- as.character(tolower(d$Full_name))
d$code_old <- as.character(tolower(d$Code))

# let's make sure shortlist doesn't have variations of a plant name

### investigating results using ITIS integrated Taxonomic Information System

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

# need code_new column now -- use same pattern matching technique ^

# doesn't account for multiple codes so going to take first two letters from each new full_name
# d$code_new <- tolower(short_lst$new_code)[pmatch(d$code_old, as.character(tolower(short_lst$old_code)),
#                                                 duplicates.ok = T)]

#d$code_new[is.na(d$code_new)] <- d$code_old[is.na(d$code_new)]

# need to replace only plants with code d[d$Plant=="y",] %>% dplyr::select(fullname_new)

#JTS_CHECKME - line 129

# first two letters replace in code_new
d$code_new<- paste(substr(d$fullname_new, start = 1, stop = 2), 
      substr(sub("^\\S+\\s+", '', d$fullname_new), start = 1, stop = 2), sep = "")


#to use for crosswalk with master data "d"
#Do the crosswalk and
#Also do the crosswalk with "d_13"
#then move on to step 5.
#A


####5. Deal with unknown orgins####

#Flag plants that don't have origin. Develop a reconciliation document for these:
d$RowNum <- c(1:nrow(d)) #for eventual sorting

# could be new plants or non plants -- or only identified to genus -- 
#no_origins <- d[is.na(d$Origin),]

# can make an object or filter in excel
#ssp_origins <- d[grepl("spp", d$Full_name, ignore.case = T),]

# reconciliation 

#write.csv(d, "Data/Tmp/Reference1.csv")
issues <- d[is.na(d$Origin) & d$Plant=="y",
            c("RowNum","Transect_id", "year", "Total_cm", "Code", "Full_name", "Origin")]
issues$Code_new <- "unchanged"
issues$Full_name_new <- "unchanged"

write.csv(no_origins, "./Data/BiancaReconciliation/Reconciliation_no_origins.csv")


