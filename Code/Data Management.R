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
#Create list of name changes (code_old, fullname_old, code_new etc)

#Duplicate columns
d$fullname_old <- d$Full_name
d$cold_old <- d$Code 

# add other columns to fill out later
#d[c("fullname_new", "code_new")] <-NA

# let's make sure shortlist doesn't have mispellings in dataset and there are only two! 
d[grep("villosa", d$Full_name, ignore.case = T),] %>% select("Full_name") %>% unique()

# make a dataframe of characters to loop through to check partial name matches - can also import when Kay sends
shortlist_partial <- c("neomexicana", "villosa")

# Shortlist (already converted to READable format from txt file recieved)
short_lst <- read_excel("./Data/new_sci_names_DOME_plants.xlsx", trim_ws = T)

short_lst$first <- stringr::word(short_lst$Old, 1)

for(i in 1:nrow(short_lst)){
  d[grep(short_lst[i,4], d$Full_name, ignore.case = T),] %>% select("Full_name") %>% unique() %>% print()  
}


#### with above list check out other possible name combos that they can be under and add to list

### JTS_CHECKME
# initalize column
d$fullname_new <- NA

for(i in seq_along(1:nrow(short_lst))){
  print(i)
  
  for (j in seq_along(1:dim(d)[1])){
    
    d[j,20]<- d %>% mutate(fullname_new= ifelse(as.character(Full_name)== as.character(short_lst[i,1]),
                                               as.character(short_lst[i,2]), 
                                               as.character(Full_name))) 
  }
}


#### why doesn't above loop work? sample code below 
#JTS note: Bianca, I modified the code below and it is now functional. Haven't touched the code above on line 108. 
#The issue was that in "d", Full_name is a factor and not a character string. 
#This fix is suboptimal, the issue was that they were characters and got changed to factors on line 103. 
#PS: To flag things for my attention, use "JTS_CHECKME". Thanks!

d<-as.data.frame(lapply(d, tolower)) #BG_CHECKME: This step erroneously converts Full_name from character back to factor. Is there a different way to do this step? I suspect a lot of variable types get changed here and we don't want that. 
short_lst<-as.data.frame(lapply(short_lst, tolower))

d %>%   # fill out new code y
  mutate(fullname_new= 
           ifelse(as.character(Full_name)== as.character(short_lst[1,1]), 
                  as.character(short_lst[1,2]), 
                  as.character(Full_name)
           )
  ) %>% View()



# Little DF from stack overflow to start off the naming
df<-data.frame(matrix(NA, nrow = 1, ncol = 2,
                  dimnames = list(NULL, paste0("new_Name", 1:2))) ) %>% dplyr::rename(Old_Name = new_Name1)
df$Old_Name<- "Chrysopsis villosa"
df$new_Name2 <- "Heterotheca villosa"

# need codes

# write the file to compare with KAY and get her opinion
write.csv(d, paste0(getwd(), "/", "name_changes.csv"))

#to use for crosswalk with master data "d"
#Do the crosswalk and 
#Also do the crosswalk with "d_13"
#then move on to step 5.
#A

####5. Deal with unknown orgins####

#Flag plants that don't have origin. Develop a reconciliation document for these:
d$RowNum <- c(1:nrow(d)) #for eventual sorting
#write.csv(d, "Data/Tmp/Reference1.csv")
issues <- d[is.na(d$Origin) & d$Plant=="y",
            c("RowNum","Transect_id", "year", "Total_cm", "Code", "Full_name", "Origin")]
issues$Code_new <- "unchanged"
issues$Full_name_new <- "unchanged"
#issues$Code_old <- ""	#Not doing this, the name changes are being dealt with separately
#issues$Full_name_old <- ""
#write.csv(issues, "Data/BiancaReconciliation/Reconciliation1.csv")

