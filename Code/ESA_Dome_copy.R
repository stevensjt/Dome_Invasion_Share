
##ESA Dome code - TE copy
## modified: 1/6/19


##Process Species Attributes

library(readxl) #version 1.3.1 for read_excel()
library(dplyr) #version
library(ggplot2) #version 3.1.0
library(reshape2)
library(splines)
library(scales)
stdErr <- function(x) sqrt(var(x, na.rm = T)/length(na.exclude(x)))

####1. Data input and processing####
d_old <- read_excel("./Data/dome_invasion_data.xlsx", sheet = "data") #Old data through 2013
## use above code
d_old$Origin[d_old$Origin=="NA"] <- NA #Recode NA from text to NA
exotics <- unique(d_old$Code[which(d_old$Origin=="exotic")])
natives <- unique(d_old$Code[which(d_old$Origin=="native")])
#unk <- unique(d_old$Code[which(is.na(d_old$Origin))])
#unk_full <- unique(d_old$Full_name[which(is.na(d_old$Origin))])
d <- # took ./Data/ out; change to 2019
  read_excel("./Data/All Years 1997-2019 PermPlot-Entire.xlsx", sheet = "97-2019 EX1") 

# Species Code > Code; live/dead > Status; GF > Growth_form; Basal + canopy cm > 
# Total_cm; Total transect_cm > Transect_cm; %C > PctC
d$Origin[d$Code%in%exotics] <- "exotic" 
d$Origin[d$Code%in%natives] <- "native"
d$Transect_id <- toupper(d$Transect_id) #Uppercase for consistency
d <- d[-which(d$Transect_id=="3MBU2A"),] #Remove transect only sampled in one yr.
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3LAU1)
d[which(d$Transect_id=="3LAU1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"
#Investigate multiple burn severity issue:

d <- #Merge live and dead cover of a single species on a single transect|year
  d %>%
  group_by(Transect_id, year, Code) %>%
  summarise(
    RevBI = unique(RevBI), FS_BAND = unique(FS_BAND), TRT = unique(TRT),
    Full_name = unique(Full_name),
    Status = paste(Status, collapse = ''),
    Growth_form = unique(Growth_form), Origin = unique(Origin),
    Basal_cm = sum(Basal_cm), Canopy_cm = sum(Canopy_cm), 
    Total_cm = sum(Total_cm), Transect_cm = unique(Transect_cm),
    PctC = sum(PctC)#, PctB = sum(PctB) #Last one deprecated in 2016 data
    
  )

d_fs <- d[which(d$FS_BAND == "FS"),]
  

d_annual <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(Transect_id, year) %>%
  summarise(
    RevBI = unique(RevBI),
    FS_BAND = unique(FS_BAND), 
    TRT = unique(TRT),
    PctC_exotic = sum(PctC[Origin == "exotic"], na.rm = T),
    PctC_lolium = sum(PctC[grep("Lolium",Full_name)], na.rm = T), #deprecate?
    PctC_brin = sum(PctC[grep("brin",Code)], na.rm = T), #deprecate?
    Richness_exotic = length(which(Origin=="exotic")),
    Richness_native = length(which(Origin=="native")),
    Prop_exotic = length(which(Origin=="exotic")) / 
      (length(which(Origin=="exotic")) + length(which(Origin=="native")))

  )

d_annual_fs <- d_annual[which(d_annual$FS_BAND == "FS"),]
d_fs_mean <- d_annual_fs %>%
  group_by(year,RevBI,TRT) %>%
  summarise(PctC_exotic_mean = mean(PctC_exotic), 
            PctC_exotic_se = stdErr(PctC_exotic), 
            Richness_exotic_mean = mean(Richness_exotic), 
            Richness_exotic_se = stdErr(Richness_exotic),
            Richness_native_mean = mean(Richness_native), 
            Richness_native_se = stdErr(Richness_native),
            Prop_exotic_mean = mean(Prop_exotic),
            Prop_exotic_se = stdErr(Prop_exotic),
            PctC_exotic_sig = NA, Richness_exotic_sig = NA, Prop_exotic_sig = NA)

#There are 20 unique exotics (all are found on FS land, haven't checked BAND)
unique_exotic <- 
  unique(na.exclude(d$Full_name[d$Origin=="exotic" & d$FS_BAND == "FS"]))
d_exotic <- 
  d[d$Full_name%in%unique_exotic & d$FS_BAND == "FS",]
d_exotic_stats <-
  d_exotic %>%
  group_by(Full_name) %>%
  summarize(max_PctC = max(PctC))
exotic_2main <- #Inclues BRIN, LOMU, and BRTE (which I remove below)
  c(d_exotic_stats[d_exotic_stats$max_PctC>50, "Full_name"]) [[1]]

d_exotic_2main <- 
  d_fs %>%
  group_by(Transect_id, year, RevBI,TRT) %>%
  summarise(brin = 0, lomu = 0, brte = 0) %>%
  melt(measure.vars = c("brin", "lomu", "brte"),
       variable.name = "Code", value.name = "ExtraCol") %>%
  merge(d_exotic %>%
          #filter(Full_name%in%c("Bromus inermis", "Lolium multiflorum")) %>%
          filter(Full_name%in%exotic_2main) %>%
          select(c("Transect_id", "year", "RevBI", "Code", "Full_name", "PctC")),
        by = c("Transect_id", "year", "RevBI", "Code"),
        all = TRUE)
d_exotic_2main$PctC[is.na(d_exotic_2main$PctC)] <- 0 #Add zeros
d_exotic_2main$Code = as.character(d_exotic_2main$Code) #Need for model code later

d_mean_exotic_2main <-
  d_exotic_2main %>%
  group_by(year, RevBI, TRT, Code) %>%
  summarise(PctC_mean = mean(PctC, na.rm = T),
            PctC_se = stdErr(PctC),
            PctC_sig = NA)
d_mean_exotic_2main$Code <- factor(d_mean_exotic_2main$Code, levels = c("brin", "lomu", "brte"))

####2. EDA####
#What does sampling scheme look like
t1 <-
  cbind(
    table(d_annual$Transect_id,d_annual$year),
    table(d_annual$Transect_id,d_annual$TRT),
    table(d_annual$Transect_id,d_annual$FS_BAND),
    table(d_annual$Transect_id,d_annual$RevBI)
  )
t2 <-
  cbind(
    table(d_annual$year,d_annual$TRT),
    table(d_annual$year,d_annual$FS_BAND),
    table(d_annual$year,d_annual$RevBI)
  )
#Bandelier plots sampled in 2002, FS plots sampled in 2008. Unbalanced sample.
#All Bandelier plots unseeded. Should probably be analyzed separately.
t2_fs <- 
  cbind(
    table(d_annual_fs$year,d_annual_fs$TRT),
    table(d_annual_fs$year,d_annual_fs$RevBI)
  )


####3. Analysis questions####
#Analyzing FS data only for 3.1 - 
#Assuming all plots burned at HS in Las Conchas. Need to check.
####3.1 Q1####
#How does exotic species richness change over time as a function of burn severity and seeding?

#Models
v = "Richness_native" #Can do "Richness_native" or "Richness_exotic"
for(r in 1:nrow(d_fs_mean)){
  if(d_fs_mean$RevBI[r]!="Unburned"){
    d_yr <- d_annual_fs[d_annual_fs$year == as.integer(d_fs_mean[r,"year"]) & 
                          d_annual_fs$RevBI == as.character(d_fs_mean[r,"RevBI"]),]
    d_fs_mean[r,paste(v,"sig",sep = "_")] = 
      summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr)))$coef[2,4]
    print(summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr))))
    if(d_fs_mean[r,"TRT"] == "Unseeded" & d_fs_mean[r,paste(v,"sig",sep = "_")] <0.05){
      d_fs_mean[r,paste(v,"symbol",sep = "_")] <- "*"
    } else d_fs_mean[r,paste(v,"symbol",sep = "_")] <- ""
  }
}

#Plot:
#pdf("./Figures/EDA/RichnessNative1.pdf",height = 4, width = 6)
ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=Richness_native_mean, col = TRT)) + 
  geom_line(aes(x=year, y=Richness_native_mean, col = TRT))+
  geom_errorbar(aes(x=year, ymin = Richness_native_mean - Richness_native_se, 
                    ymax = Richness_native_mean + Richness_native_se, 
                    col = TRT))+
  scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 4, label = Richness_native_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  #JTS check facet_grid error. Column missing?
  labs(col = "Treatment", y = "Native Richness") +
  theme(legend.position = c(0.8,0.8))
#dev.off()

####3.2 Q2####
#How does exotic species proportion change over time as a function of burn severity and seeding?
v = "Prop_exotic"
for(r in 1:nrow(d_fs_mean)){
  if(d_fs_mean$RevBI[r]!="Unburned"){
    d_yr <- d_annual_fs[d_annual_fs$year == as.integer(d_fs_mean[r,"year"]) & 
                          d_annual_fs$RevBI == as.character(d_fs_mean[r,"RevBI"]),]
    d_fs_mean[r,paste(v,"sig",sep = "_")] = 
      summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr)))$coef[2,4]
    if(d_fs_mean[r,"TRT"] == "Unseeded" & d_fs_mean[r,paste(v,"sig",sep = "_")]<0.05){
      d_fs_mean[r,paste(v,"symbol",sep = "_")] <- "*"
    } else d_fs_mean[r,paste(v,"symbol",sep = "_")] <- ""
  }
}

#Plot:
#pdf("./Figures/EDA/PropExotic1.pdf", height = 4, width = 6)
ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=Prop_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=Prop_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = Prop_exotic_mean - Prop_exotic_se, 
                    ymax = Prop_exotic_mean + Prop_exotic_se #, lty = TRT
                    ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  geom_text(aes(x = year, y = 0.5, label = Prop_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Proportion Exotic") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#dev.off()

####3.3 Q3####
#How does exotic species cover change over time as a function of burn severity and seeding?

#Linear models:
v = "PctC_exotic"
for(r in 1:nrow(d_fs_mean)){
  if(d_fs_mean$RevBI[r]!="Unburned"){
    d_yr <- d_annual_fs[d_annual_fs$year == as.integer(d_fs_mean[r,"year"]) & 
                          d_annual_fs$RevBI == as.character(d_fs_mean[r,"RevBI"]),]
    d_fs_mean[r,paste(v,"sig",sep = "_")] = 
      summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr)))$coef[2,4]
    if(d_fs_mean[r,"TRT"] == "Unseeded" & d_fs_mean[r,paste(v,"sig",sep = "_")]<0.05){
      d_fs_mean[r,paste(v,"symbol",sep = "_")] <- "*"
    } else d_fs_mean[r,paste(v,"symbol",sep = "_")] <- ""
  }
}

#Plot:
#pdf("./Figures/EDA/PctCoverExotic1.pdf", height = 4, width = 6)
ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=PctC_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                    ymax = PctC_exotic_mean + PctC_exotic_se #,lty = 1 
                    ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 55, label = PctC_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Exotic Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#dev.off()

####3.4 Q4####
#What are the dynamics of the invaders in the seed mix?
#Ppt said Bromus carinatus was exotic but USDA plants says native
#Focusing on Lolium perenne, Lolium multiflorum and Bromus inermis
#Story is that Loliums disappear but Bromus increases from very minimal amount
#Watch out for bromis contamination

#create exp(x)-1 transformation, the inverse of log(1+p)
#https://stackoverflow.com/questions/2777053/in-ggplot-restrict-y-to-be-0-in-loess/2782383#2782383
expm1_trans <-  function() trans_new("expm1", "expm1", "log1p")
#Need this for a plotting function below.

##2 main models
#Models
v = "PctC"
for(r in 1:nrow(d_mean_exotic_2main)){
  if(d_mean_exotic_2main$TRT[r]!="Unseeded"){
    d_yr <- d_exotic_2main[d_exotic_2main$year == as.integer(d_mean_exotic_2main[r,"year"]) & 
                             d_exotic_2main$RevBI == as.character(d_mean_exotic_2main[r,"RevBI"])& 
                             d_exotic_2main$Code == as.character(d_mean_exotic_2main[r,"Code"]),]
    #d_mean_exotic_2main[r,paste(v,"sig",sep = "_")] <- #not doing because three-way contrasts
    #  summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr)))$coef[2,4]
    if(is.na(d_mean_exotic_2main[r,paste(v,"sig",sep = "_")])){
      d_mean_exotic_2main[r,paste(v,"sig",sep = "_")] <- 1 
      #NA value comes from same numbers in seeded/unseeded so P = 1
    }
    #print(summary(do.call("lm", list(as.formula(paste (v, "~ TRT")), data = d_yr))))
    if(d_mean_exotic_2main[r,"TRT"] == "Seeded" & 
       d_mean_exotic_2main[r,paste(v,"sig",sep = "_")]<0.05){
         d_mean_exotic_2main[r,paste(v,"symbol",sep = "_")] <- "*"
       }else d_mean_exotic_2main[r,paste(v,"symbol",sep = "_")] <- ""
  }
}


#pdf("./Figures/EDA/Cover2Main_1.pdf",width = 6, height = 4)
ggplot(d_mean_exotic_2main) +
  geom_point(aes(x=year, y=PctC_mean, col = Code)) + 
  geom_line(aes(x=year, y=PctC_mean, col = Code, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_mean - PctC_se, 
                    ymax = PctC_mean + PctC_se, 
                    col = Code)) +
  #geom_text(aes(x = year, y = 60, label = PctC_symbol), size =12) +
  scale_color_manual(values = c("blue","darkred","magenta"),
                     labels = c("Bromus \ninermis","Lolium \nmultiflorum","Bromus \ntectorum"))+
  #scale_linetype_manual(values = c(1,2,0), 
  #                        labels = c("Bromus \ninermis", "Bromus \ntectorum","Lolium \nmultiflorum")) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 100, label = PctC_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", col = "Species", y = "Exotic Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.55))
#dev.off()

#pdf("./Figures/EDA/Cover2Main_1.pdf")
ggplot(d_mean_exotic_2main) +
  geom_point(aes(x = year, y = PctC_mean, col = Code, pch = TRT)) +
  geom_smooth(aes(x = year, y = PctC_mean, col = Code, lty = TRT)) + 
  scale_y_continuous(trans = "log1p") +
  coord_trans(y = expm1_trans())+
  facet_grid(cols = vars(RevBI)) + 
  theme_bw() +
  #lims(y = c(0,50)) +
  theme(legend.position = c(0.8,0.7))
#dev.off()



### TE edits (some basic exploration) - plots, no models ###

## How does native percent cover change over time? (perhaps alongside lolium/brin) 

## must add percent cover of natives to dataset

d_annual_2 <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(Transect_id, year) %>%
  summarise(
    RevBI = unique(RevBI),
    FS_BAND = unique(FS_BAND), 
    TRT = unique(TRT),
    PctC_exotic = sum(PctC[Origin == "exotic"], na.rm = T),
    PctC_lolium = sum(PctC[grep("Lolium",Full_name)], na.rm = T), #deprecate?
    PctC_brin = sum(PctC[grep("brin",Code)], na.rm = T), #deprecate?
    PctC_native = sum(PctC[Origin == "native"], na.rm = T), #add in native cover
    Richness_exotic = length(which(Origin=="exotic")),
    Richness_native = length(which(Origin=="native")),
    Prop_exotic = length(which(Origin=="exotic")) / 
      (length(which(Origin=="exotic")) + length(which(Origin=="native")))
    
  )
## adjust dataset to include FS land only and summarize by mean

d_annual_fs_2 <- d_annual_2[which(d_annual$FS_BAND == "FS"),]
d_fs_mean_2 <- d_annual_fs_2 %>%
  group_by(year,RevBI,TRT) %>%
  summarise(PctC_exotic_mean = mean(PctC_exotic), 
            PctC_exotic_se = stdErr(PctC_exotic), 
            PctC_native_mean = mean(PctC_native),
            PctC_native_se = stdErr(PctC_native),
            Richness_exotic_mean = mean(Richness_exotic), 
            Richness_exotic_se = stdErr(Richness_exotic),
            Richness_native_mean = mean(Richness_native), 
            Richness_native_se = stdErr(Richness_native),
            Prop_exotic_mean = mean(Prop_exotic),
            Prop_exotic_se = stdErr(Prop_exotic),
            PctC_exotic_sig = NA, Richness_exotic_sig = NA, Prop_exotic_sig = NA)

## add in graph of native cover over time
#Plot:
ggplot(d_fs_mean_2) +
  geom_point(aes(x=year, y=PctC_native_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_native_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_native_mean - PctC_native_se, 
                    ymax = PctC_native_mean + PctC_native_se #,lty = 1 
  ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 55, label = PctC_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Native Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#dev.off()
#!! native PC up in 2019 - exotic cover down in some instances so they may be outcompeting
#!! but 2019 had many more natives ID'd, so that may affect PC
#!! cover in general is increasing post 2011 fire


## plot native richness over time 

ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=Richness_native_mean, col = TRT)) + 
  geom_line(aes(x=year, y=Richness_native_mean, col = TRT))+
  geom_errorbar(aes(x=year, ymin = Richness_native_mean - Richness_native_se, 
                    ymax = Richness_native_mean + Richness_native_se, 
                    col = TRT))+
  scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 4, label = Richness_native_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(col = "Treatment", y = "Native Richness") +
  theme(legend.position = c(0.8,0.8))
#!! increase of native richness in 2019 - we counted many more natives in the field
#!! are there more species coming up post-fire? post a good precip year??
#!! OR did the crew in 2015 just have more unknown natives!
#!! BIG question


## plot lolium cover on native graph

d_mean_exotic_2main %>%
  filter(Code == "lomu") %>%
  ggplot() +
  geom_point(aes(x=year, y=PctC_mean, col = Code)) + 
  geom_line(aes(x=year, y=PctC_mean, col = Code, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_mean - PctC_se, 
                    ymax = PctC_mean + PctC_se, 
                    col = Code)) + 
  geom_point(data = d_fs_mean_2, aes(x=year, y=PctC_native_mean, lty = TRT)) + 
  geom_line(data = d_fs_mean_2, aes(x=year, y=PctC_native_mean, lty = TRT))+
  geom_errorbar(data = d_fs_mean_2, aes(x=year, ymin = PctC_native_mean - PctC_native_se, 
                    ymax = PctC_native_mean + PctC_native_se #,lty = 1 
  ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 55, label = PctC_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Native Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#!!! lolium cover decreases to 0 through the decade, was higher when native cover low

## plot brin cover on native graph
d_mean_exotic_2main %>%
  filter(Code == "brin") %>%
  ggplot() +
  geom_point(aes(x=year, y=PctC_mean, col = Code)) + 
  geom_line(aes(x=year, y=PctC_mean, col = Code, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_mean - PctC_se, 
                    ymax = PctC_mean + PctC_se, 
                    col = Code)) + 
  geom_point(data = d_fs_mean_2, aes(x=year, y=PctC_native_mean, lty = TRT)) + 
  geom_line(data = d_fs_mean_2, aes(x=year, y=PctC_native_mean, lty = TRT))+
  geom_errorbar(data = d_fs_mean_2, aes(x=year, ymin = PctC_native_mean - PctC_native_se, 
                                        ymax = PctC_native_mean + PctC_native_se #,lty = 1 
  ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 55, label = PctC_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Native Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#!!! not much info given, overall cover for natives and brin increases in 2007
#!!! decreases post 2011 fire 


### How has oak/locust cover changed over time (alongside exotic cover)?
### data visualization (no models)

# generate dataset with mean percent cover of oak and locust

## get oak/locust cover per transect per year (forest service land only)
d_native <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(Transect_id, year) %>%
  filter(FS_BAND == "FS") %>%
  summarise(
    RevBI = unique(RevBI),
    FS_BAND = unique(FS_BAND), 
    TRT = unique(TRT),
    PctC_quercus = sum(PctC[grep("quga",Code)], na.rm = T), 
    PctC_robinia = sum(PctC[grep("rone",Code)], na.rm = T)) 

##average oak/locust cover per year 

d_native_mean <- d_native %>%
  group_by(year,RevBI,TRT) %>%
  summarise(PctC_quercus_mean = mean(PctC_quercus), 
            PctC_quercus_se = stdErr(PctC_quercus), 
            PctC_robinia_mean = mean(PctC_robinia),
            PctC_robinia_se = stdErr(PctC_robinia))
    
# plot exotic percent cover with Quercus gambellii, all burn plots

ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=PctC_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                                        ymax = PctC_exotic_mean + PctC_exotic_se)) + 
  geom_point(data = d_native_mean, aes(x=year, y=PctC_quercus_mean, color = 'Q. gambellii percent cover')) + 
  geom_line(data = d_native_mean, aes(x=year, y=PctC_quercus_mean,lty = TRT, color = 'Q. gambellii percent cover'))+
  geom_errorbar(data = d_native_mean, aes(x=year, ymin = PctC_quercus_mean - PctC_quercus_se, 
                    ymax = PctC_quercus_mean + PctC_quercus_se),color = "salmon2") + 
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Exotic Percent Cover", col = "") +
  theme_bw() +
  theme(legend.position = c(0.2,0.8))

#!!! Q. agrifolia average cover is highest in high severity burn plots
#!!! increasing cover across all plots by 2019

# plot exotic percent cover with Quercus gambellii, high severity plots only

# make dataframe with high severity quercus (and robinia) cover

d_native_high <- d_native_mean %>%
  filter(RevBI == "High")

d_fs_mean %>%
filter(RevBI == "High") %>%
ggplot() +
  geom_point(aes(x=year, y=PctC_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                                      ymax = PctC_exotic_mean + PctC_exotic_se)) + 
  geom_point(dat = d_native_high, aes(x=year, y=PctC_quercus_mean, color = 'Q. gambellii percent cover')) + 
  geom_line(dat = d_native_high, aes(x=year, y=PctC_quercus_mean,lty = TRT,  color = 'Q. gambellii percent cover'))+
  geom_errorbar(dat = d_native_high, aes(x=year, ymin = PctC_quercus_mean - PctC_quercus_se, 
                                          ymax = PctC_quercus_mean + PctC_quercus_se), color = "salmon2") + 
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  labs(lty = "Treatment", y = "Exotic Percent Cover", color = "") +
  theme_bw() +
  theme(legend.position = c(0.25,0.8))
#!! Q. gambellii cover increased in seeded plots from 2016 to 2019 , while exotic cover decreased
#!! in seeded plots 

# plot exotic percent cover with R. neomexicana, all burn plots

ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=PctC_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                    ymax = PctC_exotic_mean + PctC_exotic_se)) + 
  geom_point(data = d_native_mean, aes(x=year, y=PctC_robinia_mean, color = 'R. neomexicana\npercent cover')) + 
  geom_line(data = d_native_mean, aes(x=year, y = PctC_robinia_mean,lty = TRT, color = 'R. neomexicana\npercent cover'))+
  geom_errorbar(data = d_native_mean, aes(x=year, ymin = PctC_robinia_mean - PctC_robinia_se, 
                                          ymax = PctC_robinia_mean + PctC_robinia_se), color = "salmon2") + 
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Exotic Percent Cover", col = "") +
  theme_bw() +
  theme(legend.position = c(0.85,0.8))

#!!! Robinia cover increasing most in high & mod severity plots

# plot exotic percent cover with robinia neomexicana, high severity burn plots only 

d_fs_mean %>%
  filter(RevBI == "High") %>%
  ggplot() +
  geom_point(aes(x=year, y=PctC_exotic_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                    ymax = PctC_exotic_mean + PctC_exotic_se)) + 
  geom_point(dat = d_native_high, aes(x=year, y=PctC_robinia_mean, color = 'R. neomexicana\npercent cover')) + 
  geom_line(dat = d_native_high, aes(x=year, y=PctC_robinia_mean,lty = TRT,  color = 'R. neomexicana\npercent cover'))+
  geom_errorbar(dat = d_native_high, aes(x=year, ymin = PctC_robinia_mean - PctC_robinia_se, 
                                         ymax = PctC_robinia_mean + PctC_robinia_se), color = "salmon3") + 
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  labs(lty = "Treatment", y = "Exotic Percent Cover", color = "") +
  theme_bw() +
  theme(legend.position = c(0.25,0.80)) 

#!!! Average robinia cover incresed slightly from 2016-2019, as exotic cover dropped  
