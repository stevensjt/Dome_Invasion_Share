
##Process Species Attributes

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(splines)
library(scales)
stdErr <- function(x) sqrt(var(x, na.rm = T)/length(na.exclude(x)))

####1. Data input and processing####
d_old <- read_excel("./Data/dome_invasion_data.xlsx", sheet = "data") #Old data through 2013
d_old$Origin[d_old$Origin=="NA"] <- NA #Recode NA from text to NA
exotics <- unique(d_old$Code[which(d_old$Origin=="exotic")])
natives <- unique(d_old$Code[which(d_old$Origin=="native")])
#unk <- unique(d_old$Code[which(is.na(d_old$Origin))])
#unk_full <- unique(d_old$Full_name[which(is.na(d_old$Origin))])
d <- read_excel("./Data/All Years 1997-2016 PermPlot-Entire.xlsx", sheet = "97-2016 EX1")
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
    if(d_fs_mean[r,"TRT"] == "Unseeded" & d_fs_mean[r,paste(v,"sig",sep = "_")]<0.05){
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
  labs(col = "Treatment", y = "Native Richness") +
  theme(legend.position = c(0.8,0.8))
#dev.off()

ggplot(d_annual_fs) + #Old, deprecate eventually
  geom_smooth(aes(x = year, y = Richness_exotic, col = TRT)) + 
  geom_vline(aes(xintercept = 1996), lty = 2) + 
  geom_vline(aes(xintercept = 2011), lty = 2) +
  geom_jitter(aes(x = year, y = Richness_exotic, col = TRT), width = 0.2) +
  theme_bw()

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

