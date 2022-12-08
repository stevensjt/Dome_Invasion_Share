
##Process Species Attributes

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(splines)
library(scales)
library(data.table)
library(glmmTMB)
library(MuMIn)



stdErr <- function(x) sqrt(var(x, na.rm = T)/length(na.exclude(x)))

dates<-read.csv("Data/visit_dates.csv")

dates

days<-merge(d_annual,dates,by=c("Transect_id","year"))


####1. Data input and processing####

d <- read.csv("Data/Raw/All Years 1997-2019 PermPlot-Entire-APW.csv")
d
d$Transect_id <- toupper(d$Transect_id) #Uppercase for consistency
#d <- d[-which(d$Transect_id=="3MBU2A"),] #Remove transect only sampled in one yr.
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3LAU1)
#d[which(d$Transect_id=="3LAU1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"
#Investigate multiple burn severity issue:

#deprecated, d2 is not referenced anywhere below, subsequent d data table is primary source
d2 <- #Merge live and dead cover of a single species on a single transect|year
  d %>%
  group_by(Transect_id, year, Code) %>%
  summarise(
    RevBI = unique(RevBI), FS_BAND = unique(FS_BAND), TRT = unique(TRT),
    Full_name = unique(Full_name),
    Status = paste(Status, collapse = ''),
    Growth_form = unique(Growth_form), Origin = unique(Origin),
    Basal_cm = sum(Basal_cm), Canopy_cm = sum(Canopy_cm), 
    Total_cm = sum(Total_cm), Transect_cm = unique(Transect_cm),
    PctC_basal=(sum(Basal_cm))/(Transect_cm)*100,
    PctC = sum(PctC)#, PctB = sum(PctB) #Last one deprecated in 2016 data
    
  )

d<-as.data.table(d)
d<-d[,.(PctC=sum(PctC), PctC_basal=(sum(Basal_cm))/(Transect_cm)*100, Total_cm = sum(Total_cm), Basal_cm = sum(Basal_cm), Canopy_cm=sum(Canopy_cm)),by=c("Transect_id","year","Code","RevBI","FS_BAND","TRT","Full_name","Origin","Transect_cm","Growth_form")]

dtest<-d[,.(grandsum=sum(PctC_basal)),by=c("Transect_id","year")]

#rounding errors result in Pct greater than 100 in 1 Brin plot

d$PctC<-ifelse(d$PctC>100,100,d$PctC)

d_fs <- d[which(d$FS_BAND == "FS"),]
d_band<- d[which(d$FS_BAND == "BAND"),]



d_annual <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(Transect_id, year) %>%
  summarise(
    RevBI = unique(RevBI),
    FS_BAND = unique(FS_BAND), 
    TRT = unique(TRT),
    PctC_exotic = sum(PctC_basal[Origin == "exotic"], na.rm = T),
    PctC_lolium = sum(PctC_basal[grep("Lolium",Full_name)], na.rm = T), #deprecate?
    PctC_brin = sum(PctC_basal[grep("brin",Code)], na.rm = T), #deprecate?
    PctC_brte = sum(PctC_basal[grep("brte",Code)], na.rm = T), #deprecate?
    PctC_shrub = sum(PctC_basal[grep("S",Growth_form)], na.rm =T),
    PctC_oak =  sum(PctC_basal[grep("qu",Code)], na.rm = T),
    PctC_rone =  sum(PctC_basal[grep("rone",Code)], na.rm = T),
    PctC_rone =  sum(PctC_basal[grep("rone",Code)], na.rm = T),
    PctC_bare = sum(Total_cm[grep("B",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_litter = sum(Total_cm[grep("L",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_unk = sum(Total_cm[grep("Unidentified",Full_name)], na.rm =T)/mean(Transect_cm),
    Richness_exotic = length(which(Origin=="exotic")),
    Richness_native = length(which(Origin=="native")),
    Prop_exotic = length(which(Origin=="exotic")) / 
      (length(which(Origin=="exotic")) + length(which(Origin=="native")))
    
  )


unique(d$Code)
#rounding errors result in Pct greater than 100 for two lolium spp.  doing it again here now that they're combined
d_annual$PctC_lolium<-ifelse(d_annual$PctC_lolium>100,100,d_annual$PctC_lolium)
d_annual$PctC_exotic<-ifelse(d_annual$PctC_exotic>100,100,d_annual$PctC_exotic)


d_annual_fs <- d_annual[which(d_annual$FS_BAND == "FS"),]
d_annual_band <- d_annual[which(d_annual$FS_BAND == "BAND"),]


d_fs_mean <- d_annual %>%
  group_by(year,RevBI,TRT) %>%
  summarise(FS_BAND = unique(FS_BAND),
            PctC_exotic_mean = mean(PctC_exotic), 
            PctC_exotic_se = stdErr(PctC_exotic), 
            PctC_shrub_mean = mean(PctC_shrub),
            PctC_shrub_se = stdErr(PctC_shrub),
            PctC_oak_mean = mean(PctC_oak),
            PctC_oak_se = stdErr(PctC_oak),
            PctC_rone_mean = mean(PctC_rone),
            PctC_rone_se = stdErr(PctC_rone),
            PctC_bare_mean = mean(PctC_bare),
            PctC_bare_se = stdErr(PctC_bare),
            Richness_exotic_mean = mean(Richness_exotic), 
            Richness_exotic_se = stdErr(Richness_exotic),
            Richness_native_mean = mean(Richness_native), 
            Richness_native_se = stdErr(Richness_native),
            Prop_exotic_mean = mean(Prop_exotic),
            Prop_exotic_se = stdErr(Prop_exotic),
            PctC_exotic_sig = NA, Richness_exotic_sig = NA, Prop_exotic_sig = NA)



d_band_mean <- d_annual_band %>%
  group_by(year,RevBI,TRT) %>%
  summarise(PctC_exotic_mean = mean(PctC_exotic), 
            PctC_exotic_se = stdErr(PctC_exotic), 
            PctC_shrub_mean = mean(PctC_shrub),
            PctC_shrub_se = stdErr(PctC_shrub),
            PctC_oak_mean = mean(PctC_oak),
            PctC_oak_se = stdErr(PctC_oak),
            PctC_rone_mean = mean(PctC_rone),
            PctC_rone_se = stdErr(PctC_rone),
            Richness_exotic_mean = mean(Richness_exotic), 
            Richness_exotic_se = stdErr(Richness_exotic),
            Richness_native_mean = mean(Richness_native), 
            Richness_native_se = stdErr(Richness_native),
            Prop_exotic_mean = mean(Prop_exotic),
            Prop_exotic_se = stdErr(Prop_exotic),
            PctC_exotic_sig = NA, Richness_exotic_sig = NA, Prop_exotic_sig = NA)




#There are 20 unique exotics (all are found on FS land, haven't checked BAND)
unique_exotic <- 
  unique(na.exclude(d$Full_name[d$Origin=="exotic"]))
d_exotic <- 
  d[d$Full_name%in%unique_exotic,]
d_exotic_stats <-
  d_exotic %>%
  group_by(Full_name) %>%
  summarize(max_PctC = max(PctC))
exotic_2main <- #Inclues BRIN, LOMU, and BRTE (which I remove below)
  c(d_exotic_stats[d_exotic_stats$max_PctC>50, "Full_name"]) [[1]]



#Checking out BAND...12 exotic species
unique_exotic_band <- 
  unique(na.exclude(d$Full_name[d$Origin=="exotic" & d$FS_BAND == "BAND"]))
d_exotic_band <- 
  d[d$Full_name%in%unique_exotic_band & d$FS_BAND == "BAND",]
d_exotic_band_stats <-
  d_exotic_band %>%
  group_by(Full_name) %>%
  summarize(max_PctC = max(PctC))
exotic_2main_band <- #Inclues BRIN, LOMU, and BRTE (which I remove below)
  c(d_exotic_band_stats[d_exotic_band_stats$max_PctC>50, "Full_name"]) [[1]]



d_exotic_2main <- 
  d %>%
  group_by(Transect_id, year, RevBI,TRT) %>%
  summarise(brin = 0, lomu = 0, brte = 0) %>%
  melt(measure.vars = c("brin", "lomu", "brte"),
       variable.name = "Code", value.name = "ExtraCol") %>%
  merge(d_exotic %>%
          #filter(Full_name%in%c("Bromus inermis", "Lolium multiflorum")) %>%
          filter(Full_name%in%exotic_2main) %>%
          dplyr::select(c("Transect_id", "year", "RevBI", "Code", "Full_name", "PctC")),
        by = c("Transect_id", "year", "RevBI", "Code"),
        all = TRUE)

d_exotic_2main_band <- 
  d_band %>%
  group_by(Transect_id, year, RevBI,TRT) %>%
  summarise(brin = 0, lomu = 0, brte = 0) %>%
  melt(measure.vars = c("brin", "lomu", "brte"),
       variable.name = "Code", value.name = "ExtraCol") %>%
  merge(d_exotic_band %>%
          #filter(Full_name%in%c("Bromus inermis", "Lolium multiflorum")) %>%
          filter(Full_name%in%exotic_2main) %>%
          dplyr::select(c("Transect_id", "year", "RevBI", "Code", "Full_name", "PctC")),
        by = c("Transect_id", "year", "RevBI", "Code"),
        all = TRUE)

d_exotic_2main$PctC[is.na(d_exotic_2main$PctC)] <- 0 #Add zeros
d_exotic_2main$Code = as.character(d_exotic_2main$Code) #Need for model code later


d_exotic_2main_band$PctC[is.na(d_exotic_2main_band$PctC)] <- 0 #Add zeros
d_exotic_2main_band$Code = as.character(d_exotic_2main_band$Code) #Need for model code later

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




#species richness models

#run file dome_covariates to merge here
d_annual<-as.data.table(d_annual)
full_d<-merge(d_annual,yo,by=c("Transect_id","year"))

str(d_annual)
str(yo)
full_d$RevBI <- factor(full_d$RevBI, levels = c("Unburned", "Mod", "High"))


exotic_rich_mod<-glmmTMB(Richness_exotic ~ TRT + RevBI + cwd + winter_ppt + (1|Transect_id), data=full_d)
summary(exotic_rich_mod)
r.squaredGLMM(exotic_rich_mod)

native_rich_mod<-glmmTMB(Richness_native ~ TRT + RevBI + cwd + winter_ppt + (1|Transect_id), data=full_d)
summary(native_rich_mod)
r.squaredGLMM(native_rich_mod)



geom_line

#Jens figs

#Plot:
#pdf("./Figures/EDA/RichnessNative1.pdf",height = 4, width = 6)
ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=Richness_native_mean, lty = TRT,col=TRT)) + 
  geom_line(aes(x=year, y=Richness_native_mean, lty = TRT,col=TRT))+
  geom_errorbar(aes(x=year, ymin = Richness_native_mean - Richness_native_se, 
                    ymax = Richness_native_mean + Richness_native_se, 
                    lty = TRT, col=TRT))+
  scale_color_manual(values = c("blue","brown3"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 4, label = Richness_native_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty="Treatment", y = "Native Richness") +
  theme(legend.position = c(0.8,0.8))+
  theme_bw()
#dev.off()


ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=Richness_exotic_mean, lty = TRT,col=TRT)) + 
  geom_line(aes(x=year, y=Richness_exotic_mean, lty = TRT,col=TRT))+
  geom_errorbar(aes(x=year, ymin = Richness_exotic_mean - Richness_exotic_se, 
                    ymax = Richness_exotic_mean + Richness_exotic_se, 
                    lty = TRT,col=TRT))+
  scale_color_manual(values = c("blue","brown3"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 4, label = Richness_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty="Treatment", y = "Exotic Richness") +
  theme(legend.position = c(0.8,0.8))+
  theme_bw()

#dev.off()

ggplot(d_annual_fs) + #Old, deprecate eventually
  geom_smooth(aes(x = year, y = Richness_exotic, col = TRT)) + 
  geom_vline(aes(xintercept = 1996), lty = 2) + 
  geom_vline(aes(xintercept = 2011), lty = 2) +
  geom_jitter(aes(x = year, y = Richness_exotic, col = TRT), width = 0.2) +
  theme_bw()




##Proportion of exotics at plot - models and figs

prop_exotic_mod<-glmmTMB(Prop_exotic ~ TRT + RevBI + cwd + winter_ppt + (1|Transect_id), data= full_d, family="binomial")
summary(prop_exotic_mod)
r.squaredGLMM(prop_exotic_mod)

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
  #geom_text(aes(x = year, y = 0.5, label = Prop_exotic_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Proportion Exotic") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#dev.off()

cor.test(d_fs_mean$Prop_exotic_mean,d_fs_mean$PctC_exotic_mean)

#Exotic species percent cover over time - models and figs


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


ggplot(d_fs_mean) +
  geom_point(aes(x=year, y=PctC_bare_mean, lty = TRT)) + 
  geom_line(aes(x=year, y=PctC_bare_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se #,lty = 1 
  ))+
  #scale_color_manual(values = c("blue","darkred"))+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #geom_text(aes(x = year, y = 55, label = PctC_bare_symbol), size =12) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "bare Percent Cover") +
  theme_bw() +
  theme(legend.position = c(0.8,0.8))
#dev.off()

#Percent cover must be constrained between 0-1 for models
full_d$PctC_exotic<-full_d$PctC_exotic/100
full_d$PctC_lolium<-full_d$PctC_lolium/100
full_d$PctC_brin<-full_d$PctC_brin/100
full_d$PctC_brte<-full_d$PctC_brte/100
full_d$PctC_shrub<-full_d$PctC_shrub/100
full_d$PctC_oak<-full_d$PctC_oak/100
full_d$PctC_rone<-full_d$PctC_rone/100


d_16<-subset(d,year==2016)
unique(d_16$Full_name)

pct_mod<-glmmTMB(PctC_exotic ~ TRT + RevBI + scale(cwd)  + winter_ppt+ (1|Transect_id), data = full_d, family = "binomial" )
r.squaredGLMM(pct_mod)
summary(pct_mod)

pct_shrub_mod<-glmmTMB(PctC_shrub ~ TRT + RevBI + scale(cwd)  + summer_ppt + (1|Transect_id), data = full_d, family = "binomial" )
r.squaredGLMM(pct_shrub_mod)
summary(pct_shrub_mod)

#plot(full_d$PctC_shrub,(1-full_d$PctC_exotic))

#Exotic spp percent cover by species


#create exp(x)-1 transformation, the inverse of log(1+p)
#https://stackoverflow.com/questions/2777053/in-ggplot-restrict-y-to-be-0-in-loess/2782383#2782383
expm1_trans <-  function() trans_new("expm1", "expm1", "log1p")
#Need this for a plotting function below.


brin_pct<-glmmTMB(PctC_brin ~ TRT + RevBI + cwd  + winter_ppt + (1|Transect_id), data=full_d, family="binomial")
summary(brin_pct)
r.squaredGLMM(brin_pct)

lol_pct<-glmmTMB(PctC_lolium ~ TRT + RevBI + cwd + summer_ppt + (1|Transect_id) , data=full_d, family="binomial")
summary(lol_pct)
r.squaredGLMM(lol_pct)

brte_pct<-glmmTMB(PctC_brte ~ TRT + RevBI + cwd  + summer_ppt + (1|Transect_id) , data=full_d, family="binomial")
summary(brte_pct)
r.squaredGLMM(brte_pct)

oak_pct<-glmmTMB(PctC_oak ~ TRT + RevBI + cwd + winter_vpd + (1|Transect_id), data=full_d, family="binomial")
summary(oak_pct)
r.squaredGLMM(oak_pct)


rone_pct<-glmmTMB(PctC_rone ~ TRT + RevBI + cwd + summer_ppt + (1|Transect_id), data=full_d, family="binomial")
summary(rone_pct)
r.squaredGLMM(oak_pct)





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


ggplot(d_fs_mean)+
  geom_line(mapping = aes(x=year,y=PctC_shrub_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_shrub_mean - PctC_shrub_se, 
                    ymax = PctC_shrub_mean + PctC_oak_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "All shrub Percent Cover") +
  theme_bw() 

ggplot(d_fs_mean)+
  geom_line(mapping = aes(x=year,y=PctC_oak_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_oak_mean - PctC_oak_se, 
                    ymax = PctC_oak_mean + PctC_oak_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Oak Percent Cover") +
  theme_bw() 
  
ggplot(d_fs_mean)+
  geom_line(mapping = aes(x=year,y=PctC_rone_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_rone_mean - PctC_rone_se, 
                    ymax = PctC_rone_mean + PctC_rone_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Robinia Percent Cover") +
  theme_bw() 


cor.test(full_d$summer_ppt,full_d$winter_ppt)

