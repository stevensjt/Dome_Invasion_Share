
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

#create exp(x)-1 transformation, the inverse of log(1+p)
#https://stackoverflow.com/questions/2777053/in-ggplot-restrict-y-to-be-0-in-loess/2782383#2782383
expm1_trans <-  function() trans_new("expm1", "expm1", "log1p")
#Need this for a plotting function below.

#sampling dates
dates<-read.csv("Data/visit_dates.csv")

dates



####1. Data input and processing####

d <- read.csv("Data/Raw/All Years 1997-2019 PermPlot-Entire-APW.csv")
d
d$Transect_id <- toupper(d$Transect_id) #Uppercase for consistency
#d <- d[-which(d$Transect_id=="3MBU2A"),] #Remove transect only sampled in one yr.
#d <- d[-which(d$RevBI=="Low"),] #Remove the only transect in low severity (N=1; 3LAU1)
#d[which(d$Transect_id=="3LAU1"),"RevBI"] <- "Unburned" #Alternatively, lump in with "unburned"
#Investigate multiple burn severity issue:

yo<-read.csv("covariates.csv")



d<-as.data.table(d)
d<-d[,.(PctC=sum(PctC), PctC_basal=(sum(Basal_cm))/(Transect_cm), Total_cm = sum(Total_cm), Basal_cm = sum(Basal_cm), Canopy_cm=sum(Canopy_cm)),by=c("Transect_id","year","Code","RevBI","FS_BAND","TRT","Full_name","Origin","Transect_cm","Growth_form")]


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
    PctC_exotic = sum(Canopy_cm[Origin == "exotic"], na.rm = T)/mean(Transect_cm),
    PctC_lolium = sum(Canopy_cm[grep("Lolium",Full_name)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brin = sum(Canopy_cm[grep("brin",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brte = sum(Canopy_cm[grep("brte",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brca = sum(Canopy_cm[grep("brca",Code)], na.rm = T)/mean(Transect_cm),
    PctC_eltr = sum(Canopy_cm[grep("eltr",Code)], na.rm = T)/mean(Transect_cm),
    
    PctC_shrub = sum(Canopy_cm[grep("S",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_oak =  sum(Canopy_cm[grep("qu",Code)], na.rm = T)/mean(Transect_cm),
    PctC_rone =  sum(Canopy_cm[grep("rone",Code)], na.rm = T)/mean(Transect_cm),
    
    PctC_gram = sum(Canopy_cm[grep("G",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_forb = sum(Canopy_cm[grep("F",Growth_form)], na.rm =T)/mean(Transect_cm),
    
    PctC_shit = sum(Total_cm[grep("Animal feces",Full_name)], na.rm = T)/mean(Transect_cm),
    PctC_bare = sum(Total_cm[grep("Bare",Full_name)], na.rm =T)/mean(Transect_cm),
    PctC_litter = sum(Basal_cm[grep("L",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_logs = sum(Total_cm[grep("Wood",Full_name)], na.rm =T)/mean(Transect_cm),
    PctC_unk = sum(Total_cm[grep("Unidentified",Full_name)], na.rm =T)/mean(Transect_cm),
    
    total_pct = sum(PctC, na.rm=T),
    
    Richness_exotic = length(which(Origin=="exotic")),
    Richness_native = length(which(Origin=="native")),
    Prop_exotic = length(which(Origin=="exotic")) / 
      (length(which(Origin=="exotic")) + length(which(Origin=="native")))
    
  )


d_annual$PctC_exotic<-ifelse(d_annual$PctC_exotic>1,1,d_annual$PctC_exotic)
d_annual$PctC_lolium<-ifelse(d_annual$PctC_lolium>1,1,d_annual$PctC_lolium)
d_annual$PctC_brin<-ifelse(d_annual$PctC_brin>1,1,d_annual$PctC_brin)
d_annual$PctC_brte<-ifelse(d_annual$PctC_brte>1,1,d_annual$PctC_brte)
d_annual$PctC_gram<-ifelse(d_annual$PctC_gram>1,1,d_annual$PctC_gram)


unique(d$Code)
#rounding errors result in Pct greater than 100 for two lolium spp.  doing it again here now that they're combined
d_annual$PctC_lolium<-ifelse(d_annual$PctC_lolium>100,100,d_annual$PctC_lolium)
d_annual$PctC_exotic<-ifelse(d_annual$PctC_exotic>100,100,d_annual$PctC_exotic)


d_annual_fs <- d_annual[which(d_annual$FS_BAND == "FS"),]
d_annual_band <- d_annual[which(d_annual$FS_BAND == "BAND"),]


d_mean <- d_annual_fs %>%
  group_by(year,RevBI,TRT) %>%
  summarise(FS_BAND = unique(FS_BAND),
            PctC_exotic_mean = mean(PctC_exotic), 
            PctC_exotic_se = stdErr(PctC_exotic), 
            
            PctC_brin_mean = mean(PctC_brin), 
            PctC_brin_se = stdErr(PctC_brin), 
            
            PctC_brte_mean = mean(PctC_brte), 
            PctC_brte_se = stdErr(PctC_brte), 
            
            PctC_lolium_mean = mean(PctC_lolium), 
            PctC_lolium_se = stdErr(PctC_lolium),
            
            PctC_brca_mean = mean(PctC_brca), 
            PctC_brca_se = stdErr(PctC_brca), 
            
            PctC_eltr_mean = mean(PctC_eltr), 
            PctC_eltr_se = stdErr(PctC_eltr), 
            
            PctC_gram_mean = mean(PctC_gram), 
            PctC_gram_se = stdErr(PctC_gram), 
            
            PctC_forb_mean = mean(PctC_forb), 
            PctC_forb_se = stdErr(PctC_forb), 
            
            PctC_shrub_mean = mean(PctC_shrub),
            PctC_shrub_se = stdErr(PctC_shrub),
            
            PctC_oak_mean = mean(PctC_oak),
            PctC_oak_se = stdErr(PctC_oak),
            
            PctC_rone_mean = mean(PctC_rone),
            PctC_rone_se = stdErr(PctC_rone),
            
            PctC_shit_mean = mean(PctC_shit), 
            PctC_shit_se = stdErr(PctC_shit), 
            
            PctC_bare_mean = mean(PctC_bare),
            PctC_bare_se = stdErr(PctC_bare),
            
            PctC_logs_mean = mean(PctC_logs),
            PctC_logs_se = stdErr(PctC_logs),
            
            PctC_litter_mean = mean(PctC_litter),
            PctC_litter_se = stdErr(PctC_litter),
            
            PctC_unk_mean = mean(PctC_unk),
            PctC_unk_se = stdErr(PctC_unk),
            
            Richness_exotic_mean = mean(Richness_exotic), 
            Richness_exotic_se = stdErr(Richness_exotic),
            
            Richness_native_mean = mean(Richness_native), 
            Richness_native_se = stdErr(Richness_native),
            
            Prop_exotic_mean = mean(Prop_exotic),
            Prop_exotic_se = stdErr(Prop_exotic),
            
            )



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


#species richness models

#run file dome_covariates to merge here
d_annual<-as.data.table(d_annual)
d_ann<-merge(dates,d_annual)

full_d<-merge(d_ann,yo,by=c("Transect_id","year"))



pct_mod<-glmmTMB(PctC_exotic ~ TRT + RevBI + elevation  + spei + (1|Transect_id), data = full_d, family = "binomial" )
r.squaredGLMM(pct_mod)
summary(pct_mod)


brin_pct<-glmmTMB(PctC_brin ~ TRT + RevBI + elevation  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(brin_pct)
r.squaredGLMM(brin_pct)

lol_pct<-glmmTMB(PctC_lolium ~ TRT + RevBI + cwd  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(lol_pct)
r.squaredGLMM(lol_pct)

brte_pct<-glmmTMB(PctC_brte ~ TRT + RevBI + cwd  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(brte_pct)
r.squaredGLMM(brte_pct)

brca_pct<-glmmTMB(PctC_brca ~ TRT + RevBI + cwd  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(brca_pct)
r.squaredGLMM(brca_pct)

eltr_pct<-glmmTMB(PctC_eltr ~ TRT + RevBI + cwd  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(eltr_pct)
r.squaredGLMM(eltr_pct)

bare_pct<-glmmTMB(PctC_bare ~ TRT + RevBI + cwd   + spei + (1|Transect_id), data=full_d, family="binomial")
summary(bare_pct)
r.squaredGLMM(bare_pct)

gram_pct<-glmmTMB(PctC_gram ~ TRT + RevBI + cwd  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(gram_pct)
r.squaredGLMM(gram_pct)

forb_pct<-glmmTMB(PctC_forb ~ TRT + RevBI + cwd  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(forb_pct)
r.squaredGLMM(forb_pct)









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



exotic_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_exotic_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                    ymax = PctC_exotic_mean + PctC_exotic_se, col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "All exotic Percent Cover") +
  theme_bw() 


bare_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_bare_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Bare Percent Cover") +
  theme_bw() 

gram_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_gram_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_gram_mean - PctC_gram_se, 
                    ymax = PctC_gram_mean + PctC_gram_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "gram Percent Cover") +
  theme_bw() 

forb_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_forb_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_forb_mean - PctC_forb_se, 
                    ymax = PctC_forb_mean + PctC_forb_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "forb Percent Cover") +
  theme_bw() 

unk_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_unk_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_unk_mean - PctC_unk_se, 
                    ymax = PctC_unk_mean + PctC_unk_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "Unknown Percent Cover") +
  theme_bw() 


lit_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_litter_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_litter_mean - PctC_litter_se, 
                    ymax = PctC_litter_mean + PctC_litter_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "litter Percent Cover") +
  theme_bw() 

wood_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_logs_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_logs_mean - PctC_logs_se, 
                    ymax = PctC_logs_mean + PctC_logs_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "wood Percent Cover") +
  theme_bw() 

shit_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_shit_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_shit_mean - PctC_shit_se, 
                    ymax = PctC_shit_mean + PctC_shit_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "shit Percent Cover") +
  theme_bw() 


brca_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_brca_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "brca Percent Cover") +
  theme_bw() 

eltr_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_eltr_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "eltr Percent Cover") +
  theme_bw() 



eltr_plot

library(patchwork)

(exotic_plot/gram_plot/forb_plot)

(shrub_plot/oak_plot/rone_plot)


(bare_plot/lit_plot/wood_plot)

unk_plot
