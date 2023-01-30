
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
library(patchwork)
library(scico)


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

d$RevBI<-ifelse(d$RevBI=="Unburned","Low",d$RevBI)


d$RevBI<-as.factor(d$RevBI)

d$RevBI<-factor(d$RevBI, levels = c("High","Mod","Low"))

str(d)

yo<-read.csv("Data/covariates.csv")
yo<-yo[,-1]



d<-as.data.table(d)
d<-d[,.(PctC=sum(PctC), PctC_basal=(sum(Basal_cm))/(Transect_cm), Total_cm = sum(Total_cm), Basal_cm = sum(Basal_cm), Canopy_cm=sum(Canopy_cm)),by=c("Transect_id","year","Code","RevBI","FS_BAND","TRT","Full_name","Origin","Transect_cm","Growth_form")]



#rounding errors result in Pct greater than 100 in 1 Brin plot

d$PctC<-ifelse(d$PctC>100,100,d$PctC)

d_fs <- d[which(d$FS_BAND == "FS"),]
d_band<- d[which(d$FS_BAND == "BAND"),]



d_freq_TRT <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(year, TRT) %>%
  summarise(
    prop_brin = length(which(Code=="brin"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brte = length(which(Code=="brte"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lomu = length(which(Code=="lomu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lope = length(which(Code=="lope"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brca = length(which(Code=="brca2"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_scsc = length(which(Code=="scsc"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_eltr = length(which(Code=="eltr"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_bocu = length(which(Code=="bocu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    nplot = length(unique(Transect_id)),
    )


trt<-d[,.(TRT=unique(TRT),RevBI=unique(RevBI)),by="Transect_id"]
trt


#frequency of occurrence table

dft<-melt(d_freq_TRT,id.vars=c("year","TRT"))
dft<-dcast(dft,variable+TRT~year,value.var="value")
#write.csv(dft,"freq_table_TRT.csv")

#frequency table sorted by agency


d_freq_FS <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(year, Full_name) %>%
  summarise(
    prop_brin = length(which(Code=="brin"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brte = length(which(Code=="brte"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lomu = length(which(Code=="lomu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lope = length(which(Code=="lope"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brca = length(which(Code=="brca2"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_scsc = length(which(Code=="scsc"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_eltr = length(which(Code=="eltr"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_bocu = length(which(Code=="bocu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    nplot = length(unique(Transect_id)),
  )




dfb<-melt(d_freq_FS,id.vars=c("year","FS_BAND"))
dfb<-dcast(dfb,variable+FS_BAND~year,value.var="value")
#write.csv(dfb,"freq_table_fsband.csv")

#frequency table sorted by burn severity

d_freq_RevBI <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(year, RevBI) %>%
  summarise(
    prop_brin = length(which(Code=="brin"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brte = length(which(Code=="brte"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lomu = length(which(Code=="lomu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_lope = length(which(Code=="lope"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_brca = length(which(Code=="brca2"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_scsc = length(which(Code=="scsc"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_eltr = length(which(Code=="eltr"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    prop_bocu = length(which(Code=="bocu"&Growth_form =="G")) / 
      (length(unique(Transect_id))),
    nplot = length(unique(Transect_id)),
  )

freq_FS <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(year, Full_name, Transect_id) %>%
  summarise(
    canopy=sum(Canopy_cm, na.rm=TRUE),
    canopy_pc=sum(Canopy_cm, na.rm=TRUE)/(Transect_cm),
    basal=sum(Basal_cm, na.rm=TRUE),
    basal_pc=sum(Basal_cm, na.rm=TRUE)/(Transect_cm),    
    total=sum(Total_cm, na.rm=TRUE),
    total_pc=sum(Total_cm, na.rm=TRUE)/(Transect_cm),
    nplot = length(unique(Transect_id)),
  )



spp<-dcast(freq_FS,year+Transect_id~Full_name,value.var="canopy")
spp<-as.data.frame(spp)

sppred<-spp[,-2]
sppred

spnames<-colnames(sppred)
sppsum<-colSums(sppred)
sitenames<-spp$Transect_id
ref<-cbind.data.frame(sppsum,spnames)
refred<-subset(ref,sppsum>20)
refred<-refred[-1,]
refname<-refred[,2]

df = spp[,(names(spp) %in% refname)]

sppab<-dcast(freq_FS,year+Transect_id~Full_name,value.var="canopy_pc",mean)
df = sppab[,(names(sppab) %in% refname)]

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- 0
names(df)

covs<-c("Animal feces","Litter","Bare","Rock","Wood","Moss","Gravel","Pumice soil","Unidentified forb","Unidentified grass","unknown Composite")

#covs<-c("Bromus inermis","Bromus tectorum","Elymus trachycaulum","Schizachyrium scoparius","Bouteloua curtipendula","Bromus carinatus","Lolium multiflorum","Lolium perenne")
#df2 = df[,!(names(df) %in% covs)]

df2 = df[,!(names(df) %in% covs)]

df2$year<-spp$year
df2$site<-sitenames


df97<-subset(df2,year==1997)
df98<-subset(df2,year==1998)
df08<-subset(df2,year==2008)
df13<-subset(df2,year==2013)
df16<-subset(df2,year==2016)
df19<-subset(df2,year==2019)

rownames(df97)<-df97$site
rownames(df98)<-df98$site
rownames(df08)<-df08$site
rownames(df13)<-df13$site
rownames(df16)<-df16$site
rownames(df19)<-df19$site

df97<-df97[,-(41:42)]
df98<-df98[,-(41:42)]
df08<-df08[,-(41:42)]
df13<-df13[,-(41:42)]
df16<-df16[,-(41:42)]
df19<-df19[,-(41:42)]


#set seed
set.seed(747)
library(vegan)

spp.bcd<-vegdist(df97)

#ordination code
spp.ord97<-metaMDS(df97,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord97,choices=c(1,2))

#adding plots and species labels
text(spp.ord97, display = "species", cex=1, col="black") #label species 
text(spp.ord97, display = "sites", cex=1, col="red") #label species 



spp.bcd98<-vegdist(df98)

#ordination code
spp.ord98<-metaMDS(df98,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord98,choices=c(1,2))

#adding plots and species labels
text(spp.ord98, display = "species", cex=1, col="black") #label species 
text(spp.ord98, display = "sites", cex=1, col="red") #label species 



spp.bcd08<-vegdist(df08)

#ordination code
spp.ord08<-metaMDS(df08,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord08,choices=c(1,2))

#adding plots and species labels
text(spp.ord08, display = "species", cex=1, col="black") #label species 
text(spp.ord08, display = "sites", cex=1, col="red") #label species 


spp.bcd13<-vegdist(df13)

#ordination code
spp.ord13<-metaMDS(df13,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord13,choices=c(1,2))

#adding plots and species labels
text(spp.ord13, display = "species", cex=1, col="black") #label species 
text(spp.ord13, display = "sites", cex=1, col="red") #label species 



spp.bcd16<-vegdist(df16)

#ordination code
spp.ord16<-metaMDS(df16,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord16,choices=c(1,2))

#adding plots and species labels
text(spp.ord16, display = "species", cex=1, col="black") #label species 
text(spp.ord16, display = "sites", cex=1, col="red") #label species 



spp.bcd19<-vegdist(df19)

#ordination code
spp.ord19<-metaMDS(df19,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
par(mfrow=c(1,1))
stressplot(spp.ord, spp.bcd) #stressplot
#plotting axes 1 and 2
ordiplot(spp.ord19,choices=c(1,2))

#adding plots and species labels
text(spp.ord19, display = "species", cex=1, col="black") #label species 
text(spp.ord19, display = "sites", cex=1, col="red") #label species 



sc97<-scores(spp.ord97)
sc98<-scores(spp.ord98)
sc08<-scores(spp.ord08)
sc13<-scores(spp.ord13)
sc16<-scores(spp.ord16)
sc19<-scores(spp.ord19)


topotrt<-merge(topo,trt)

rownames(topotrt)<-topo[,1]
topotrt<-topotrt[,-1]

topofit<-envfit(spp.ord19,topotrt)
topofit
plot(spp.ord19)
text(spp.ord19, display = "species", cex=1, col="black") #label species 
plot(topofit,p.max=0.05) #add vectors to previous ordination, only showing p<0.05
spp.ord19
cor(topo)
?metaMDS
topo

names<-(rownames(site19))

site19<-as.data.frame(sc19[[1]])
spp19<-as.data.frame(sc19[[2]])
spp19$spp<-rownames(spp19)
site19$Transect_id<-names
topotrt$Transect_id<-names
fullsite19<-merge(site19,topotrt)

grp.a <- fullsite19[fullsite19$TRT == "Seeded", ][chull(fullsite19[fullsite19$TRT == 
                                                                   "Seeded", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- fullsite19[fullsite19$TRT == "Unseeded", ][chull(fullsite19[fullsite19$TRT == 
                                                                   "Unseeded", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data19 <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data
fullsite19


spp19

ggplot()+
  geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=TRT),pch=16,size=3)+
  geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=TRT),pch=1,size=3)+
  #geom_text(spp97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  geom_segment(arrowtest, mapping=aes(x = NMDS1.x, xend = NMDS1.y, y = NMDS2.x, yend = NMDS2.y),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black") 

  
topofit$factors


arrowtest<-merge(site97,site19,by="Transect_id")

site97<-as.data.frame(sc97[[1]])
spp97<-as.data.frame(sc97[[2]])
spp97$spp<-rownames(spp97)
site97$Transect_id<-names
topotrt$Transect_id<-names
fullsite97<-merge(site97,topotrt)

grp.a <- fullsite97[fullsite97$TRT == "Seeded", ][chull(fullsite97[fullsite97$TRT == 
                                                                     "Seeded", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- fullsite97[fullsite97$TRT == "Unseeded", ][chull(fullsite97[fullsite97$TRT == 
                                                                       "Unseeded", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data97 <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data
fullsite98

ggplot()+
  geom_point(fullsite98,mapping=aes(x=NMDS1,y=NMDS2,pch=TRT,col=RevBI))+
  geom_text(spp98,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_polygon(hull.data,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.25)







ggscore19<-cbind(sc19[[1]],sc19[[2]])


spp.ord19$species

ordiplot(spp.ord97)

shush<-rbind(sppsum,sppred)


shush[1,]
shhh<-shush[1,]>20



shush<-colSums(spp[,-2])



?dcast


dfr<-melt(d_freq_RevBI,id.vars=c("year","RevBI"))
dfr<-dcast(dfr,variable+RevBI~year,value.var="value")
#write.csv(dfr,"freq_table_revbi.csv")



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
    pa_lol =  ifelse(sum(Canopy_cm[grep("Lolium",Full_name)]) < 0.000000001,0,1),
    pa_brin = ifelse(sum(Canopy_cm[grep("brin",Code)]) < 0.000000001,0,1),
    pa_brte = ifelse(sum(Canopy_cm[grep("brte",Code)]) < 0.000000001,0,1),
    pa_brca = ifelse(sum(Canopy_cm[grep("brca",Code)]) < 0.000000001,0,1),
    pa_bocu = ifelse(sum(Canopy_cm[grep("bocu",Code)]) < 0.000000001,0,1),
    pa_scsc = ifelse(sum(Canopy_cm[grep("scsc",Code)]) < 0.000000001,0,1),
    pa_eltr = ifelse(sum(Canopy_cm[grep("eltr",Code)]) < 0.000000001,0,1),
    
    PctC_brin = sum(Canopy_cm[grep("brin",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brte = sum(Canopy_cm[grep("brte",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brca = sum(Canopy_cm[grep("brca2",Code)], na.rm = T)/mean(Transect_cm),
    PctC_eltr = sum(Canopy_cm[grep("eltr",Code)], na.rm = T)/mean(Transect_cm),
    PctC_bocu = sum(Canopy_cm[grep("bocu",Code)], na.rm = T)/mean(Transect_cm),
    PctC_scsc = sum(Canopy_cm[grep("scsc",Code)], na.rm = T)/mean(Transect_cm),
    
    
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


#rounding errors result in Pct greater than 100 for two lolium spp.  doing it again here now that they're combined
d_annual$PctC_lolium<-ifelse(d_annual$PctC_lolium>100,100,d_annual$PctC_lolium)
d_annual$PctC_exotic<-ifelse(d_annual$PctC_exotic>100,100,d_annual$PctC_exotic)


d_annual_fs <- d_annual[which(d_annual$FS_BAND == "FS"),]
d_annual_band <- d_annual[which(d_annual$FS_BAND == "BAND"),]

#FS_BAND = unique(FS_BAND),
d_mean <- d_annual %>%
  group_by(year,TRT,RevBI) %>%
  summarise(#FS_BAND = unique(FS_BAND),

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
            
            PctC_bocu_mean = mean(PctC_bocu), 
            PctC_bocu_se = stdErr(PctC_bocu),
            
            PctC_scsc_mean = mean(PctC_scsc), 
            PctC_scsc_se = stdErr(PctC_scsc),
            
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


#FS_BAND = unique(FS_BAND),
d_mean_full <- d_annual %>%
  group_by(year) %>%
  summarise(#FS_BAND = unique(FS_BAND),
    
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
    
    PctC_bocu_mean = mean(PctC_bocu), 
    PctC_bocu_se = stdErr(PctC_bocu),
    
    PctC_scsc_mean = mean(PctC_scsc), 
    PctC_scsc_se = stdErr(PctC_scsc),
    
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

full_d<-merge(d_ann,yo,by=c("Transect_id","year","Month"))
full_d<-merge(full_d,clim,by="year")

full_d$PctC_brin<-ifelse(full_d$PctC_brin==1,0.999,full_d$PctC_brin)
full_d$PctC_brin<-ifelse(full_d$PctC_brin==0,0.001,full_d$PctC_brin)

full_d$RevBI <- factor(full_d$RevBI, levels = c("High", "Mod", "Low"))
d_mean$RevBI <- factor(d_mean$RevBI, levels = c("High", "Mod", "Low"))

full_d<-as.data.table(full_d)
mean_spei<-full_d[,.(spei=mean(spei)),by="year"]
d_mean$spei<-mean_spei$spei

unique(d$Full_name)

#full_d<-merge(full_d,lol_factor)

pct_mod<-glmmTMB(PctC_exotic ~ TRT + RevBI + scale(elevation)  + spei  + (1|Transect_id), data = full_d, family = "binomial" )
r.squaredGLMM(pct_mod)
summary(pct_mod)


library(DHARMa)
sim<-simulateResiduals(bare_pct)
testOverdispersion(sim)
plot(sim)


testZeroInflation(sim)





?glmmTMB
brin1<-glmmTMB(PctC_brin ~ TRT + RevBI + scale(elevation)  + spei, data=full_d)
brin2<-glmmTMB(PctC_brin ~ TRT + RevBI + scale(elevation)  + spei  +  (1|Transect_id), ziformula = ~., data=full_d, family="binomial")
brin3<-glmmTMB(PctC_brin ~ TRT + RevBI + scale(elevation)  + spei  +  (1|Transect_id), data=full_d, family="binomial")
brin4<-glmmTMB(log(PctC_brin+0.01) ~ TRT + RevBI + scale(elevation)  + spei  +  (1|Transect_id), data=full_d)

summary(brin4)

cor.test(predict(brin_pct,type="response"),full_d$PctC_brin)
plot(predict(brin_pa,type="link"),full_d$pa_brin)

?predict
predict(brin_pa,type="response")

levels(full_d$pa_brin)

full_d$pa_brin<-factor(full_d$pa_brin, levels=rev(levels(full_d$pa_brin)))

brin_pa<-glmmTMB(pa_brin ~ TRT + RevBI  + scale(elevation)  + spei + (1|Transect_id), data=full_d,family="binomial")
summary(brin_pa)
r.squaredGLMM(brin_pa)

lol_pa<-glmmTMB(as.factor(pa_lol) ~ TRT + RevBI + scale(elevation)  + spei +  (1|Transect_id) , data=full_d, family="binomial")
summary(lol_pa)
r.squaredGLMM(lol_pa)

brte_pa<-glmmTMB(pa_brte ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(brte_pa)
r.squaredGLMM(brte_pa)

brca_pa<-glmmTMB(pa_brca ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(brca_pa)
r.squaredGLMM(brca_pa)

eltr_pa<-glmmTMB(pa_eltr ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(eltr_pa)
r.squaredGLMM(eltr_pa)

bocu_pa<-glmmTMB(pa_bocu ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(bocu_pa)
performance::r2(bocu_pa)

scsc_pa<-glmmTMB(pa_scsc ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(scsc_pa)
r.squaredGLMM(scsc_pa)

mod<-glmmTMB(PctC_bocu ~ TRT + RevBI +scale(elevation)   +(1|Transect_id), data=full_d, family="binomial")
summary(mod)
mosim<-simulateResiduals(brin_pct)
plot(mosim)

library(visreg)
library(ggeffects)

plot(ggpredict(brin_pct,terms="spei"),add.data=T)

ggplot()+
  geom_line(data=full_d,)

visreg(brin_pa,"RevBI",scale="response")

library(MuMIn)
options(na.action="na.fail")
dredge(brin_pct)
brin_pct<-glmmTMB(PctC_brin ~ TRT + RevBI +scale(elevation)  + winter_spei  + summer_spei + (1|Transect_id), data=full_d,family="binomial")
summary(brin_pct)
sim<-simulateResiduals(bare_pct)
plot(sim)
plot(predict(brin_pct),full_d$PctC_brin)

brin<-acf(full_d$PctC_brin)

acf(full_d$PctC_brin)

full_d$scaledbrin<-scale(full_d$PctC_brin)

brin_pct<-glmmTMB(scaledbrin ~ TRT , data=full_d)
summary(brin_pct)
performance::r2(brin_pct)
performance::check_singularity(brin_pct)

sim<-simulateResiduals(bare_pct)
testOverdispersion(sim)
plot(sim)
testZeroInflation(sim)

plotResiduals(sim,full_d$RevBI)

lol_pct<-glmmTMB(PctC_lolium ~ TRT + RevBI + scale(elevation)  + spei +  (1|Transect_id) , data=full_d, family="binomial")
summary(lol_pct)
r.squaredGLMM(lol_pct)

brte_pct<-glmmTMB(PctC_brte ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(brte_pct)
r.squaredGLMM(brte_pct)

brca_pct<-glmmTMB(PctC_brca ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family=beta_family())
summary(brca_pct)
r.squaredGLMM(brca_pct)

eltr_pct<-glmmTMB(PctC_eltr ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id) , data=full_d, family="binomial")
summary(eltr_pct)
r.squaredGLMM(eltr_pct)

bocu_pct<-glmmTMB(PctC_bocu ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(bocu_pct)
performance::r2(bocu_pct)

scsc_pct<-glmmTMB(PctC_scsc ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id), data=full_d, family="binomial")
summary(scsc_pct)
r.squaredGLMM(scsc_pct)


bare_pct<-glmmTMB(PctC_bare ~ TRT + RevBI + scale(elevation)   + spei  + (1|Transect_id), data=full_d, family="binomial")
summary(bare_pct)

r.squaredGLMM(bare_pct)

gram_pct<-glmmTMB(PctC_gram ~ TRT + RevBI + scale(elevation)  + spei  +  (1|Transect_id), data=d_brca, family="binomial")
summary(gram_pct)
r.squaredGLMM(gram_pct)

exotic_pct<-glmmTMB(PctC_exotic ~ TRT + RevBI + scale(elevation)  + spei + (1|Transect_id), data=d_brca, family="binomial")
summary(exotic_pct)
r.squaredGLMM(exotic_pct)

performance::r2(brin_pct)

s1<-summary(brin_pct)[6]$coefficients$cond
s2<-summary(brte_pct)[6]$coefficients$cond
s3<- summary(lol_pct)[6]$coefficients$cond
s4<-summary(eltr_pct)[6]$coefficients$cond
s5<-summary(bocu_pct)[6]$coefficients$cond
s6<-summary(brca_pct)[6]$coefficients$cond
s7<-summary(scsc_pct)[6]$coefficients$cond


coefs<-rbind(s1,s2,s3,s4,s5,s6,s7)
write.csv(coefs,"coefs_prop.csv")


s1[4]

s
s


plot(d_annual$PctC_shrub,d_annual$PctC_brin)



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

hm<-merge(d_mean,clim)


full_plot_unscale<-ggplot(hm)+
  geom_hline(yintercept=0,lty=2)+
  geom_line(mapping=aes(x=year,y=(PctC_lolium_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brca_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_eltr_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_bocu_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brin_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brte_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_scsc_mean)))+
  geom_line(mapping=aes(x=year,y=divided_spei_winter),col="blue")+
  geom_line(mapping=aes(x=year,y=divided_spei_summer),col="red")

full_plot_unscale<-ggplot(hm)+
  geom_hline(yintercept=0,lty=2)+
  geom_line(mapping=aes(x=year,y=(PctC_lolium_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brca_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_eltr_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_bocu_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brin_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_brte_mean)))+
  geom_line(mapping=aes(x=year,y=(PctC_scsc_mean)))+
  geom_line(mapping=aes(x=year,y=winter_spei),col="blue")+
  geom_line(mapping=aes(x=year,y=summer_spei),col="red")


full_plot_scale<-ggplot(hm)+
  geom_hline(yintercept=0,lty=2)+
  geom_line(mapping=aes(x=year,y=scale(PctC_lolium_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_brca_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_eltr_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_bocu_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_brin_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_brte_mean)))+
  geom_line(mapping=aes(x=year,y=scale(PctC_scsc_mean)))+
  geom_line(mapping=aes(x=year,y=winter_spei),col="blue")+
  geom_line(mapping=aes(x=year,y=summer_spei),col="red")


full_plot_scale/full_plot_unscale

brin_plot

exotic_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_exotic_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_exotic_mean - PctC_exotic_se, 
                    ymax = PctC_exotic_mean + PctC_exotic_se )) +
  geom_point(d_annual, mapping = aes(x=year, y = PctC_exotic,pch=TRT),size=3,alpha=0.15)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "All exotic Percent Cover") +
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  theme_bw() 




early_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_lolium_mean, lty = TRT),col="tomato")+
  geom_errorbar(aes(x=year, ymin = PctC_lolium_mean - PctC_lolium_se, 
                    ymax = PctC_lolium_mean + PctC_lolium_se ),col="tomato") +
  geom_line(mapping = aes(x=year,y=PctC_brca_mean, lty = TRT),col="steelblue")+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se ),col="steelblue") +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "% Cover") +
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_brin),alpha=0.25)+
  scale_color_manual(name="Species",breaks=c("Lolium","BRCA"),values=c("Lolium"="tomato",'BRCA'="steelblue"))+
  theme_bw()+
  theme(legend.position = "bottom")


early_plot

early_plot/brin_plot/native_plot


early_plot

brin_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_brin_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_brin_mean - PctC_brin_se, 
                    ymax = PctC_brin_mean + PctC_brin_se )) +
  geom_line(mapping = aes(x=year,y=PctC_brte_mean, lty = TRT),col="blue")+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se ),col="blue") +
 #geom_point(d_annual, mapping = aes(x=year, y = PctC_brte),col="blue",alpha=0.25)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "% BRIN Cover") +
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_brin),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  theme_bw()+
  theme(legend.position = "none")

native_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_bocu_mean, lty = TRT),col="green")+
  geom_errorbar(aes(x=year, ymin = PctC_bocu_mean - PctC_bocu_se, 
                    ymax = PctC_bocu_mean + PctC_bocu_se ),col="green") +
  geom_line(mapping = aes(x=year,y=PctC_scsc_mean, lty = TRT),col="darkorange")+
  geom_errorbar(aes(x=year, ymin = PctC_scsc_mean - PctC_scsc_se, 
                    ymax = PctC_scsc_mean + PctC_scsc_se),col="darkorange") +
  geom_line(mapping = aes(x=year,y=PctC_eltr_mean, lty = TRT),col="black")+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se),col="black") +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_bocu, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1, "Agency")+
  labs(lty = "Treatment", y = "% BOCU Cover") +
  theme_bw() +
  theme(legend.position = "bottom")


brte_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_brte_mean, lty = TRT,col),col="blue")+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se ),col="blue") +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_brte, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1, "Agency")+
  labs(lty = "Treatment", y = "% BRTE Cover") +
  theme_bw()+
  theme(legend.position = "bottom") 
  

lolium_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_lolium_mean, lty = TRT,col=FS_BAND),col="red")+
  geom_errorbar(aes(x=year, ymin = PctC_lolium_mean - PctC_lolium_se, 
                    ymax = PctC_lolium_mean + PctC_lolium_se, col=FS_BAND ),col="red") +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_lolium, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% Lolium Cover") +
  theme_bw()+
  theme(legend.position = "none") 


bare_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_bare_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se, lty =TRT )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_bare),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% Bare Cover") +
  theme_bw()+
  theme(legend.position = "none") 

gram_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_gram_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_gram_mean - PctC_gram_se, 
                    ymax = PctC_gram_mean + PctC_gram_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_gram),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% Graminoid Cover") +
  theme_bw() +
  theme(legend.position = "none")

forb_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_forb_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_forb_mean - PctC_forb_se, 
                    ymax = PctC_forb_mean + PctC_forb_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_forb),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1, "Agency")+
  labs(lty = "Treatment", y = "% Forb Cover") +
  theme_bw() +
  theme(legend.position = "bottom")

unk_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_unk_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_unk_mean - PctC_unk_se, 
                    ymax = PctC_unk_mean + PctC_unk_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  geom_point(d_annual, mapping = aes(x=year, y = PctC_unk),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "% Unknown Cover") +
  theme_bw() 


lit_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_litter_mean, lty = TRT))+
  geom_errorbar(aes(x=year, ymin = PctC_litter_mean - PctC_litter_se, 
                    ymax = PctC_litter_mean + PctC_litter_se )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_litter),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% Litter Cover") +
  theme_bw() 

wood_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_logs_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_logs_mean - PctC_logs_se, 
                    ymax = PctC_logs_mean + PctC_logs_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_logs, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% Woody Debris Cover") +
  theme_bw() 

shit_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_shit_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_shit_mean - PctC_shit_se, 
                    ymax = PctC_shit_mean + PctC_shit_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_shit, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% SHIT Cover") +
  theme_bw() 


brca_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_brca_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_brca, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% BRCA Cover") +
  theme_bw() +
  theme(legend.position = "none")

eltr_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_eltr_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_eltr, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% ELTR Cover") +
  theme_bw() +
  theme(legend.position = "none")



  
bocu_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_bocu_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_bocu_mean - PctC_bocu_se, 
                    ymax = PctC_bocu_mean + PctC_bocu_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_bocu, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1, "Agency")+
  labs(lty = "Treatment", y = "% BOCU Cover") +
  theme_bw() +
  theme(legend.position = "bottom")


scsc_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_scsc_mean, lty = TRT,col=FS_BAND))+
  geom_errorbar(aes(x=year, ymin = PctC_scsc_mean - PctC_scsc_se, 
                    ymax = PctC_scsc_mean + PctC_scsc_se,col=FS_BAND )) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_scsc, col=FS_BAND),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  labs(lty = "Treatment", y = "% SCSC Cover") +
  theme_bw() +
  theme(legend.position = "none")




setwd("C:/Users/awion/Documents/Github/Dome_Invasion_Share")

png("./Figures/EDA/groundcover.png",height = 20, width = 20, units = "cm", res=300, bg='white')
(bare_plot/gram_plot/exotic_plot)
dev.off()

png("./Figures/EDA/exoticcover.png",height = 12.5, width = 17.5, units = "cm", res=300, bg='white')
(brin_plot/brte_plot)
dev.off()

png("./Figures/EDA/brcalol.png",height = 12.5, width = 17.5, units = "cm", res=300, bg='white')
(brca_plot/lolium_plot)
dev.off()

png("./Figures/EDA/nativecover.png",height = 20, width = 20, units = "cm", res=300, bg='white')
(eltr_plot/scsc_plot/bocu_plot)
dev.off()


eltr_plot


(bare_plot/lit_plot/wood_plot)

shit_plot



(shrub_plot/oak_plot/rone_plot)

ggplot()+
  geom_boxplot(full_d,mapping=aes(x=FS_BAND,y=cwd))

unk_plot













d_97<-d_annual[which(year==1997)]
d_98<-d_annual[which(year==1998)]
d_08<-d_annual[which(year==2008)]
d_13<-d_annual[which(year==2013)]
d_16<-d_annual[which(year==2016)]
d_19<-d_annual[which(year==2019)]
