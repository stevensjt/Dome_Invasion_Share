library(raster)
library(data.table)
library(ggplot2)
library(ggnewscale)
library(ggpattern)
library(scico)
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
library(DHARMa)

library(visreg)
library(ggeffects)

library(MuMIn)

library(vegan)



#spatial data

AET<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/AET_1981-2010.tif")
CWD<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/CWD_1981-2010.tif")
pts<-shapefile("Data/Spatial/Dome Transect stakes/Dome_Transect_pts.shp")

elev<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/USGS_13_n36w107_20220801.tif")
slop<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/jemez_slope")
aspt<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/jemez_aspect")

#soil
clay<-raster("C:/Users/awion/Downloads/polaris_clay.tif")
silt<-raster("C:/Users/awion/Downloads/polaris_silt.tif")
sand<-raster("C:/Users/awion/Downloads/polaris_sand.tif")
#created using script XXXX
awc<-read.csv("awc.csv")


dome<-shapefile("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/1996/nm3576910637819960425_19950723_19970914_burn_bndy.shp")

dome_rnbr<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/1996/nm3576910637819960425_19950723_19970914_rdnbr.tif")
dome_dnbr<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/1996/nm3576910637819960425_19950723_19970914_dnbr.tif")
dome_them<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/1996/nm3576910637819960425_19950723_19970914_dnbr6.tif")


conchas<-shapefile("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/2011/nm3581210654120110626_20110624_20120618_burn_bndy.shp")

conchas_rnbr<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/2011/nm3581210654120110626_20110624_20120618_rdnbr.tif")
conchas_dnbr<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/2011/nm3581210654120110626_20110624_20120618_dnbr.tif")
conchas_them<-raster("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/2011/nm3581210654120110626_20110624_20120618_dnbr6.tif")



juan<-shapefile("C:/Users/awion/Documents/GIS_data/MTBS/deom_veg/2009/nm3574610634920090702_20090611_20100630_burn_bndy.shp")
band<-shapefile("C:/Users/awion/Documents/GIS_data/bandelier.shp")
unseed<-shapefile("C:/Users/awion/Documents/GIS_data/dome transect stakes/dome unseeded poly/unseeded.shp")

unseed<-fortify(unseed)

utm<-crs(pts)
aea<-crs(dome_dnbr)

pts_aea<-spTransform(pts,CRS=aea)



#burn severity, rdnbr, dnbr, and thematic mapping from mtbs.  conchas is fucked up

dome_dnbr<-extract(dome_dnbr,pts_aea)
dome_them<-extract(dome_them,pts_aea)
dome_rnbr<-extract(dome_rnbr,pts_aea)
Transect_id<-pts$IDENT

#conchas_dnbr<-extract(conchas_dnbr,pts_aea)
#conchas_them<-extract(conchas_them,pts_aea)
#conchas_rnbr<-extract(conchas_rnbr,pts_aea)


sites_dnbr<-as.data.frame(dome_dnbr)
sites_them<-as.data.frame(dome_them)
sites_rnbr<-as.data.frame(dome_rnbr)

burn<-cbind(Transect_id,sites_dnbr,sites_rnbr,sites_them)
burn$stand_replacing<-as.factor(ifelse(burn$dome_them > 2, 1, 0))


###### for mapping #####

dome<-spTransform(dome,CRS=utm)
conchas<-spTransform(conchas,CRS=utm)
juan<-spTransform(juan,CRS=utm)
band<-spTransform(band,CRS=utm)

dome.df<-fortify(dome)
juan.df<-fortify(juan)
conchas.df<-fortify(conchas)
band.df<-fortify(band)



#topographic vars


folded<-abs(180-abs((aspt-225)))
elv.pts<-extract(elev,pts)
asp.pts<-extract(folded,pts)
slp.pts<-extract(slop,pts)
cwd.pts<-extract(CWD,pts)
aet.pts<-extract(AET,pts)
cly.pts<-extract(clay,pts)
snd.pts<-extract(sand,pts)
slt.pts<-extract(silt,pts)

topo<-cbind.data.frame(pts$IDENT,elv.pts,cwd.pts,aet.pts,cly.pts,snd.pts,slt.pts,slp.pts,asp.pts,awc[,c(2,4)])
names(topo)<-c("Transect_id","elevation","cwd","aet","clay","sand","silt","slope","aspect","awc",'depth')

topo

covs<-merge(burn,topo)




ptsf<-merge(pts,topocov)

pts$response<-topocov$TRT_any

plot(pts,col=pts$response)

###plot data

stdErr <- function(x) sqrt(var(x, na.rm = T)/length(na.exclude(x)))

#sampling dates
dates<-read.csv("Data/visit_dates.csv")

####1. Data input and processing####

d <- read.csv("Data/Raw/All Years 1997-2019 PermPlot-Entire-APW.csv")
d
d$Transect_id <- toupper(d$Transect_id) #Uppercase for consistency
#unburned was confusing, changing to low here, but not important later on
d$RevBI<-ifelse(d$RevBI=="Unburned","Low",d$RevBI)
d$RevBI<-as.factor(d$RevBI)
d$RevBI<-factor(d$RevBI, levels = c("High","Mod","Low"))

#creating a new treatment column - FS unseeded, which were an imperfect control

d$newtrt<-ifelse(d$FS_BAND=="FS"&d$TRT=="Unseeded","Unseeded Adjacent",d$TRT)


d<-as.data.table(d)



#factor variable, possibly delete later on
trt<-d[,.(FS_BAND=unique(FS_BAND),TRT=unique(TRT),RevBI=unique(RevBI)),by="Transect_id"]



d<-d[,.(PctC=sum(PctC), PctC_basal=(sum(Basal_cm))/(Transect_cm), 
        Total_cm = sum(Total_cm), Basal_cm = sum(Basal_cm), 
        Canopy_cm=sum(Canopy_cm)),
     by=c("Transect_id","year","Code","RevBI","FS_BAND","TRT","newtrt","Full_name","Origin","Transect_cm","Growth_form")]


#rounding errors result in Pct greater than 100 in 1 Brin plot

d$PctC<-ifelse(d$PctC>100,100,d$PctC)



#subsetting fs from band data, not important later on
d_fs <- d[which(d$FS_BAND == "FS"),]
d_band<- d[which(d$FS_BAND == "BAND"),]


#creating frequency table based on treatment - consider redoing with new control category
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


dft

#frequency of occurrence table

dft<-melt(d_freq_TRT,id.vars=c("year","TRT"))
#dft<-dcast(dft,variable+TRT~year,value.var="value")
#write.csv(dft,"freq_table_TRT.csv")

#frequency table sorted by agency


d_annual <- 
  #Remove trees from dataset because they're all native and mess up %cover
  d[-which(d$Growth_form=="T"),] %>%
  group_by(Transect_id, year) %>%
  summarise(
    RevBI = unique(RevBI),
    FS_BAND = unique(FS_BAND), 
    TRT = unique(TRT),
    FS_TRT = unique(newtrt),
    
    pa_lol =  ifelse(sum(Canopy_cm[grep("Lolium",Full_name)]) == 0,0,1),
    pa_brin = ifelse(sum(Canopy_cm[grep("brin",Code)]) == 0,0,1),
    pa_brte = ifelse(sum(Canopy_cm[grep("brte",Code)]) == 0,0,1),
    pa_brca = ifelse(sum(Canopy_cm[grep("brca",Code)]) == 0,0,1),
    pa_bocu = ifelse(sum(Canopy_cm[grep("bocu",Code)]) == 0,0,1),
    pa_scsc = ifelse(sum(Canopy_cm[grep("scsc",Code)]) == 0,0,1),
    pa_eltr = ifelse(sum(Canopy_cm[grep("eltr",Code)]) == 0,0,1),
    
    PctC_exot = sum(Canopy_cm[Origin == "exotic"], na.rm = T)/mean(Transect_cm),
    PctC_lolium = sum(Canopy_cm[grep("Lolium",Full_name)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_lomu = sum(Canopy_cm[grep("lomu",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_lope = sum(Canopy_cm[grep("lope",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brin = sum(Canopy_cm[grep("brin",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brte = sum(Canopy_cm[grep("brte",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brca = sum(Canopy_cm[grep("brca2",Code)], na.rm = T)/mean(Transect_cm),
    PctC_eltr = sum(Canopy_cm[grep("eltr",Code)], na.rm = T)/mean(Transect_cm),
    PctC_bocu = sum(Canopy_cm[grep("bocu",Code)], na.rm = T)/mean(Transect_cm),
    PctC_scsc = sum(Canopy_cm[grep("scsc",Code)], na.rm = T)/mean(Transect_cm),
    
    PctC_bare = sum(Total_cm[grep("Bare",Full_name)], na.rm =T)/mean(Transect_cm),
    PctC_gram = sum(Canopy_cm[grep("G",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_forb = sum(Canopy_cm[grep("F",Growth_form)], na.rm =T)/mean(Transect_cm),
    
    PctC_shrub = sum(Canopy_cm[grep("S",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_oak =  sum(Canopy_cm[grep("qu",Code)], na.rm = T)/mean(Transect_cm),
    PctC_rone =  sum(Canopy_cm[grep("rone",Code)], na.rm = T)/mean(Transect_cm),
    

    
    PctC_shit = sum(Total_cm[grep("Animal feces",Full_name)], na.rm = T)/mean(Transect_cm),
    PctC_litter = sum(Basal_cm[grep("L",Growth_form)], na.rm =T)/mean(Transect_cm),
    PctC_logs = sum(Total_cm[grep("Wood",Full_name)], na.rm =T)/mean(Transect_cm),
    PctC_unk = sum(Total_cm[grep("Unidentified",Full_name)], na.rm =T)/mean(Transect_cm),
    
    total_pct = sum(PctC, na.rm=T),
    
    Richness_exotic = length(which(Origin=="exotic")),
    Richness_native = length(which(Origin=="native")),
    Prop_exotic = length(which(Origin=="exotic")) / 
      (length(which(Origin=="exotic")) + length(which(Origin=="native")))
    
  )


#fixing rounding errors

d_annual$PctC_exot<-ifelse(d_annual$PctC_exot>1,1,d_annual$PctC_exot)
d_annual$PctC_lolium<-ifelse(d_annual$PctC_lolium>1,1,d_annual$PctC_lolium)
d_annual$PctC_brin<-ifelse(d_annual$PctC_brin>1,1,d_annual$PctC_brin)
d_annual$PctC_brte<-ifelse(d_annual$PctC_brte>1,1,d_annual$PctC_brte)
d_annual$PctC_gram<-ifelse(d_annual$PctC_gram>1,1,d_annual$PctC_gram)


#subsetting, not important
d_annual_fs <- d_annual[which(d_annual$FS_BAND == "FS"),]
d_annual_band <- d_annual[which(d_annual$FS_BAND == "BAND"),]



#FS_BAND = unique(FS_BAND),
d_mean <- d_annual %>%
  group_by(year,TRT) %>%
  summarise(#FS_BAND = unique(FS_BAND),
    
    PctC_exot_mean = mean(PctC_exot), 
    PctC_exot_se = stdErr(PctC_exot), 
    
    PctC_brin_mean = mean(PctC_brin), 
    PctC_brin_se = stdErr(PctC_brin), 
    
    PctC_brte_mean = mean(PctC_brte), 
    PctC_brte_se = stdErr(PctC_brte), 
    
    PctC_lolium_mean = mean(PctC_lolium), 
    PctC_lolium_se = stdErr(PctC_lolium),
    
    PctC_lomu_mean = mean(PctC_lomu), 
    PctC_lomu_se = stdErr(PctC_lomu),
    
    PctC_lope_mean = mean(PctC_lope), 
    PctC_lope_se = stdErr(PctC_lope),
    
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
    
    PctC_bare_mean = mean(PctC_bare),
    PctC_bare_se = stdErr(PctC_bare),
    
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
    
    Prop_exot_mean = mean(Prop_exotic),
    Prop_exot_se = stdErr(Prop_exotic),
    
  )


#identical to above, but averaged across all transects (no treatment or burn severity)

#FS_BAND = unique(FS_BAND),
d_mean_full <- d_annual %>%
  group_by(year) %>%
  summarise(#FS_BAND = unique(FS_BAND),
    
    PctC_exot_mean = mean(PctC_exot), 
    PctC_exot_se = stdErr(PctC_exot), 
    
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
    
    PctC_bare_mean = mean(PctC_bare),
    PctC_bare_se = stdErr(PctC_bare),
    
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
    
    Prop_exot_mean = mean(Prop_exotic),
    Prop_exot_se = stdErr(Prop_exotic),
    
  )




#early responders
d97<-subset(d_annual,year==1997)
d19<-subset(d_annual,year==2019)


late_stage<-d19

early_factor<-d97
early_factor$early_resp<-early_factor$PctC_lolium+early_factor$PctC_brca

earlyd97<-early_factor[,c(1:6,16:18,38)]

earlyd97$TRT_response<-ifelse(earlyd97$early_resp > 0.33,1,0)
earlyd97$TRT_any<-ifelse(earlyd97$early_resp > 0,1,0)


#factor data frame
trtresponse<-earlyd97[,c(1,5,6,11,12)]
trtresponse$TRT_response<-as.factor(trtresponse$TRT_response)
trtresponse$TRT_any<-as.factor(trtresponse$TRT_any)

names(d_annual)

#ground cover for ordination
herb_cover<-d_annual[,c(1,2,24:27)]

herb_cover


#old JENS code


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









covs
trt
#run file dome_covariates to merge here
d_annual<-as.data.table(d_annual)
topocov<-merge(trtresponse,covs)
topocov
full_d<-merge(d_annual,topocov)

#d_ann<-merge(dates,d_annual)


full_d<-as.data.table(full_d)
full_d$yearfac<-as.factor(full_d$year)

full_d$yearfac

#models
full_d
tst<-full_d[,c(1,2,4,14:26)]
tst$reburn<-ifelse(tst$year<2009,"pre","post")

tstmelt<-melt(tst,id.vars = c("Transect_id","TRT","year","reburn"))
tstmelt<-subset(tstmelt,year!=2002)
tstmelt<-as.data.table(tstmelt)

meanburn<-tstmelt[,.(value=mean(value)),by=c("TRT","Transect_id","reburn","variable")]


meanburn$reburn<-as.factor(meanburn$reburn)
levels(meanburn$reburn)

meanburn$reburn <- with(meanburn, relevel(reburn, "pre"))


levels(meanburn$reburn)<-rev(levels(meanburn$reburn))
meanburn$reburn

levels(meanburn$reburn)<-rev(levels(meanburn$reburn))


unique(tstmelt$year)

yrs<-(unique(tstmelt$year))
var<-as.character(unique(tstmelt$variable))



full_w<-vector()


sw$null.value

for(i in 1:length(yrs)){
  for(j in 1:length(var)){
    sub<-subset(tstmelt,year==yrs[i])
    su<-subset(sub,variable==var[j])
    sw<-wilcox.test(su$value~su$TRT)
    fu<-cbind(var[j],yrs[i],sw$p.value)
    full_w<-rbind(fu,full_w)
  }
}


#pre and post-reburn

library(PNWColors)
pal6<-pnw_palette("Sunset",n=6)
pal2<-pal6[c(1,5)]
pal4<-pnw_palette("Sunset",n=4)
pal3<-pnw_palette("Sunset",n=3)

library(ggsignif)

ggplot(meanburn,mapping=aes(x=reburn,y=value,col=TRT))+
  geom_boxplot()+
  scale_color_manual(values=pal2,limits=rev(levels(tstmelt$reburn)))+
  geom_signif(comparisons=list(c("pre","post",map_signif_level = TRUE)))+
  geom_signif(y_position = c(0.4, 0.6), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
  annotation = c("**", "NS"), tip_length = 0) +
  facet_wrap(~variable)



?grep




reb<-unique(meanburn$reburn)
rebu_w<-vector()


su$TRT

for(i in 1:length(var)){
    unsub<-subset(meanburn,TRT=="Unseeded")
    sdsub<-subset(meanburn,TRT=="Seeded")
    presub<-subset(meanburn,reburn=="pre")
    postsub<-subset(meanburn,reburn=="post")
    sw<-wilcox.test(unsub$value~unsub$reburn)
    si<-wilcox.test(sdsub$value~sdsub$reburn)
    fu<-cbind(var[i],sw$p.value,si$p.value)
    rebu_w<-rbind(fu,rebu_w)
  }





meltmean<-melt(d_mean,id.vars = c("TRT","year"))
str(meltmean)
meltmean

meltmean$mvar<-substr(meltmean$variable,1,9)

full_w<-as.data.frame(full_w)
str(full_w)
names(full_w)<-c("mvar","year",'p_value')
full_w
full_w$year<-as.integer(full_w$year)
full_w$p_value<-as.numeric(full_w$p_value)
full_w$mvar<-as.factor(full_w$mvar)


bruh<-merge(meltmean,full_w,by=c("year","mvar"))

?dcast
bruh
bruh<-as.data.table(bruh)
bruh$sig<-ifelse(bruh$p_value<0.05,1,0)
broh<-bruh[,.(value=value),by=c("year","mvar","TRT","variable","p_value","sig")]
broh

bruh$variable<- paste(bruh$variable, "_sig", sep="")
brah<-dcast(bruh,year~variable,value.var="p_value",fun=mean)

sigbrah<-(ifelse(brah<0.05,1,0))
sigbrah<-as.data.frame(sigbrah)
#sigbrah[,2:25]<-as.factor(sigbrah[,2:25])
sigbrah$year<-brah$year


d_mean_brca<-subset(bruh,mvar=="PctC_brca")
d_mean_brin<-subset(bruh,mvar=="PctC_brin")
d_mean_brte<-subset(bruh,mvar=="PctC_brte")
d_mean_bocu<-subset(bruh,mvar=="PctC_bocu")
d_mean_eltr<-subset(bruh,mvar=="PctC_eltr")
d_mean_scsc<-subset(bruh,mvar=="PctC_scsc")
d_mean_gram<-subset(bruh,mvar=="PctC_gram")
d_mean_forb<-subset(bruh,mvar=="PctC_forb")
d_mean_exot<-subset(bruh,mvar=="PctC_exot")
d_mean_lomu<-subset(bruh,mvar=="PctC_lomu")
d_mean_lope<-subset(bruh,mvar=="PctC_lope")
d_mean_bare<-subset(bruh,mvar=="PctC_bare")

d_meantry<-merge(d_mean,sigbrah,by="year")









lbare<-ggplot(d_meantry)+
  geom_line(mapping=aes(x=year,y=PctC_bare_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se,col=TRT,alpha=as.factor(PctC_bare_se_sig)),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  scale_alpha_manual(values = c(0.3,1))+
  scale_linetype_manual(values = c(2,1))+
  labs(x="Year",y="% Cover",title='Bare ground cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.5,label="*",size=9)+
  ylim(0,0.55)+
  theme_bw()+
  theme(legend.position = "none")

lbare

lgram<-ggplot(d_meantry)+
  geom_line(mapping=aes(x=year,y=PctC_gram_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_gram_mean - PctC_gram_se, 
                    ymax = PctC_gram_mean + PctC_gram_se,col=TRT,alpha=as.factor(PctC_gram_se_sig)),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_alpha_manual(values = c(0.3,1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Grass cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.7,label="*",size=9)+
  annotate("text",x=1998,y=0.7,label="*",size=9)+
  annotate("text",x=2008,y=0.7,label="*",size=9)+
  annotate("text",x=2013,y=0.7,label="*",size=9)+
  annotate("text",x=2016,y=0.7,label="*",size=9)+
  #annotate("text",x=2019,y=0.25,label="*",size=9)+
  ylim(0,0.75)+
  theme_bw()+
  theme(legend.position = "none")

lforb<-ggplot(d_meantry)+
  geom_line(mapping=aes(x=year,y=PctC_forb_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_forb_mean - PctC_forb_se, 
                    ymax = PctC_forb_mean + PctC_forb_se,col=TRT,alpha=as.factor(PctC_forb_se_sig)),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_alpha_manual(values = c(0.3,1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Forb cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.3,label="*",size=9)+
  annotate("text",x=1998,y=0.3,label="*",size=9)+
  ylim(0,.35)+
  theme_bw()+
  theme(legend.position = "none")


lexotic<-ggplot(d_meantry)+
  geom_line(mapping=aes(x=year,y=PctC_exot_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_exot_mean - PctC_exot_se, 
                    ymax = PctC_exot_mean + PctC_exot_se,col=TRT, alpha = as.factor(PctC_exot_se_sig)),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_alpha_manual(values = c(1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Non-native cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.55,label="*",size=9)+
  annotate("text",x=1998,y=0.55,label="*",size=9)+
  annotate("text",x=2008,y=0.55,label="*",size=9)+
  annotate("text",x=2013,y=0.55,label="*",size=9)+
  annotate("text",x=2016,y=0.55,label="*",size=9)+
  annotate("text",x=2019,y=0.55,label="*",size=9)+
  ylim(0,0.6)+
  theme_bw()+
  theme(legend.position = "none")

lbare|lgram|lforb|lexotic


dev.off()


llope<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lope_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lope_mean - PctC_lope_se, 
                    ymax = PctC_lope_mean + PctC_lope_se,col=TRT )) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbrte<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brte_mean,col=TRT,size=0.5))+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se,col=TRT )) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='brte')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



peltr

dft<-subset(dft,variable!="nplot")
dft<-subset(dft,year!=2002)
dft$variable<-as.factor(dft$variable)
str(dft)

pbrin<-ggplot(subset(dft,variable=="prop_brin"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRIN')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")
pbrin
pbrte<-ggplot(subset(dft,variable=="prop_brte"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='BRTE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbrca<-ggplot(subset(dft,variable=="prop_brca"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRCA')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plomu<-ggplot(subset(dft,variable=="prop_lomu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='LOMU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plope<-ggplot(subset(dft,variable=="prop_lope"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='LOPE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

peltr<-ggplot(subset(dft,variable=="prop_eltr"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='ELTR')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbocu<-ggplot(subset(dft,variable=="prop_bocu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='BOCU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pscsc<-ggplot(subset(dft,variable=="prop_scsc"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='SCSC')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")



presence<-(pbrca/plomu/plope/peltr|pbocu/pscsc/pbrin/pbrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

presence


llomu<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lomu_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lomu_mean - PctC_lomu_se, 
                    ymax = PctC_lomu_mean + PctC_lomu_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOMU')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")
llomu
llope<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lope_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lope_mean - PctC_lope_se, 
                    ymax = PctC_lope_mean + PctC_lope_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbrte<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brte_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRTE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbrin<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brin_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brin_mean - PctC_brin_se, 
                    ymax = PctC_brin_mean + PctC_brin_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRIN')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

leltr<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_eltr_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='ELTR')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


lbocu<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_bocu_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_bocu_mean - PctC_bocu_se, 
                    ymax = PctC_bocu_mean + PctC_bocu_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BOCU')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lscsc<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_scsc_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_scsc_mean - PctC_scsc_se, 
                    ymax = PctC_scsc_mean + PctC_scsc_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='SCSC')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


lbrca<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brca_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRCA')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")





abundance_lines<-(lbrca/llomu/llope/leltr|lbocu/lscsc/lbrin/lbrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

abundance_lines

alope<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_lope,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

alomu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_lomu,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOMU')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


abrca<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brca,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRCA')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

abrin<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brin,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRIN')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


abrte<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brte,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRTE')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

aeltr<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_eltr,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='ELTR')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

abocu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_bocu,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BOCU')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

ascsc<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_scsc,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='SCSC')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



abundance<-(abrca/alomu/alope/aeltr|abocu/ascsc/abrin/abrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

abundance

pbrca|abrca




















full_d$reburn<-ifelse(full_d$year>2009,"post","pre")


#Q1) Treatment effectiveness
library(emmeans)



bare_pct<-glmmTMB(PctC_bare ~ TRT  + reburn + (1|Transect_id) + (1|year), data=full_d,family="gaussian")
summary(bare_pct)
r.squaredGLMM(bare_pct)
plot(simulateResiduals(bare_pct))
bare_em<-emmeans(bare_pct, ~TRT*reburn)
contrast(bare_em)
bare_em$contrasts
bare_em

contrast(bare_em)



gram_pct<-glmmTMB(PctC_gram ~ TRT + yearfac + (1|Transect_id), data=full_d)
summary(gram_pct)
r.squaredGLMM(gram_pct)
plot(simulateResiduals(gram_pct))

gram_em<-emmeans(gram_pct, ~ TRT |yearfac, adjust= "Tukey")
cld(gram_em)

exotic_pct<-glmmTMB(PctC_exotic ~ TRT + year + (1|Transect_id), data=full_d)
summary(exotic_pct)
r.squaredGLMM(exotic_pct)
plot(simulateResiduals(exotic_pct))

exot_em<-emmeans(exotic_pct, ~ TRT |yearfac, adjust= "Tukey")
cld(exot_em)





#Individual species

full_d$sqbrin<-sqrt(full_d$PctC_brin)

brin_pct<-glmmTMB(PctC_brin ~ TRT    +  (yearfac)+ (1|Transect_id) , data=full_d,family="gaussian")
summary(brin_pct)
performance::r2(brin_pct)
performance::check_singularity(brin_pct)
plot(simulateResiduals(brin_pct))

glht(brin_pct,l)

chuh<-emmeans(brin_pct,~yearfac|TRT)
mult<-as.glht(chuh)
summary(mult)



lol_pct<-glmmTMB(PctC_lolium ~  TRT +(yearfac)+  (1|Transect_id) ,data=full_d,family="gaussian")
summary(lol_pct)
r.squaredGLMM(lol_pct)
plot(simulateResiduals(lol_pct))

chuh<-emmeans(lol_pct,~TRT|yearfac)
mult<-as.glht(chuh)
mult
summary(mult)
confint(mult)
contrast(chuh)

brte_pct<-glmmTMB(PctC_brte ~ TRT   +(yearfac) +(1|Transect_id) , data=full_d, family="binomial")
summary(brte_pct)
r.squaredGLMM(brte_pct)
plot(simulateResiduals(brte_pct))


chuh<-emmeans(brca_pct,~TRT*yearfac)
cld(chuh)
pairs(chuh)
mult<-as.glht(chuh)
summary(mult)
contrast(chuh)

?kruskal.test
kruskal.test(full_d,PctC_brte~yearfac)

brca_pct<-glmmTMB(pa_brca ~ TRT   + (yearfac) + (1|Transect_id) , data=full_d, family="binomial")
summary(brca_pct)
r.squaredGLMM(brca_pct)
plot(simulateResiduals(brca_pct))


eltr_pct<-glmmTMB(PctC_eltr ~ TRT  + (1|year)+(1|Transect_id) , data=full_d, family="binomial")
summary(eltr_pct)
r.squaredGLMM(eltr_pct)
plot(simulateResiduals(eltr_pct))


bocu_pct<-glmmTMB(PctC_bocu ~ TRT  + (1|year) + (1|Transect_id), data=full_d, family="binomial")
summary(bocu_pct)
performance::r2(bocu_pct)
plot(simulateResiduals(bocu_pct))


scsc_pct<-glmmTMB(PctC_scsc ~ TRT  + (1|year) + (1|Transect_id), data=full_d, family="binomial")
summary(scsc_pct)
r.squaredGLMM(scsc_pct)
plot(simulateResiduals(scsc_pct))


###plots

topocor_site

d_mean_topo<-merge(d_mean,topocor_site)



early_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_lolium_mean, lty = TRT),col=col2)+
  geom_errorbar(aes(x=year, ymin = PctC_lolium_mean - PctC_lolium_se, 
                    ymax = PctC_lolium_mean + PctC_lolium_se ),col=col2) +
  geom_line(mapping = aes(x=year,y=PctC_brca_mean, lty = TRT),col=col6)+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se ),col=col6) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #facet_grid(cols=vars(stand_replacing)) + 
  labs(lty = "Treatment", y = "% Cover Early Responders") +
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_lolium),col=col2,alpha=0.25)+
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_brca),col=col6,alpha=0.25)+
  scale_color_manual(name="Species",breaks=c("Lolium","BRCA"),values=c("Lolium"="tomato",'BRCA'="steelblue"))+
  theme_bw()+
  theme(legend.position = "none")


early_plot

native_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_bocu_mean, lty = TRT),col=pal[40])+
  geom_errorbar(aes(x=year, ymin = PctC_bocu_mean - PctC_bocu_se, 
                    ymax = PctC_bocu_mean + PctC_bocu_se ),col=pal[40]) +
  geom_line(mapping = aes(x=year,y=PctC_scsc_mean, lty = TRT),col=pal[30])+
  geom_errorbar(aes(x=year, ymin = PctC_scsc_mean - PctC_scsc_se, 
                    ymax = PctC_scsc_mean + PctC_scsc_se),col=pal[30]) +
  geom_line(mapping = aes(x=year,y=PctC_eltr_mean, lty = TRT),col=pal[20])+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se),col=pal[20]) +
  geom_line(mapping = aes(x=year,y=PctC_brca_mean, lty = TRT),col=pal[10])+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se ),col=pal[10]) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  geom_point(d_annual, mapping = aes(x=year, y = PctC_bocu), col=pal[40],alpha=0.25)+
  geom_point(d_annual, mapping = aes(x=year, y = PctC_scsc), col=pal[30],alpha=0.25)+
  geom_point(d_annual, mapping = aes(x=year, y = PctC_eltr), col=pal[20],alpha=0.25)+
  geom_point(d_annual, mapping = aes(x=year, y = PctC_brca),col= pal[10],alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1, "Agency")+
  labs(lty = "Treatment", y = "% Native-seeded Cover") +
  theme_bw() +
  theme(legend.position = "bottom")


brin_plot<-ggplot(d_mean)+
  geom_line(mapping = aes(x=year,y=PctC_brin_mean, lty = TRT),col=pal[10])+
  geom_errorbar(aes(x=year, ymin = PctC_brin_mean - PctC_brin_se, 
                    ymax = PctC_brin_mean + PctC_brin_se ),col=pal[10]) +
  geom_line(mapping = aes(x=year,y=PctC_brte_mean, lty = TRT),col=pal[40])+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se ),col=pal[40]) +
  geom_line(mapping = aes(x=year,y=PctC_lolium_mean, lty = TRT),col=pal[50])+
  geom_errorbar(aes(x=year, ymin = PctC_lolium_mean - PctC_lolium_se, 
                    ymax = PctC_lolium_mean + PctC_lolium_se ),col=pal[50]) +
  geom_point(d_annual, mapping = aes(x=year, y = PctC_lolium),col=pal[50],alpha=0.25)+
  geom_point(d_annual, mapping = aes(x=year, y = PctC_brte),col=pal[40],alpha=0.25)+
  geom_point(d_annual, mapping = aes(x=year, y = PctC_brin),col=pal[10],alpha=0.25)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  facet_grid(cols=vars(RevBI)) + 
  labs(lty = "Treatment", y = "% Non-natives Cover") +
  #scale_color_manual("Spp")+
  #geom_point(d_annual, mapping = aes(x=year, y = PctC_brin),alpha=0.25)+
  scale_color_scico_d(palette = "vik",begin=.25,end=.7,direction=-1)+
  theme_bw()+
  theme(legend.position = "none")




abundance_lines|presence


(pbrca|lbrca)/(plomu|llomu)/(pbrte|lbrte)/(pbrin|lbrin)|

(peltr|leltr)/(pbocu|lbocu)/(pscsc|lscsc)/(plope|llope)

(pbrca/lbrca)|(plomu/llomu)|(pbrte/lbrte)|(pbrin/lbrin) + plot_layout(guides = "collect") & theme(legend.position = 'right')



png("./Figures/EDA/presence.png",height = 20, width = 15, units = "cm", res=300, bg='white')
presence
#native_plot/brin_plot


dev.off()

png("./Figures/EDA/abundance.png",height = 20, width = 15, units = "cm", res=300, bg='white')
abundance_lines
#native_plot/brin_plot


dev.off()

png("./Figures/EDA/groundcover.png",height = 9, width = 27, units = "cm", res=300, bg='white')
lgram|lexotic|lbare

dev.off()


png("./Figures/EDA/mainspp.png",height = 15, width = 30, units = "cm", res=300, bg='white')

(pbrca/lbrca)|(plomu/llomu)|(pbrin/lbrin)|(pbrte/lbrte) + plot_layout(guides = "collect") & theme(legend.position = 'right')

dev.off()
png("./Figures/EDA/fewspp.png",height = 15, width = 30, units = "cm", res=300, bg='white')

(pbocu/lbocu)|(peltr/leltr)|(pscsc/lscsc) + plot_layout(guides = "collect") & theme(legend.position = 'right')
dev.off()

png("./Figures/EDA/groundcover.png",height = 20, width = 20, units = "cm", res=300, bg='white')
(lbare/lgram|lforb/lexotic)
 plot_layout(guides = "collect") & theme(legend.position = 'bottom')
dev.off()





















####MAPS



########MAPS############

plot.df<-as.data.frame(pts,xy=T)

plot(plot.df)
names(plot.df)[2]<-"Transect_id"

red<-merge.data.frame(plot.df,d_annual)
red<-subset(red,year!=2002)

ext<-extent(buffer(pts,1500))

ext

plot(band)

red$brin_abs<-ifelse(red$PctC_brin>0,1,0)

red$brin_abs<-as.factor(red$brin_abs)

red_brin_prs<-subset(red,PctC_brin>0)
red_brin_abs<-subset(red,PctC_brin==0)

red_brte_prs<-subset(red,PctC_brte>0)
red_brte_abs<-subset(red,PctC_brte==0)

red_lolium_prs<-subset(red,PctC_lolium>0)
red_lolium_abs<-subset(red,PctC_lolium==0)

red_brca_prs<-subset(red,PctC_brca>0)
red_brca_abs<-subset(red,PctC_brca==0)

red_eltr_prs<-subset(red,PctC_eltr>0)
red_eltr_abs<-subset(red,PctC_eltr==0)

red_scsc_prs<-subset(red,PctC_scsc>0)
red_scsc_abs<-subset(red,PctC_scsc==0)

red_bocu_prs<-subset(red,PctC_bocu>0)
red_bocu_abs<-subset(red,PctC_bocu==0)



library(ggspatial)

year_facet=data.frame(year=2008)
red_brin_pre<-subset(red_brin_prs,year<2009)
red_brin_pst<-subset(red_brin_prs,year>2009)

red_brin_pre_ab<-subset(red_brin_abs,year<2009)
red_brin_pst_ab<-subset(red_brin_abs,year>2009)

red_brin_prs$fact
red_brin_prs$fact<-red_brin_prs$PctC_brin
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.50001 & red_brin_prs$fact < 1,"5",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact <= 0.05,"1",red_brin_prs$fact)

red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.049 & red_brin_prs$fact <= 0.15,"2",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.15 & red_brin_prs$fact <= 0.33,"3",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.33 & red_brin_prs$fact <= 0.50,"4",red_brin_prs$fact)
str(red_brin_prs)
as.factor(red_brin_prs$fact)

?scale_color_scico_d

red_brin_prs$fact

new_brin_pre<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="gray",lty=1,size=1.1,alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  scale_color_scico(palette = "vikO",begin=0.45,end=0)+
  scale_shape_manual(values=c(17,16),"")+
  geom_point(red_brin_pre,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT),alpha=0.9,size=3,show.legend = F)+
  
  new_scale(new_aes = "color")+
  new_scale(new_aes = "shape")+
  
  scale_shape_manual(values=c(2,1),"Treatment")+
  
  
  geom_point(red_brin_pre_ab,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT),alpha=0.75,size=2,show.legend = F)+
  scale_color_scico(palette = "vikO",begin=0.85,end=0.86)+
  facet_wrap(~year)+
  coord_cartesian(xlim=c(370070.1      ,380610.1      ), ylim=c(3953533      ,3962753     ))+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())+
  annotation_scale(data=year_facet)+
  theme_void()
  
new_brin_pre


new_brin_pst<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray",lty=2,size=0.75,pattern_alpha=0.25)+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="gray",lty=1,size=1,alpha=0.25)+
  scale_color_scico(palette = "vikO",begin=0.45,end=0,"% BRIN")+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.1)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+  scale_shape_manual(values=c(17,16), "")+
  geom_point(red_brin_pst,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT),size=3,alpha=0.9)+
  scale_shape_manual(values=c(17,16),"Treatment")+
  new_scale(new_aes = "shape")+
  new_scale(new_aes = "color")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(red_brin_pst_ab,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT),alpha=0.75,size=3,show.legend=FALSE)+
  scale_color_scico(palette = "vikO",begin=0.85,end=0.86)+
  facet_wrap(~year)+
  #coord_cartesian(xlim=c(370570.1    ,380110.1    ), ylim=c(3954033    ,3962253   ))+
  coord_cartesian(xlim=c(370070.1      ,380610.1      ), ylim=c(3953533      ,3962753     ))+
  theme_void()+
  theme(legend.position = "bottom")+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5,pch=c(2,1))))

new_brin_pst

png("./Figures/EDA/brin_spatial.png",height = 25, width = 32, units = "cm", res=300, bg='white')
(new_brin_pre/new_brin_pst)
dev.off()


plot_layout(guides = "collect") & theme(legend.position = 'bottom')

brin_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="gray",lty=1,size=2,alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_point(red_brin_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),size=4,alpha=0.5,show.legend=FALSE)+
  #scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16), "")+
  scale_color_scico_d(palette="romaO",direction=1,begin=0.5,end=1,"% Cover")+
  
  #scale_size_continuous(range = c(1.5,4))+
  new_scale(new_aes = "shape")+
  geom_point(red_brin_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=brin_abs,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1),"Treatment")+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brin,pch=TRT,col=brin_abs,size=PctC_brin),alpha=0.5)+
  #scale_alpha(range = c(0.4,0.75))+
  #scale_color_manual(values=c("NA","black"))+
  #scale_color_scico_d(palette="davos",direction=-1,begin=0,end=1,"% Cover")+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Smooth Brome (BRIN)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(370570.1 ,380110.1 ), ylim=c(3954033 ,3962253 ))+
  theme_void()+
  theme(legend.position = c(0.95,0.67))+
  annotation_scale(data=year_facet)+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5,pch=c(17,16))))+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())


brin_spatial


lol_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_lolium_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_lolium_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_lolium,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_lolium,pch=TRT,col=lolium_abs,size=PctC_lolium),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="oslo",direction=1,begin=1,end=0)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Ryegrass (Lolium sp.)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())


brte_spatial<-ggplot()+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_brte_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_brte_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brte,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brte,pch=TRT,col=brte_abs,size=PctC_brte),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Cheatgrass (BRTE)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

brca_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_brca_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_brca_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brca,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brca,pch=TRT,col=brca_abs,size=PctC_brca),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Mountain Brome (BRCA)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

eltr_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_eltr_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_eltr_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_eltr,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_eltr,pch=TRT,col=eltr_abs,size=PctC_eltr),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Smooth Wheatgrass (ELTR)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

bocu_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_bocu_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_bocu_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_bocu,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_bocu,pch=TRT,col=bocu_abs,size=PctC_bocu),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Sideoats Gramma (BOCU)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())



scsc_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_scsc_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_scsc,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  new_scale(new_aes = "shape")+
  geom_point(red_scsc_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_scsc,pch=TRT,col=scsc_abs,size=PctC_scsc),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  #scale_color_scico(palette = "grayC",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Little Bluestem (scsc)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

scsc_spatial


png("./Figures/EDA/brin_spatial.png",height = 20, width = 32, units = "cm", res=300, bg='white')
brin_spatial
dev.off()

png("./Figures/EDA/lol_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
lol_spatial
dev.off()


png("./Figures/EDA/brte_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
brte_spatial
dev.off()

png("./Figures/EDA/brca_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
brca_spatial
dev.off()

png("./Figures/EDA/bocu_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
bocu_spatial
dev.off()

png("./Figures/EDA/eltr_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
eltr_spatial
dev.off()

png("./Figures/EDA/scsc_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
scsc_spatial
dev.off()



