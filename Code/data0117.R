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

conchas_lat<-spTransform(conchas,crs(elev))

elev_crop<-crop(elev, conchas_lat)
plot(elev_crop)


cont<-rasterToContour(elev_crop,maxpixels = 1000000)
cont<-spTransform(cont,crs(pts))
plot(cont)
cont.line<-fortify(cont)


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





###plot data

stdErr <- function(x) sqrt(var(x, na.rm = T)/length(na.exclude(x)))



svTransform <- function(y)
{
  n <- length(y)
  transformed <- (y * (n-1) + 0.5)/n
  return(transformed)
}




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

d$newtrt<-as.factor(ifelse(d$FS_BAND=="FS"&d$TRT=="Unseeded","Unseeded Adjacent",d$TRT))

d<-as.data.table(d)



#factor variable, possibly delete later on
trt<-d[,.(FS_BAND=unique(FS_BAND),TRT=unique(TRT),RevBI=unique(RevBI)),by="Transect_id"]



d<-d[,.(PctC=sum(PctC), PctC_basal=(sum(Basal_cm))/(Transect_cm), 
        Total_cm = sum(Total_cm), Basal_cm = sum(Basal_cm), 
        Canopy_cm=sum(Canopy_cm)),
     by=c("Transect_id","year","Code","RevBI","FS_BAND","TRT","newtrt","Full_name","Origin","Transect_cm","Growth_form")]


#rounding errors result in Pct greater than 100 in 1 Brin plot

d<-subset(d,year!=2002)
d<-subset(d,year!=2016)






#some problems
#a number of bare hits labeled as growth form = F
d$Growth_form<-ifelse(d$Full_name=="Bare" & d$Growth_form=="F","B",d$Growth_form)

#much less than 1 percent of few transects
d$Growth_form<-ifelse(d$Full_name=="Laennecia schiedeana" & d$Growth_form=="B","F",d$Growth_form)
d$Growth_form<-ifelse(d$Full_name=="Rhus glabra","S",d$Growth_form)
#couple plots with a lot of mentzelia
d$Growth_form<-ifelse(d$Growth_form=="0","F",d$Growth_form)



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
    exotic_sppratio = (length(which(Origin=="exotic"))/length(which(Origin=="native"))),
    exotic_coverratio = (sum(Canopy_cm[Origin == "exotic"], na.rm = T)/mean(Transect_cm))/(sum(Canopy_cm[Origin == "native"], na.rm = T)/mean(Transect_cm)),
    
    PctC_plnt = sum(Canopy_cm[grep("G|F",Growth_form)], na.rm = T)/mean(Transect_cm),
#    PctC_plntc = ((sum(Canopy_cm[Origin == "exotic"], na.rm = T)+sum(Canopy_cm[Origin == "native"], na.rm = T))/mean(Transect_cm)),
    
    
    
    PctC_natv = sum(Canopy_cm[Origin == "native"], na.rm = T)/mean(Transect_cm),
    PctC_nfor = sum(Canopy_cm[Origin == "native" & Growth_form=="F"], na.rm = T)/mean(Transect_cm),
    PctC_efor = sum(Canopy_cm[Origin == "exotic" & Growth_form=="F"], na.rm = T)/mean(Transect_cm),
    PctC_egra = sum(Canopy_cm[Origin == "exotic" & Growth_form=="G"], na.rm = T)/mean(Transect_cm),
    PctC_ngra = sum(Canopy_cm[Origin == "native" & Growth_form=="G"], na.rm = T)/mean(Transect_cm),
    
    
    PctC_exot = sum(Canopy_cm[Origin == "exotic"], na.rm = T)/mean(Transect_cm),
    PctC_coca = sum(Canopy_cm[grep("coca",Code)], na.rm=T)/mean(Transect_cm),
    PctC_lolium = sum(Canopy_cm[grep("Lolium",Full_name)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_lomu = sum(Canopy_cm[grep("lomu",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_lope = sum(Canopy_cm[grep("lope",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brin = sum(Canopy_cm[grep("brin",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brte = sum(Canopy_cm[grep("brte",Code)], na.rm = T)/mean(Transect_cm), #deprecate?
    PctC_brca = sum(Canopy_cm[grep("brca2",Code)], na.rm = T)/mean(Transect_cm),
    PctC_eltr = sum(Canopy_cm[grep("eltr",Code)], na.rm = T)/mean(Transect_cm),
    PctC_bocu = sum(Canopy_cm[grep("bocu",Code)], na.rm = T)/mean(Transect_cm),
    PctC_scsc = sum(Canopy_cm[grep("scsc",Code)], na.rm = T)/mean(Transect_cm),
    
    PctC_bare = sum(Basal_cm[grep("Bare",Full_name)], na.rm =T)/mean(Transect_cm),
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


d_annual$exotic_covrat<-d_annual$PctC_exot/d_annual$PctC_natv
d_annual$extoci_spprat<-d_annual$Richness_exotic/d_annual$Richness_native


d_annual[,c(14,15,47,48)]

full_d[,30]


svTransform(full_d$PctC_brin)

#fixing rounding errors
d_annual$PctC_plnt<-ifelse(d_annual$PctC_plnt>1,1,d_annual$PctC_plnt)
d_annual$PctC_natv<-ifelse(d_annual$PctC_natv>1,1,d_annual$PctC_natv)
d_annual$PctC_egra<-ifelse(d_annual$PctC_egra>1,1,d_annual$PctC_egra)
d_annual$PctC_ngra<-ifelse(d_annual$PctC_ngra>1,1,d_annual$PctC_ngra)

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
    
    PctC_plnt_mean = mean(PctC_plnt), 
    PctC_plnt_se = stdErr(PctC_plnt), 
    
    PctC_natv_mean = mean(PctC_natv), 
    PctC_natv_se = stdErr(PctC_natv), 
    
    PctC_nfor_mean = mean(PctC_nfor), 
    PctC_nfor_se = stdErr(PctC_nfor), 
    
    PctC_ngra_mean = mean(PctC_ngra), 
    PctC_ngra_se = stdErr(PctC_ngra), 
    
    PctC_egra_mean = mean(PctC_egra), 
    PctC_egra_se = stdErr(PctC_egra), 
    
    PctC_efor_mean = mean(PctC_efor), 
    PctC_efor_se = stdErr(PctC_efor), 
    
    PctC_brin_mean = mean(PctC_brin), 
    PctC_brin_se = stdErr(PctC_brin), 
    
    PctC_brte_mean = mean(PctC_brte), 
    PctC_brte_se = stdErr(PctC_brte), 
    
    PctC_lolium_mean = mean(PctC_lolium), 
    PctC_lolium_se = stdErr(PctC_lolium),
    
    PctC_lomu_mean = mean(PctC_lomu), 
    PctC_lomu_se = stdErr(PctC_lomu),
    
    PctC_coca_mean = mean(PctC_coca), 
    PctC_coca_se = stdErr(PctC_coca),
    
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
    
    PctC_exoticsppratio_mean = mean(exotic_sppratio),
    PctC_exoticsppratio_se = stdErr(exotic_sppratio),
    
    PctC_exoticcovratio_mean= mean(exotic_coverratio),
    PctC_exoticcovratio_se= stdErr(exotic_coverratio),
    
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

hist(full_d$PctC_egra)

ggplot(full_d)+
  geom_histogram(mapping=aes(x=exotic_coverratio))+
  facet_wrap(~year)


#early responders
d97<-subset(d_annual,year==1997)
d19<-subset(d_annual,year==2019)


late_stage<-d19

early_factor<-d97
early_factor$early_resp<-early_factor$PctC_lolium+early_factor$PctC_brca
str(early_factor)
earlyd97<-early_factor[,c(1:6,24:26,49)]

earlyd97$TRT_response<-ifelse(earlyd97$early_resp > 0.33,1,0)
earlyd97$TRT_any<-ifelse(earlyd97$early_resp > 0,1,0)


#factor data frame
trtresponse<-earlyd97[,c(1,5,6,11,12)]
trtresponse$TRT_response<-as.factor(trtresponse$TRT_response)
trtresponse$TRT_any<-as.factor(trtresponse$TRT_any)

names(d_annual)

#ground cover for ordination
herb_cover<-d_annual[,c(1,2,33:36)]

names(herb_cover)<-c("Transect_id","year","% Bare","% Grass","% forb","% shrub")
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

full_d<-subset(full_d,year!=2002)
full_d<-subset(full_d,yearfac!="2002")

full_d<-subset(full_d,year!=2016)
full_d<-subset(full_d,yearfac!="2016")


d_mean<-subset(d_mean,year!=2002)
d_mean<-subset(d_mean,year!=2016)

d_annual<-subset(d_annual,year!=2002)
d_annual<-subset(d_annual,year!=2016)


mod_egra<-glmmTMB(svTransform(PctC_egra)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))
mod_ngra<-glmmTMB(svTransform(PctC_ngra)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))
mod_nfor<-glmmTMB(svTransform(PctC_nfor)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))
mod_efor<-glmmTMB(svTransform(PctC_efor)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))
mod_plnt<-glmmTMB(svTransform(PctC_plnt)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))
mod_bare<-glmmTMB(svTransform(PctC_bare)~TRT*yearfac +(1|Transect_id),data=full_d,family=beta_family(link="logit"))

plot(simulateResiduals(mod_egra))
plot(simulateResiduals(mod_ngra))
plot(simulateResiduals(mod_nfor))
plot(simulateResiduals(mod_efor))
plot(simulateResiduals(mod_plnt))
plot(simulateResiduals(mod_bare))

emmeans(mod_egra,pairwise~TRT|yearfac)
emmeans(mod_ngra,pairwise~TRT|yearfac)
emmeans(mod_nfor,pairwise~TRT|yearfac)
emmeans(mod_efor,pairwise~TRT|yearfac)
emmeans(mod_plnt,pairwise~TRT|yearfac)
emmeans(mod_bare,pairwise~TRT|yearfac)




#models
names(full_d)
tst<-full_d[,c(1,2,4,14:36)]


tstmelt<-melt(tst,id.vars = c("Transect_id","TRT","year"))
tstmelt<-as.data.table(tstmelt)

meanburn<-tstmelt[,.(value=mean(value)),by=c("TRT","Transect_id","variable")]


yrs<-(unique(tstmelt$year))
var<-as.character(unique(tstmelt$variable))


full_w<-vector()

for(i in 1:length(yrs)){
  for(j in 1:length(var)){
    sub<-subset(tstmelt,year==yrs[i])
    su<-subset(sub,variable==var[j])
    sw<-wilcox.test(su$value~su$TRT)
    fu<-cbind(var[j],yrs[i],sw$p.value)
    full_w<-rbind(fu,full_w)
  }
}

mood<-glmmTMB(exotic_coverratio~TRT*yearfac+(1|Transect_id),data=full_d)
mood
summary(mood)
emmeans(mood,pairwise~TRT|yearfac)
plot(simulateResiduals(mood))


library(PNWColors)

redblu<-pnw_palette("Sunset2",n=100)
redblu
pal2<-redblu[c(1,100)]

library(ggsignif)


















ggplot(meanburn,mapping=aes(x=reburn,y=value,col=TRT))+
  geom_boxplot()+
  scale_color_manual(values=pal2,limits=rev(levels(tstmelt$reburn)))+
  geom_signif(comparisons=list(c("pre","post",map_signif_level = TRUE)))+
  geom_signif(y_position = c(0.4, 0.6), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
              annotation = c("**", "NS"), tip_length = 0) +
  facet_wrap(~variable)






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

full_d$betabrin






mod<-glmmTMB((PctC_egra)~TRT*yearfac +(1|Transect_id),data=full_d,family="gaussian")

summary(mod)
hist(full_d$PctC_bare,breaks=100)
plot(simulateResiduals(mod))
emmeans(mod,pairwise~TRT|yearfac)

performance::r2(mod)


mod<-glmmTMB(betabrte~yearfac*TRT,data=full_d,family="gaussian")

summary(mod)
library(emmeans)
plot(simulateResiduals(mod))
contrast(emmeans(mod,trt.vs.ctrl~TRT*yearfac))



