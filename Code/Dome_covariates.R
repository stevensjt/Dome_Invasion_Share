library(raster)
library(data.table)
getwd()


SPEI<-read.csv("C:/Users/awion/Documents/GitHub/Dome_Invasion_Share/Data/Spatial/Dome Transect stakes/jemez_3mspei.csv")
SPEI<-SPEI[,-1]
names(SPEI)<-c("Transect_id","year","Month","spei")

AET<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/AET_1981-2010.tif")
CWD<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/CWD_1981-2010.tif")
pts<-shapefile("Data/Spatial/Dome Transect stakes/Dome_Transect_pts.shp")

elev<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/USGS_13_n36w107_20220801.tif")

clay<-raster("C:/Users/awion/Downloads/polaris_clay.tif")
silt<-raster("C:/Users/awion/Downloads/polaris_silt.tif")
sand<-raster("C:/Users/awion/Downloads/polaris_sand.tif")
thetas<-raster("C:/Users/awion/Downloads/polaris_thetas.tif")
thetar<-raster("C:/Users/awion/Downloads/polaris_thetar.tif")


#see supplementary material for 2022 land paper "spatial estimates of soil moisture"
#awc = saturated water content - residual water content 
awc<-thetas-thetar



elv.pts<-extract(elev,pts)
cwd.pts<-extract(CWD,pts)
aet.pts<-extract(AET,pts)
cly.pts<-extract(clay,pts)
snd.pts<-extract(sand,pts)
slt.pts<-extract(silt,pts)
awc.pts<-extract(awc,pts)

topo<-cbind.data.frame(pts$IDENT,elv.pts,cwd.pts,aet.pts,cly.pts,snd.pts,slt.pts,awc.pts)
names(topo)<-c("Transect_id","elevation","cwd","aet","clay","sand","silt","awc")


yo<-merge(topo,SPEI)

write.csv(yo,"covariates.csv")










cwd_crop<-crop(CWD,pts)


ref<-crs(elev)
pts_ref<-spTransform(pts,ref)


elev<-projectRaster(elev,crs=ref)
elv_crop<-crop(elev,pts_ref)

plot(elv_crop,col = terrain.colors(255))
plot(cwd_crop)
plot(pts)














climdir<-("C:/Users/awion/Documents/GIS_data/PRISM/")
years<-seq(1996,2019,1)
months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

poly<-pts

poly[[2]]

# TMAX DATA extract raster data from overlying polygon                     
vpd_data<-vector()
for(i in 1:length(years)){
  for(j in 1:length(months)){
    raster<-paste(climdir,'vpdmax/PRISM_vpdmax_stable_4kmM3_',years[i],months[j],'_bil.bil',sep="")
    rast <- raster(raster)
    ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
    vpd<-ext.poly[,2]
    site<-poly[[2]]
    month <- rep(months[j],length(ext.poly[,2]))
    year <- rep(years[i],length(ext.poly[,2]))
    vpd_site<-cbind(as.character(site),vpd,month,year)
    vpd_data<-rbind(vpd_data,vpd_site)}}

vpd_data

# TMAX DATA extract raster data from overlying polygon                     
ppt_data<-vector()
for(i in 1:length(years)){
  for(j in 1:length(months)){
    raster<-paste(climdir,'ppt/PRISM_ppt_stable_4kmM3_',years[i],months[j],'_bil.bil',sep="")
    rast <- raster(raster)
    ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
    ppt<-ext.poly[,2]
    site<-poly[[2]]
    month <- rep(months[j],length(ext.poly[,2]))
    year <- rep(years[i],length(ext.poly[,2]))
    ppt_site<-cbind(as.character(site),ppt,month,year)
    ppt_data<-rbind(ppt_data,ppt_site)}}

ppt_data

climate<-(cbind.data.frame(ppt_data,vpd_data))

climate<-climate[,-c(3:5)]
names(climate)<-c("Transect_id","ppt","vpd","month","year")

climate$ppt<-as.numeric(as.character(climate$ppt))
climate$vpd<-as.numeric(as.character(climate$vpd))
climate$month<-as.numeric(as.character(climate$month))
climate$year<-as.numeric(as.character(climate$year))

climate<-as.data.table(climate)

#climate<-climate[,.(ppt=mean(ppt),vpd=mean(vpd)),by=c("month","year")]

climate

winter<-subset(climate,month<=5)
winter<-subset(climate,month>2)


summer<-climate[month > 5]
summer<-summer[month <= 8]

win<-winter[,.(winter_vpd=mean(vpd),winter_ppt=sum(ppt)),by=c("year","Transect_id")]
sum<-summer[,.(summer_vpd=mean(vpd),summer_ppt=sum(ppt)),by=c("year","Transect_id")]


clim<-cbind(sum,win)
clim<-clim[,-c(5,6)]


yo<-merge(clim,topo,by="Transect_id")

full_d<-merge(d_annual_fs,yo,by=c("Transect_id","year"))


ggplot(full_d)+
  geom_line(mapping=aes(x=year,y=summer_ppt,col="blue"))+
  geom_line(mapping=aes(x=year,y=winter_ppt,col="red"))


