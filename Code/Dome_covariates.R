library(raster)
library(data.table)
library(ggplot2)
library(ggnewscale)
library(ggpattern)
library(scico)

getwd()


SPEI<-read.csv("C:/Users/awion/Documents/GitHub/Dome_Invasion_Share/Data/Spatial/Dome Transect stakes/jemez_3mspei.csv")
SPEI<-SPEI[,-1]
names(SPEI)<-c("Transect_id","year","Month","spei")


SPEI<-as.data.table(SPEI)
SPEI$yfac<-as.factor(SPEI$year)
SPEI$mfac<-as.factor(SPEI$Month)


winter<-SPEI[Month<=5]
winter<-winter[Month>2]


summer<-SPEI[Month > 5]
summer<-summer[Month <= 8]


win<-winter[,.(winter_spei=mean(spei)),by=c("year")]
sum<-summer[,.(summer_spei=mean(spei)),by=c("year")]

ann<-SPEI[,.(annual_spei=mean(spei)),by=c("year")]

clim<-cbind(sum,win,ann)


clim<-clim[,-c(3,5)]




test<-SPEI[,.(spei=mean(spei)),by=c("yfac")]

test<-SPEI[,.(winter_spei=mean(spei)),by=c("yfac")]

sub<-test[-(1:15),]
sub$seq<-seq(1996,2019,1)

ggplot(clim)+
  geom_line(aes(x=year,y=winter_spei),col="blue")+
  geom_line(aes(x=year,y=summer_spei),col="red")
  




AET<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/AET_1981-2010.tif")
CWD<-raster("C:/Users/awion/Documents/PhData/PhD Data/Cones/PIPO/CWD_1981-2010.tif")
pts<-shapefile("Data/Spatial/Dome Transect stakes/Dome_Transect_pts.shp")

elev<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/USGS_13_n36w107_20220801.tif")
slop<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/jemez_slope")
aspt<-raster("C:/Users/awion/Documents/GIS_Data/rasters_USGS10m.tar/jemez_aspect")


clay<-raster("C:/Users/awion/Downloads/polaris_clay.tif")
silt<-raster("C:/Users/awion/Downloads/polaris_silt.tif")
sand<-raster("C:/Users/awion/Downloads/polaris_sand.tif")
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

extract(dome_dnbr,pts_aea)
dome_dnbr<-extract(dome_dnbr,pts_aea)
dome_them<-extract(dome_them,pts_aea)
dome_rnbr<-extract(dome_rnbr,pts_aea)
Transect_id<-pts$IDENT

conchas_dnbr<-extract(conchas_dnbr,pts_aea)
conchas_them<-extract(conchas_them,pts_aea)
conchas_rnbr<-extract(conchas_rnbr,pts_aea)




sites_dnbr<-cbind.data.frame(dome_dnbr,conchas_dnbr)
sites_them<-cbind.data.frame(dome_them,conchas_them)
sites_rnbr<-cbind.data.frame(dome_rnbr,conchas_rnbr)



burn<-cbind(Transect_id,sites_dnbr,sites_rnbr,sites_them)




dome<-spTransform(dome,CRS=utm)
conchas<-spTransform(conchas,CRS=utm)
juan<-spTransform(juan,CRS=utm)
band<-spTransform(band,CRS=utm)



dome.df<-fortify(dome)
juan.df<-fortify(juan)
conchas.df<-fortify(conchas)
band.df<-fortify(band)



folded<-abs(180-abs((aspt-225)))

elv.pts<-extract(elev,pts)
asp.pts<-extract(folded,pts)

slp.pts<-extract(slop,pts)

cwd.pts<-extract(CWD,pts)
aet.pts<-extract(AET,pts)
cly.pts<-extract(clay,pts)
snd.pts<-extract(sand,pts)
slt.pts<-extract(silt,pts)
awc


topo<-cbind.data.frame(pts$IDENT,elv.pts,cwd.pts,aet.pts,cly.pts,snd.pts,slt.pts,slp.pts,asp.pts,awc[,c(2,4)])
names(topo)<-c("Transect_id","elevation","cwd","aet","clay","sand","silt","slope","aspect","awc",'depth')

topo


yo<-merge(topo,SPEI)

#write.csv(yo,"covariates.csv")







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


yo<-merge(clim,topo)

full_d<-merge(d_annual_fs,yo,by=c("Transect_id","year"))


ggplot(full_d)+
  geom_line(mapping=aes(x=year,y=summer_ppt,col="blue"))+
  geom_line(mapping=aes(x=year,y=winter_ppt,col="red"))







########MAPS############

plot.df<-as.data.frame(pts,xy=T)

plot(plot.df)
names(plot.df)[2]<-"Transect_id"

red<-merge.data.frame(plot.df,d_annual)
red<-subset(red,year!=2002)

ext<-extent(dome)

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


buffer(pts,500)

library(ggspatial)

year_facet=data.frame(year=2008)


brin_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_brin_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin,size=PctC_brin),show.legend=FALSE)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16), "")+
  scale_size_continuous(range = c(1.5,4))+
  new_scale(new_aes = "shape")+
  geom_point(red_brin_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1),"Treatment")+
    #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brin,pch=TRT,col=brin_abs,size=PctC_brin),alpha=0.5)+
  #scale_alpha(range = c(0.4,0.75))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="oslo",direction=1,begin=1,end=0,"% Cover")+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Smooth Brome (BRIN)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = c(0.95,0.67))+
  annotation_scale(data=year_facet)+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5)))+
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



