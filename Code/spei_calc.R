



#####################
#####SPEI TIME#######
#####################

library(raster)
library(SPEI)
library(zoo)


climdir<-'C:/Users/awion/Documents/GIS_data/PRISM'

testr<-raster("C:/Users/awion/Documents/GIS_data/PRISM/ppt/PRISM_ppt_stable_4kmM3_198101_bil.bil")

pts<-shapefile("C:/Users/awion/Documents/GitHub/Dome_Invasion_Share/Data/Spatial/Dome Transect stakes/Dome_Transect_pts.shp")

clay<-raster("C:/Users/awion/Downloads/polaris_clay.tif")

clay
poly
latlon<-crs(testr)
#transforming states
poly<-spTransform(pts,latlon)

cr<-extent(clay)
cr

#years<-seq(1901,1980,1)
months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

ppt.rast<-list()


#for(i in 1:length(years)){
#  for(j in 1:length(months)){
#    raster<-paste(climdir,'/M3_ppt/PRISM_ppt_stable_4kmM2_',years[i],months[j],'_bil.bil',sep="")
#    rast <- raster(raster)
#    ext.poly<-crop(rast,cr)
#    #ext.poly <- extract(rast, poly, fun = NULL, na.rm=TRUE, df=FALSE, cellnumbers = TRUE)
#    ppt.rast[[length(ppt.rast)+1]]<-ext.poly
#  }
#}

years<-seq(1981,2019,1)

for(i in 1:length(years)){
  for(j in 1:length(months)){
    raster<-paste(climdir,'/ppt/PRISM_ppt_stable_4kmM3_',years[i],months[j],'_bil.bil',sep="")
    rast <- raster(raster)
    ext.poly<-crop(rast,cr)
    #ext.poly <- extract(rast, poly, fun = NULL, na.rm=TRUE, df=FALSE, cellnumbers = TRUE)
    ppt.rast[[length(ppt.rast)+1]]<-ext.poly
  }
}

ppt.rast<-brick(ppt.rast)


years<-seq(1981,2019,1)
months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

tmean.rast<-list()
for(i in 1:length(years)){
  for(j in 1:length(months)){
    raster<-paste(climdir,'/tmean/PRISM_tmean_stable_4kmM3_',years[i],months[j],'_bil.bil',sep="")
    rast <- raster(raster)
    ext.poly<-crop(rast,cr)
    #ext.poly <- extract(rast, poly, fun = NULL, na.rm=TRUE, df=FALSE, cellnumbers = TRUE)
    tmean.rast[[length(tmean.rast)+1]]<-ext.poly
  }
}

tmean.rast<-brick(tmean.rast)


tm<-tmean.rast

plot(tm[[2]])

dates=seq(as.Date("1981-01-01"), as.Date("2019-12-31"), by="month")
tm<- setZ(tm,dates)
names(tm) <- as.yearmon(getZ(tm))

#thornthwaite ET
th <- function(Tave, lat) {
  as.vector(SPEI::thornthwaite(as.vector(Tave), lat))
} 

a <- raster(tm)
lat <- init(a, "y")

#this line of code seemed to work.  processed a few percent of 50 years of data in ~few minutes.  give it a solid hundred probably, maybe more
#out <- raster::overlay(tm, lat, fun = Vectorize(th))

#this code sort of seemed to work.  Ran forever, and then I just ended it after about 2 hours.  No errors, no warnings.  File seemed intact, everything else.  Just rolled with it.  Think the forloop might be missing something to tell it to stop?
out <- brick(tm, values=FALSE)

for (i in 1:ncell(tm)) {
  out[i] <- th(tm[i], lat[i])
}

bal<-overlay(tm,ppt.rast, fun=function(tm,ppt.rast){return(ppt.rast-tm)})

bal<-ppt.rast-tm

funSPEI <- function(x, scale=3, na.rm=TRUE,...) as.numeric((spei(x, scale=scale, na.rm=na.rm, ...))$fitted)

rstSPEI <- calc(bal, fun = funSPEI)



date_name<-as.character(dates)
date_name<-substr(date_name,start=1,stop=7)
date_name

names(rstSPEI)<-date_name


poly[[2]]

# TMAX DATA extract raster data from overlying polygon                     
spei_data<-vector()

length(rstSPEI[1])


ugh<-extract(rstSPEI,pts)

library(data.table)

ugh<-as.data.frame(ugh)
ugh$plot<-poly[[2]]


melted<-melt(ugh)
melted$year<-substr(melted$variable,start=2,stop=5)
melted$month<-substr(melted$variable,start=7,stop=8)

fum<-as.data.table(melted)
fum<-fum[,.(spei=mean(value)),by=c("plot","year","month")]


write.csv(fum,"C:/Users/awion/Documents/GitHub/Dome_Invasion_Share/Data/Spatial/Dome Transect stakes/jemez_3mspei.csv")
