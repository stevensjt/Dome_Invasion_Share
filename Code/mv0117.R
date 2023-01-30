
#creating data frame of canopy, basal, and total cover for all species

library(vegan)


freq_ <- 
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



#calculate the frequency of occurrence of each spp

spp<-dcast(freq_,year+Transect_id~Full_name,value.var="canopy")
spp<-as.data.frame(spp)

sppred<-spp[,-2]
sppred

spnames<-colnames(sppred)
sppsum<-colSums(sppred)
sitenames<-spp$Transect_id
ref<-cbind.data.frame(sppsum,spnames)

#subsetting out those spp with fewer than 20 observations 
refred<-subset(ref,sppsum>20)
refred<-refred[-1,]

#reference data of spp names with greater than 20 occurences
refname<-refred[,2]

df = spp[,(names(spp) %in% refname)]



#create identical dataset, but of mean canopy pct
sppab<-dcast(freq_,year+Transect_id~Full_name,value.var="canopy_pc",mean)
df = sppab[,(names(sppab) %in% refname)]

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- 0
names(df)

df

#to select just seeded spp
#covs<-c("Bromus inermis","Bromus tectorum","Elymus trachycaulum","Schizachyrium scoparius","Bouteloua curtipendula","Bromus carinatus","Lolium multiflorum","Lolium perenne")
#df2 = df[,!(names(df) %in% covs)]


test<-wilcox.test(late_stage$PctC_bare~late_stage$TRT)
test
  
#items to remove
out<-c("Animal feces","Litter","Bare","Rock","Wood","Moss","Gravel","Pumice soil","Unidentified forb","Unidentified grass","unknown Composite")


#identical to above, but with basal cover, to calculate percent bareground, etc.
gc<-dcast(freq_,year+Transect_id~Full_name,value.var="basal_pc",mean)
gc

dfgc = gc[,(names(gc) %in% out)]
dfgc<-dfgc[,c(2:7,11)]

dfgc[is.nan(dfgc)] <- 0

#gravel and pumice soil are equivalent but categorized differently in 1997/98 vs later years.  combining them into single pumice soil column here
dfgc$Pumice_soil<-dfgc$Gravel+dfgc[,5]
dfgc<-dfgc[,-c(2,5)]
dfgc

dfgc$year<-spp$year
dfgc$Transect_id<-sitenames

#subset years

gc97<-subset(dfgc,year==1997)
gc98<-subset(dfgc,year==1998)
gc08<-subset(dfgc,year==2008)
gc13<-subset(dfgc,year==2013)
gc16<-subset(dfgc,year==2016)
gc19<-subset(dfgc,year==2019)

#add rows

rownames(gc97)<-gc97$site
rownames(gc98)<-gc98$site
rownames(gc08)<-gc08$site
rownames(gc13)<-gc13$site
rownames(gc16)<-gc16$site
rownames(gc19)<-gc19$site

gc97<-gc97[,(7:8)]
gc98<-gc98[,(7:8)]
gc08<-gc08[,(7:8)]
gc13<-gc13[,(7:8)]
gc16<-gc16[,(7:8)]
gc19<-gc19[,(7:8)]


gc97<-merge(gc97,herb_cover)
gc98<-merge(gc98,herb_cover)
gc08<-merge(gc08,herb_cover)
gc13<-merge(gc13,herb_cover)
gc16<-merge(gc16,herb_cover)
gc19<-merge(gc19,herb_cover)



str(topocov)
cor(topocov[,c(4,5,8:17)])
topocov
#select variables for correlation
topocor<-topocov[,c(2,4,7,9:12,14:17)]




topocor_site<-topocov[,c(1,2,3,4,6,9,11:14,16:19)]
topocor_site$TRT<-as.factor(topocor_site$TRT)
topocor_site$FS_TRT<-as.factor(topocor_site$FS_TRT)

topocor97<-merge(gc97,topocor_site)
topocor98<-merge(gc98,topocor_site)
topocor08<-merge(gc08,topocor_site)
topocor13<-merge(gc13,topocor_site)
topocor16<-merge(gc16,topocor_site)
topocor19<-merge(gc19,topocor_site)


topocor97

#remove year and transect id



#spp data for ordination
gc97
#remove litter, shit, etc.


df2 = df[,!(names(df) %in% out)]

reffnames<-names(df2)

#subset by year


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





#ordinate


#set seed
set.seed(747)

#ordination code, one for each year.  using default bray curtis for dissimilarity index.


spp.bcd<-vegdist(df97)
spp.ord97<-metaMDS(df97,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
spp.bcd98<-vegdist(df98)
spp.ord98<-metaMDS(df98,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
spp.bcd08<-vegdist(df08)
spp.ord08<-metaMDS(df08,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
spp.bcd13<-vegdist(df13)
spp.ord13<-metaMDS(df13,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
spp.bcd16<-vegdist(df16)
spp.ord16<-metaMDS(df16,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")
spp.bcd19<-vegdist(df19)
spp.ord19<-metaMDS(df19,trace = TRUE,autotransform = F,maxit=1000,k=3,engine="monoMDS")









#kmeans

kk<-kmeans(site19[,-4],3)
kk$cluster
fullcluster19<-cbind(fullsite19,kk$cluster)
fullsite19$Transect_id





rotate08<-MDSrotate(spp.ord08, topocor08$FS_TRT)
rotate13<-MDSrotate(spp.ord13, topocor13$FS_TRT)
rotate98<-MDSrotate(spp.ord98, topocor98$FS_TRT)
rotate97<-MDSrotate(spp.ord97, topocor97$FS_TRT)
rotate19<-MDSrotate(spp.ord19, topocor19$FS_TRT)
rotate16<-MDSrotate(spp.ord16, topocor16$FS_TRT)

rotate97

#scores


sc97<-scores(rotate97)
sc98<-scores(rotate98)
sc08<-scores(rotate08)
sc13<-scores(rotate13)
sc16<-scores(rotate16)
sc19<-scores(rotate19)


site97<-as.data.frame(sc97[[1]])
spp97<-as.data.frame(sc97[[2]])

site98<-as.data.frame(sc98[[1]])
spp98<-as.data.frame(sc98[[2]])

site08<-as.data.frame(sc08[[1]])
spp08<-as.data.frame(sc08[[2]])

site13<-as.data.frame(sc13[[1]])
spp13<-as.data.frame(sc13[[2]])

site16<-as.data.frame(sc16[[1]])
spp16<-as.data.frame(sc16[[2]])

site19<-as.data.frame(sc19[[1]])
spp19<-as.data.frame(sc19[[2]])


topocor

names<-(rownames(site19))

spp97$spp<-rownames(spp97)
site97$Transect_id<-rownames(site97)
spp98$spp<-rownames(spp98)
site98$Transect_id<-rownames(site98)
spp08$spp<-rownames(spp08)
site08$Transect_id<-rownames(site08)
spp13$spp<-rownames(spp13)
site13$Transect_id<-rownames(site13)
spp16$spp<-rownames(spp16)
site16$Transect_id<-rownames(site16)
spp19$spp<-rownames(spp19)
site19$Transect_id<-(rownames(site19))

fullsite97<-merge(site97,topocor97)
fullsite98<-merge(site98,topocor98)
fullsite08<-merge(site08,topocor08)
fullsite13<-merge(site13,topocor13)
fullsite16<-merge(site16,topocor16)
fullsite19<-merge(site19,topocor19)


topocormat97<-topocor97[,-(1:2)]
topocormat98<-topocor98[,-(1:2)]
topocormat08<-topocor08[,-(1:2)]
topocormat13<-topocor13[,-(1:2)]
topocormat16<-topocor16[,-(1:2)]
topocormat19<-topocor19[,-(1:2)]


#correlation analysis



topofit97<-envfit(rotate97,topocormat97)
topofit98<-envfit(rotate98,topocormat98)
topofit08<-envfit(rotate08,topocormat08)
topofit13<-envfit(rotate13,topocormat13)
topofit16<-envfit(rotate16,topocormat16)
topofit19<-envfit(rotate19,topocormat19)

topofit19


#correlation vectors

topo_vec19 <-as.data.frame(topofit19$vectors$arrows)
topo_fac19 <-as.data.frame(topofit19$factors$centroids)
topo_vecp19<-as.data.frame(topofit19$vectors$pvals)
names(topo_vecp19)<-"p"
topo_vects19<-cbind(topo_vec19,topo_vecp19)
tov19<-subset(topo_vects19,p < 0.05)
topo_facp19<-as.data.frame(topofit19$factors$pvals)
names(topo_facp19)<-"p"
tof19<-subset(topo_facp19,p < 0.05)



topo_vec98 <-as.data.frame(topofit98$vectors$arrows)
topo_fac98 <-as.data.frame(topofit98$factors$centroids)
topo_vecp98<-as.data.frame(topofit98$vectors$pvals)
names(topo_vecp98)<-"p"
topo_vects98<-cbind(topo_vec98,topo_vecp98)
tov98<-subset(topo_vects98,p < 0.05)
topo_facp98<-as.data.frame(topofit98$factors$pvals)
names(topo_facp98)<-"p"
tof98<-subset(topo_facp98,p < 0.05)



topo_vec97 <-as.data.frame(topofit97$vectors$arrows)
topo_fac97 <-as.data.frame(topofit97$factors$centroids)
topo_vecp97<-as.data.frame(topofit97$vectors$pvals)
names(topo_vecp97)<-"p"
topo_vects97<-cbind(topo_vec97,topo_vecp97)
tov97<-subset(topo_vects97,p < 0.05)
topo_facp97<-as.data.frame(topofit97$factors$pvals)
names(topo_facp97)<-"p"
tof97<-subset(topo_facp97,p < 0.05)



topo_vec08 <-as.data.frame(topofit08$vectors$arrows)
topo_fac08 <-as.data.frame(topofit08$factors$centroids)
topo_vecp08<-as.data.frame(topofit08$vectors$pvals)
names(topo_vecp08)<-"p"
topo_vects08<-cbind(topo_vec08,topo_vecp08)
tov08<-subset(topo_vects08,p < 0.05)
topo_facp08<-as.data.frame(topofit08$factors$pvals)
names(topo_facp08)<-"p"
tof08<-subset(topo_facp08,p < 0.05)



topo_vec13 <-as.data.frame(topofit13$vectors$arrows)
topo_fac13 <-as.data.frame(topofit13$factors$centroids)
topo_vecp13<-as.data.frame(topofit13$vectors$pvals)
names(topo_vecp13)<-"p"
topo_vects13<-cbind(topo_vec13,topo_vecp13)
tov13<-subset(topo_vects13,p < 0.05)
topo_facp13<-as.data.frame(topofit13$factors$pvals)
names(topo_facp13)<-"p"
tof13<-subset(topo_facp13,p < 0.05)













#topofit$factors


#ordination arrows

arrow9719<-merge(site97,site19,by="Transect_id")
arrow9798<-merge(site97,site98,by="Transect_id")
arrow9808<-merge(site98,site08,by="Transect_id")
arrow9708<-merge(site97,site08,by="Transect_id")
arrow9713<-merge(site97,site13,by="Transect_id")
arrow9813<-merge(site98,site13,by="Transect_id")
arrow9819<-merge(site98,site19,by="Transect_id")
arrow0813<-merge(site08,site13,by="Transect_id")
arrow1319<-merge(site13,site19,by="Transect_id")


arrow9719<-merge(arrow9719,topocor_site)
arrow9798<-merge(arrow9798,topocor_site)
arrow9808<-merge(arrow9808,topocor_site)
arrow9708<-merge(arrow9708,topocor_site)
arrow9713<-merge(arrow9713,topocor_site)
arrow9813<-merge(arrow9813,topocor_site)
arrow9819<-merge(arrow9819,topocor_site)
arrow0813<-merge(arrow0813,topocor_site)
arrow1319<-merge(arrow1319,topocor_site)



arrow9719<-as.data.table(arrow9719)
arrow9798<-as.data.table(arrow9798)
arrow9808<-as.data.table(arrow9808)
arrow9708<-as.data.table(arrow9708)
arrow9713<-as.data.table(arrow9713)
arrow9813<-as.data.table(arrow9813)
arrow9819<-as.data.table(arrow9819)
arrow0813<-as.data.table(arrow0813)
arrow1319<-as.data.table(arrow1319)

#arrow9719<-arrow9719[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9798<-arrow9798[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9808<-arrow9808[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9708<-arrow9708[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9713<-arrow9713[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9813<-arrow9813[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow9819<-arrow9819[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow0813<-arrow0813[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]
#arrow1319<-arrow1319[,.(NMDS1.x=mean(NMDS1.x),NMDS1.y=mean(NMDS1.y),NMDS2.x=mean(NMDS2.x),NMDS2.y=mean(NMDS2.y)),by="FS_TRT"]


#for highlighting spp scores of non-natives and anti-non-natives (natives)

sppnames<-c("Schizachyrium scoparius","Lolium multiflorum","Bouteloua curtipendula","Bromus carinatus","Bromus inermis","Bromus tectorum","Elymus trachycaulum")

nons19<-spp19[spp19$spp %in% sppnames,]
antinons19<-spp19[!spp19$spp %in% sppnames,]

nons97<-spp97[spp97$spp %in% sppnames,]
antinons97<-spp97[!spp97$spp %in% sppnames,]

nons98<-spp98[spp98$spp %in% sppnames,]
antinons98<-spp98[!spp98$spp %in% sppnames,]

nons08<-spp08[spp08$spp %in% sppnames,]
antinons08<-spp08[!spp08$spp %in% sppnames,]

nons13<-spp13[spp13$spp %in% sppnames,]
antinons13<-spp13[!spp13$spp %in% sppnames,]

UE<-as.data.frame(unique_exotic)
UE$origin<-"exotic"
names(UE)<-c("y","exotic")
UE

exref<-merge(UE,refname,all=T)
exref[is.na(exref)]<-"native"
names(exref)<-c("spp","origin")

UE19<-merge(spp19,exref)

#ordination ellipses


veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}


oe19<-ordiellipse(rotate19,groups=fullsite19$FS_TRT)

df_ell19 <- data.frame()
for(g in levels(fullsite19$FS_TRT)){
  df_ell19 <- rbind(df_ell19, cbind(as.data.frame(with(fullsite19[fullsite19$FS_TRT==g,],
                                                   veganCovEllipse(oe19[[g]]$cov,oe19[[g]]$center,oe19[[g]]$scale)))
                                ,group=g))
}

fullsite19<-as.data.table(fullsite19)
NMDS.mean19<-fullsite19[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]

df_ell19




oe97<-ordiellipse(rotate97,groups=fullsite97$FS_TRT)

df_ell97 <- data.frame()
for(g in levels(fullsite97$FS_TRT)){
  df_ell97 <- rbind(df_ell97, cbind(as.data.frame(with(fullsite97[fullsite97$FS_TRT==g,],
                                                       veganCovEllipse(oe97[[g]]$cov,oe97[[g]]$center,oe97[[g]]$scale)))
                                    ,group=g))
}

fullsite97<-as.data.table(fullsite97)
NMDS.mean97<-fullsite97[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]
NMDS.mean97$year<-1997
df_ell97

oe98<-ordiellipse(rotate98,groups=fullsite98$FS_TRT)

df_ell98 <- data.frame()
for(g in levels(fullsite98$FS_TRT)){
  df_ell98 <- rbind(df_ell98, cbind(as.data.frame(with(fullsite98[fullsite98$FS_TRT==g,],
                                                       veganCovEllipse(oe98[[g]]$cov,oe98[[g]]$center,oe98[[g]]$scale)))
                                    ,group=g))
}

fullsite98<-as.data.table(fullsite98)
NMDS.mean98<-fullsite98[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]
NMDS.mean98$year<-1998
df_ell98



oe08<-ordiellipse(rotate08,groups=fullsite08$FS_TRT)

df_ell08 <- data.frame()
for(g in levels(fullsite08$FS_TRT)){
  df_ell08 <- rbind(df_ell08, cbind(as.data.frame(with(fullsite08[fullsite08$FS_TRT==g,],
                                                       veganCovEllipse(oe08[[g]]$cov,oe08[[g]]$center,oe08[[g]]$scale)))
                                    ,group=g))
}

fullsite08<-as.data.table(fullsite08)
NMDS.mean08<-fullsite08[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]
NMDS.mean08$year<-2008
df_ell08


oe13<-ordiellipse(rotate13,groups=fullsite13$FS_TRT)

df_ell13 <- data.frame()
for(g in levels(fullsite13$FS_TRT)){
  df_ell13 <- rbind(df_ell13, cbind(as.data.frame(with(fullsite13[fullsite13$FS_TRT==g,],
                                                       veganCovEllipse(oe13[[g]]$cov,oe13[[g]]$center,oe13[[g]]$scale)))
                                    ,group=g))
}

fullsite13<-as.data.table(fullsite13)
NMDS.mean13<-fullsite13[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]
NMDS.mean13$year<-2013
df_ell13





oe19<-ordiellipse(rotate19,groups=fullsite19$FS_TRT)

df_ell19 <- data.frame()
for(g in levels(fullsite19$FS_TRT)){
  df_ell19 <- rbind(df_ell19, cbind(as.data.frame(with(fullsite19[fullsite19$FS_TRT==g,],
                                                     veganCovEllipse(oe19[[g]]$cov,oe19[[g]]$center,oe19[[g]]$scale)))
                                  ,group=g))
}

fullsite19<-as.data.table(fullsite19)
NMDS.mean19<-fullsite19[,.(MDS1=mean(NMDS1),MDS2=mean(NMDS2)),by="FS_TRT"]
NMDS.mean19$year<-2019
df_ell19

pal2
rev(pal2)

ordplot<-ggplot()+
  #geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=TRT,pch=TRT),alpha=0.5)+
  #geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=TRT,pch=TRT),alpha=1)+

  geom_path(data=df_ell19, aes(x=NMDS1, y=NMDS2,colour=group),alpha=0.5, size=1, linetype=2)+
  geom_polygon(data=df_ell19, aes(x=NMDS1, y=NMDS2,fill=group), size=1, linetype=2,alpha=0.1)+
  #annotate("text",x=NMDS.mean19$MDS1,y=NMDS.mean19$MDS2,label=NMDS.mean19$year,alpha=0.5)+
  
  geom_path(data=df_ell13, aes(x=NMDS1, y=NMDS2,colour=group),alpha=0.4, size=1, linetype=2)+
  geom_polygon(data=df_ell13, aes(x=NMDS1, y=NMDS2,fill=group), size=1, linetype=2,alpha=0.1)+
  #annotate("text",x=NMDS.mean13$MDS1,y=NMDS.mean13$MDS2,label=NMDS.mean13$year,alpha=0.5)+
  
  geom_path(data=df_ell97, aes(x=NMDS1, y=NMDS2,colour=group),alpha=0.1, size=1, linetype=2)+
  geom_polygon(data=df_ell97, aes(x=NMDS1, y=NMDS2,fill=group), size=1, linetype=2,alpha=0.1)+
  #annotate("text",x=NMDS.mean97$MDS1,y=NMDS.mean97$MDS2,label=NMDS.mean97$year,alpha=0.3)+
  
  
  geom_path(data=df_ell98, aes(x=NMDS1, y=NMDS2,colour=group),alpha=0.2, size=1, linetype=2)+
  geom_polygon(data=df_ell98, aes(x=NMDS1, y=NMDS2,fill=group), size=1, linetype=2,alpha=0.1)+
  #annotate("text",x=NMDS.mean98$MDS1,y=NMDS.mean98$MDS2,label=NMDS.mean98$year,alpha=0.5)+
  
  
  geom_path(data=df_ell08, aes(x=NMDS1, y=NMDS2,colour=group),alpha=0.3, size=1, linetype=2)+
  geom_polygon(data=df_ell08, aes(x=NMDS1, y=NMDS2,fill=group), size=1, linetype=2,alpha=0.1)+
  #annotate("text",x=NMDS.mean08$MDS1,y=NMDS.mean08$MDS2,label=NMDS.mean08$year,alpha=0.5)+
  #geom_segment(antinons19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  #geom_text(antinons19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  
  #geom_segment(tov19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  #geom_text(tov19,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(tov19)))+
  
  #geom_text(spp19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  scale_color_manual(values=(pal3),"Treatment")+
  scale_fill_manual(values=(pal3),"Treatment")+
  #geom_segment(tov19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  
  #geom_text(tov19,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(tov19)))+
  theme_bw()+
  theme(legend.position="none")


#geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
#geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+

ordplot

ordplot_arrows<-ggplot()+
  geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=1,size=3,alpha=0.25)+
  geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=16,size=3,alpha=0.25)+
  geom_segment(arrow9719, mapping=aes(x = NMDS1.x, xend = NMDS1.y, y = NMDS2.x, yend = NMDS2.y,col=FS_TRT),alpha=0.75,
               arrow = arrow(length = unit(0.5, "cm")),size=1.1)+
  scale_color_manual(values=(pal3))+
  #facet_wrap(~FS_TRT)+
  #geom_text(spp19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  #geom_segment(arrow9719, mapping=aes(x = NMDS1.x, xend = NMDS1.y, y = NMDS2.x, yend = NMDS2.y,col=TRT),
  #             arrow = arrow(length = unit(0.5, "cm")),alpha=0.5,size=2)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+

  theme_bw()



ordplot|ordplot_arrows

pal3<-pnw_palette("Sunset",n=3)

?scale_color_manual


ordplot_correlation97<-ggplot()+
  geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=topocor97$TRT_response),pch=1,size=3)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  geom_segment(tov97, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(tov97,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(tov97)))+
  geom_segment(topo_fac97, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(topo_fac97,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(topo_fac97)))+
  #geom_segment(nons97, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  #geom_text(antinons97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  
  
  theme_bw()

ordplot_correlation19<-ggplot()+
  #geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=1,size=3,alpha=0.5)+
  geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=16,alpha=0.5)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  geom_segment(tov19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(tov19,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(tov19)),size=5)+
  xlim(c(-1.5,1.5))+
  #geom_segment(topo_fac19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  #geom_text(topo_fac19,mapping=aes(x=NMDS1,y=NMDS2,label=rownames(topo_fac19)))+
  #geom_segment(nons19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  #geom_text(antinons19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  scale_color_manual(values=(pal3))+
  theme_bw()+
  theme(legend.position = "none")
  



UEsub<-subset(UE19,abs(NMDS1)>0.5|abs(NMDS2)>0.5)

UEsub

ordplot_correlation19


check<-full_d[,.(med=median(exotic_coverratio)),by=c("year","TRT")]

ordplot_spp19<-ggplot()+
  geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=TRT),pch=16,size=3,alpha=0.5)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  geom_segment(nons19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(nons19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  xlim(c(-1.75,1.5))+
  scale_color_manual(values=(pal2))+
#  geom_text(antinons19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  theme_bw()

ordplot_spp19_native<-ggplot()+
  geom_point(fullsite19,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=16,alpha=0.5)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  xlim(c(-2,1.75))+
  scale_color_manual(values=(pal3))+
  new_scale_color()+
  geom_segment(UE19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2,color=origin),
               arrow = arrow(length = unit(0.25, "cm")),alpha=0.5)+
  geom_text(UEsub,mapping=aes(x=NMDS1,y=NMDS2,label=spp,color=origin),size=5,alpha=0.75)+
  #  geom_text(antinons19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  scale_color_manual(values=(pal3))+
  theme_bw()+
  theme(legend.position="none")

ordplot_spp13<-ggplot()+
  geom_point(fullsite13,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=1,size=3)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_segment(nons19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(nons13,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(nons13,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(nons13,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(antinons13,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  theme_bw()

ordplot_spp97<-ggplot()+
  geom_point(fullsite97,mapping=aes(x=NMDS1,y=NMDS2,col=FS_TRT),pch=1,size=3)+
  #geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
  #geom_segment(nons19, mapping=aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5)+
  geom_text(nons97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(nons97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(nons97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  geom_text(antinons97,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  theme_bw()


ordplot_spp97
ordplot_spp13
ordplot_spp19_native

png("./Figures/EDA/ordinations_fstrt.png",height = 25, width = 30, units = "cm", res=300, bg='white')
ordplot/ordplot_spp19_native|ordplot_arrows/ordplot_correlation19
dev.off()


png("./Figures/EDA/ordinations_reduce.png",height = 12.5, width = 40, units = "cm", res=300, bg='white')
ordplot|ordplot_spp19_native|ordplot_arrows
dev.off()


png("./Figures/EDA/ordinations_native_trt.png",height = 20, width = 20, units = "cm", res=300, bg='white')
ordplot_spp19_native
dev.off()


png("./Figures/EDA/ordinations_fstrt.png",height = 12.5, width = 30, units = "cm", res=300, bg='white')
ordplot|ordplot_arrows

dev.off()


cor.test(full_d$PctC_lolium,full_d$PctC_exotic)

ordplot_poly

ordplot<-ggplot()+
  geom_point(fullsite97,mapping=aes(x=NMDS2,y=NMDS3,col=TRT),pch=16,size=3)+
  geom_point(fullsite19,mapping=aes(x=NMDS2,y=NMDS3,col=TRT),pch=1,size=3)+
  #geom_text(spp19,mapping=aes(x=NMDS1,y=NMDS2,label=spp))+
  theme_bw()
#geom_polygon(hull.data97,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
#geom_polygon(hull.data19,mapping=aes(x=NMDS1,y=NMDS2,fill=TRT,grp=TRT),alpha=0.15)+
#geom_segment(arrowtest, mapping=aes(x = NMDS1.x, xend = NMDS1.y, y = NMDS2.x, yend = NMDS2.y),
#             arrow = arrow(length = unit(0.25, "cm")), colour = "black",alpha=0.5) 



sc19
ordplot
topofit$vectors$pvals

ggscore19<-cbind(sc19[[1]],sc19[[2]])


spp.ord19$species

ordiplot(spp.ord97)

shush<-rbind(sppsum,sppred)


shush[1,]
shhh<-shush[1,]>20



shush<-colSums(spp[,-2])



?dcast