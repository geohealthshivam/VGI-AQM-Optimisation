library(raster)
DEM_stut1000 = raster("dgm1000.utm32s.gridascii/dgm1000/dgm1000_utm32s.asc")
plot(DEM_stut1000)
DEM_stut200 = raster("dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc")
plot(DEM_stut200)

?mask


DEM_stut1000<-crop(DEM_stut1000,stutgart_shp)
DEM_stut200<-crop(DEM_stut200,stutgart_shp)

crs(DEM_stut1000)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
crs(DEM_stut200)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
DEM_stut1000<-mask(DEM_stut1000,stutgart_shp)
DEM_stut200<-mask(DEM_stut200,stutgart_shp)
plot(DEM_stut1000)
plot(DEM_stut200)


#original data set of monitoring station 
original_sttut_dataset
original_stut_sp<-original_sttut_dataset
coordinates(original_stut_sp)<-cbind(original_stut_sp$coords_x1,original_stut_sp$coords_x2)
crs(original_stut_sp)<-"+proj=longlat +datum=WGS84 +no_defs "
original_stut_sp<-spTransform(original_stut_sp,crs(stutgart_shp))

#Data set of monitoring station after first deletion(n=73)
sttut_dataset_del1
del1_stut_sp<-sttut_dataset_del1

coordinates(del1_stut_sp)<-cbind(del1_stut_sp$coords_x1,del1_stut_sp$coords_x2)
crs(del1_stut_sp)<-"+proj=longlat +datum=WGS84 +no_defs "
del1_stut_sp<-spTransform(del1_stut_sp,crs(stutgart_shp))
#original config
#for 200m resolutiond DEM
plot(DEM_stut200)
plot(stutgart_shp,add=T)
plot(original_stut_sp,add=T)
#for 1000m resolutiond DEM
plot(DEM_stut1000)
plot(stutgart_shp,add=T)
plot(original_stut_sp,add=T)

#deleated configuration 
#for 200m resolutiond DEM
plot(DEM_stut200)
plot(stutgart_shp,add=T)
plot(del1_stut_sp,add=T)
#for 1000m resolutiond DEM
plot(DEM_stut1000)
plot(stutgart_shp,add=T)
plot(del1_stut_sp,add=T)

#squareroot of altitude for original configuration 
original_stut_sp$SQRALT<-extract(DEM_stut200,original_stut_sp)
original_stut_sp$SQRALT<-sqrt(original_stut_sp$SQRALT)

#squareroot go altitude for deletaed configuration (n=73)
del1_stut_sp$SQRALT<-extract(DEM_stut200,del1_stut_sp)
del1_stut_sp$SQRALT<-sqrt(del1_stut_sp$SQRALT)

#putting values back to data frame for regression analysis 
original_sttut_dataset$SQRALT<-original_stut_sp$SQRALT
write.csv(original_sttut_dataset,"Original stutgart data configuration DEM included")
sttut_dataset_del1$SQRALT<-del1_stut_sp$SQRALT

write.csv(sttut_dataset_del1,"Post deletion stutgart data configuration DEM included")



#SQRALT for candi location 
names(covars_all)
covars_all.sp<-covars_all
coordinates(covars_all.sp)<-cbind(covars_all.sp$x,covars_all.sp$y)
crs(covars_all.sp)<-crs(stutgart_shp)
plot(covars_all.sp,add=T)
plot(stutgart_shp)
covars_all.sp$SQRALT<-extract(DEM_stut200,covars_all.sp)
covars_all.sp$SQRALT<-sqrt(covars_all.sp$SQRALT)
covars_all$SQRALT<-covars_all.sp$SQRALT
write.csv(covars_all,"500m resolution points for prediction")

#population data extraction
library(raster)
stut_pop = raster("Population data/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev10_2015_30_sec_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev10_2015_30_sec.tif")

plot(stut_pop)
summary(stut_pop)
barplot(stut_pop)
writeRaster(stut_pop,"Sttutgart_pop_1km","GTiff")
?writeRaster
stut_pop<-crop(stut_pop,stutgart_shp.trans)
DEM_stut200<-crop(DEM_stut200,stutgart_shp)

crs(DEM_stut1000)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
crs(DEM_stut200)<-"+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
DEM_stut1000<-mask(DEM_stut1000,stutgart_shp)
DEM_stut200<-mask(DEM_stut200,stutgart_shp)
plot(DEM_stut1000)
plot(DEM_stut200)


#original data set of monitoring station 
original_sttut_dataset
original_stut_sp<-original_sttut_dataset
coordinates(original_stut_sp)<-cbind(original_stut_sp$coords_x1,original_stut_sp$coords_x2)
crs(original_stut_sp)<-"+proj=longlat +datum=WGS84 +no_defs "
original_stut_sp<-spTransform(original_stut_sp,crs(stutgart_shp))

#Data set of monitoring station after first deletion(n=73)
sttut_dataset_del1
del1_stut_sp<-sttut_dataset_del1

coordinates(del1_stut_sp)<-cbind(del1_stut_sp$coords_x1,del1_stut_sp$coords_x2)
crs(del1_stut_sp)<-"+proj=longlat +datum=WGS84 +no_defs "
del1_stut_sp<-spTransform(del1_stut_sp,crs(stutgart_shp))
#original config
#for 200m resolutiond DEM
plot(DEM_stut200)
plot(stutgart_shp,add=T)
plot(original_stut_sp,add=T)
#for 1000m resolutiond DEM
plot(DEM_stut1000)
plot(stutgart_shp,add=T)
plot(original_stut_sp,add=T)

#deleated configuration 
#for 200m resolutiond DEM
plot(DEM_stut200)
plot(stutgart_shp,add=T)
plot(del1_stut_sp,add=T)
#for 1000m resolutiond DEM
plot(DEM_stut1000)
plot(stutgart_shp,add=T)
plot(del1_stut_sp,add=T)

#squareroot of altitude for original configuration 
original_stut_sp$SQRALT<-extract(DEM_stut200,original_stut_sp)
original_stut_sp$SQRALT<-sqrt(original_stut_sp$SQRALT)

#squareroot go altitude for deletaed configuration (n=73)
del1_stut_sp$SQRALT<-extract(DEM_stut200,del1_stut_sp)
del1_stut_sp$SQRALT<-sqrt(del1_stut_sp$SQRALT)

#putting values back to data frame for regression analysis 
original_sttut_dataset$SQRALT<-original_stut_sp$SQRALT
write.csv(original_sttut_dataset,"Original stutgart data configuration DEM included")
sttut_dataset_del1$SQRALT<-del1_stut_sp$SQRALT

write.csv(sttut_dataset_del1,"Post deletion stutgart data configuration DEM included")


