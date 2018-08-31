library(raster)
pop_census2011 = raster("~/sciebo/Sttutgart optimisation paper/Sttutgart data/Population/Bevoelkerungszahl.tif")
plot(pop_census2011)
crs(pop_census2011)<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs  "

pop_census2011.proj<-projectRaster(pop_census2011,crs=crs(stutgart_shp))

plot(pop_census2011.proj)

plot(stutgart_shp,add=T)
plot(stut_sample,add=T)

plot(stut_sample_500m,add=T)

pop_census2011.proj<-crop(pop_census2011.proj,stutgart_shp)


plot(pop_census2011.proj)

candi_500m$population<-extract(pop_census2011.proj,candi_500m)
candi_1km$population<-extract(pop_census2011.proj,candi_1km)
head(candi_1km)
SpatialTools::dist1(coordinates(complete_1stweek_pm2.5[1:10,]))

optimUSER
spsann:::.prepare_points

