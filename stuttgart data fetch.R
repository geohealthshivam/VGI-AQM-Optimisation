baseURL<-'http://archive.luftdaten.info'
date<-'2017-10-21'
sensorname<-"sds011"
type<-"sensor"
id<-777
library(RCurl)
sensorfilename<-paste(date,sensorname,type,id,sep = "_")
filename<-paste(sensorfilename,".csv",sep="")
filename
url2fetch <- paste(baseURL,date,filename,sep="/")
url2fetch
url_content <- getURL(url2fetch)
mydat <- read.csv2(textConnection(url_content), header=T)
head(mydat)
length(sensorids)

library(httr)
url2fetch
r <- GET("http://archive.luftdaten.info/2017-10-21/2017-10-21_sds011_sensor_111.csv")
 
getsttutgartdata<-function(date,sensorname,baseURL,type,sensorid){
  baseURL<-'http://archive.luftdaten.info'
  l<-length(sensorid)
  meandata<-data.frame(sensor_id=numeric(l),location=numeric(l),lat=double(l),lon=double(l),P1=double(l),P2=double(l))
  for(i in 1:length(sensorid)){id<-sensorid[i]
  sensorfilename<-paste(date,sensorname,type,id,sep = "_")
  filename<-paste(sensorfilename,".csv",sep="")
  url2fetch <- paste(baseURL,date,filename,sep="/")
  r <- httr::GET(url2fetch)
  stdata<-data.frame()
  if(r$status_code==200){
    url_content <- getURL(url2fetch)
    stdata<-read.csv2(textConnection(url_content), header=T,stringsAsFactors = F)
    meandata$sensor_id[i]<-stdata$sensor_id[1]
    meandata$location[i]<-stdata$location[1]
    meandata$lat[i]<-as.numeric(stdata$lat[1])
    meandata$lon[i]<-as.numeric(stdata$lon[1])
    meandata$P1[i]<-mean(as.numeric(stdata$P1),na.rm=T)
    meandata$P2[i]<-mean(as.numeric(stdata$P2),na.rm=T)
  } else {
    i<-i+1
  }
  }
  return(meandata)}

setwd("~/Dropbox/Sttutgart data")
require(RCurl)
stutgartdata_20_10<-stutgartdata
stutgartdata_21_10<-getsttutgartdata(date='2017-10-21',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensorids)



stutgartdata_21_10.copy<-stutgartdata_21_10[!!rowSums(abs(stutgartdata_21_10[-c(1:2)])),]
coordinates(stutgartdata_21_10.copy)<-c("lon","lat")
proj4string(stutgartdata_21_10.copy)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
mapview(stutgartdata_21_10.copy)
#mapview
shpplot<-mapview(stutgart_shp.trans)
poin<-mapview(as(pointinsttut, 'Spatial'),zcol="P1",cex = "P1") 
shpplot+poin
#sf for intersection
require(sf)
pointinsttut<-st_intersection(st_as_sf(stutgartdata_21_10.copy),st_as_sf(stutgart_shp.trans))
#convert to sp object
pointinsttutgart_sp<-as(pointinsttut, 'Spatial')

#Variogram
dat1=data.frame(coordinates(pointinsttutgart_sp),P1=pointinsttutgart_sp$P1,P2=pointinsttutgart_sp$P2)
dat2=dat1
geo1=as.geodata(dat1,coords.col=1:2,data.col=3)
plot(geo1)
#variogram cloud
var1=variog(geo1,estimator.type = 'modulus', option = "cloud")
#Variogram
var1=variog(geo1,estimator.type = 'modulus')
plot(var1)
#other code 
stutgart20oct<-stutgartdata_20_10.copy
rm(stutgart20oct)
coordinates(stutgartdata_20_10.copy)<-c("lon","lat")
mapview(stutgart20oct)
(stutgart20oct)

plot(stutgart_shp.trans)

stutgart_shp.trans.cov<-spTransform(stutgart_shp.trans,"+proj=longlat +ellps=GRS80 +no_defs ")
plot(var1)

CORINE<-raster("/Users/shivamgupta/Downloads/g100_clc12_V18_5a/g100_clc12_V18_5.tif")
proj4string(CORINE)
proj4string(CORINE)<-CRS("+init=epsg:3035")
shape_sttut<-spTransform(stutgart_shp.trans,proj4string(CORINE))
proj4string(shape_sttut)
proj4string(CORINE)
plot(CORINE,extent=T)

landuse_corine_stut<-crop(CORINE,shape_sttut)
plot(landuse_corine_stut)
plot(shape_sttut,add=T)
proj4string(landuse_corine_stut)

require(raster)
sttut_Raster2polygon<-rasterToPolygons(landuse_corine_stut,dissolve = TRUE)
plot(sttut_Raster2polygon)
library(rgeos)
Sttutgart_landuse<-intersect(sttut_Raster2polygon,shape_sttut)
plot(Sttutgart_landuse)
writeRaster(landuse_corine_stut,filename = 'Sttutgart_LU_corine_unprojected.tif')
library(readxl)
clc_legend <- read_excel("~/Downloads/g100_clc12_V18_5a/Legend/clc_legend.xls")









require(raster)
require(rgeos)
require(gstat)
require(rgdal)
require(geoR)
require(maptools)
setwd("~/Downloads/Data for airpollution with time")
dir()
stutgart_shp<-readShapeSpatial("Sttutgartshapefile.shp" )
major.roads<-readShapeLines("~/Documents/Data for analysis Muenster/Shape files 32632/SHAPE FILE/Major roads.shp")
minor.roads<-readShapeSpatial("~/Documents/Data for analysis Muenster/Shape files 32632/SHAPE FILE/Minor Roads.shp")
#extracting points in stutggart
st_intersection(st_as_sf(stutgart20oct),st_as_sf(stutgart_shp.trans))
plot(stutgart_shp.trans,axis=T)
plot(pointsinstut["P1"],add=T,axis=T)
#extracting roads in stuttgart
stutgart_roads<-read_sf("stuttgart-regbez-latest-free.shp/gis.osm_roads_free_1.shp")

roadsinstut<-st_intersection((stutgart20oct),st_as_sf(stutgart_shp.trans))

rm(stut.offical.data)

stut.offical.data<-read.csv2("data_2017_11_16.csv",sep =";",header = T,stringsAsFactors = F)
meta.offial.station<- read.csv2("Bericht_EU_Meta_Stationen.csv" ,sep=";",stringsAsFactors = F)
stut.offical.data

for (i in 1:nrow(stut.offical.data)){
  for (j in 1:nrow(meta.offial.station)){
  if (stut.offical.data$Stationscode[i]==meta.offial.station$station_code[j]){
    stut.offical.data$lon[i]<-as.numeric(meta.offial.station$station_longitude_d[j])
    stut.offical.data$lat[i]<-as.numeric(meta.offial.station$station_latitude_d[j])
  }
}}


meta.offial.station[meta.offial.station$station_code=="DEBW005",]
stut.offical.data[2,]
stut.offical.data$lon<-as.numeric(stut.offical.data$lon)
stut.offical.data$lat<-as.numeric(stut.offical.data$lat)
class(stut.offical.data$lon)
coordinates(stut.offical.data)<-c("lon","lat")
projection(stut.offical.data)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
stut.offical.data<-spTransform(stut.offical.data,projection(stutgart_shp.trans))
stutgart_shp.trans



library(ggmap)
m <- get_map("stuttgart",zoom=12,maptype="terrain",source="google")
ggmap(m)


p<-mapview(stut.offical.data)
m<-mapview(stutgart_shp.trans)
s<-mapview(stutgart20oct)
p+m+s

sensorids<-c(92,125,140,143,146,151,164,171,177,179,181,187,189,191,209,211,215,217,21,227,237,286,287,295,309,359,415,426,430,436,466,549,553,568,576,585,609,620,645,649,671,673,675,677,701,723,727,751,755,757,763,767,773,783,789,809,824,842,956,962,968,986,114,118,120,122,128,132,136,143,147,148,150,159,162,165,170,171,172,173,193,199,200,202,215,219,228,229,2478,2492,2544,2586,2608,2694,2700,2820,2870,2996,3010,3034,3066,3117,3229,3441,3465,3559,3885,3975,4208,4313,4383,4458,4636,4640,4740,4837,4935,5127,5341,5355,5477,5756,5758,5935,5957,5991,6059,6107,6400,6408,6414,6455,6479,6487,6509,6549,6553,6763,6797,6809,6854,7003,7007,7037,7041,173,175,193,223,231,271,331,428,438,444,658,663,669,679,737,801,112,130,131,132,133,135,153,154,154,161,164,168,175,209,244,279,3209,3419,3781,4460,4508,4786,5694,6111,6576,6655,95,15,22,24,27,27,28,29,33,34,361,363,422,450,460,467,495,529,535,641,665,703,721,769,793,924,942,950,966,1026,1090,1092,1132,1216,1306,1334,1378,1386,1487,1589,1635,1645,2480,2488,2614,2652,2664,2730,2746,2784,3143,3145,3199,3651,3985,4827,5135,5307,5399,5929,5981,5993,6115,6551,6582,6588,6890,493,547,705,715,944,978,994,114,161,176,178,196,214,245,259,262,271,2716,2726,3153,3181,3201,3591,3681,3775,3914,3918,3955,3957,3959,3963,3969,4001,4011,4013,4031,4057,4250,4303,4486,4555,4597,4628,4823,5205,5495,5785,5933,6290,6354,6541,6661,6791,6821,434,775,1354,1424,2221,2251,2430,2482,3085,3231,3237,3307,3769,6338,6517,6896)

#146:
92,125,140,143,146,151,164,171,177,179,181,187,189,191,209,211,215,217,21,227,237,286,287,295,309,359,415,426,430,436,466,549,553,568,576,585,609,620,645,649,671,673,675,677,701,723,727,751,755,757,763,767,773,783,789,809,824,842,956,962,968,986,114,118,120,122,128,132,136,142,143,147,148,150,159,162,165,170,171,172,173,193,199,200,202,215,219,228,229,2478,2492,2544,2586,2608,2694,2700,2820,2870,2996,3010,3034,3066,3117,3229,3441,3465,3559,3885,3975,4208,4313,4383,4458,4636,4640,4740,4837,4935,5127,5341,5355,5477,5756,5758,5935,5957,5991,6059,6107,6400,6408,6414,6455,6479,6487,6509,6549,6553,6763,6797,6809,6854,7003,7007,7037,7041

#42:
173,175,193,223,231,271,331,428,438,444,658,663,669,679,737,801,112,130,131,132,133,135,153,154,154,161,164,168,175,209,244,279,3209,3419,3781,4460,4508,4786,5694,6111,6576,6655

#67:
95,15,22,24,27,27,28,29,33,34,361,363,422,450,460,467,495,529,535,641,665,703,721,769,793,924,942,950,966,1026,1090,1092,1132,1216,1306,1334,1378,1386,1487,1589,1635,1645,2480,2488,2614,2652,2664,2730,2746,2784,3143,3145,3199,3651,3985,4827,5135,5307,5399,5929,5981,5993,6115,6551,6582,6588,6890

sensorids.try<-for(i in 1:5){print(sensorids[i])}

#54:
493,547,705,715,944,978,994,114,161,176,178,196,214,245,259,262,271,2716,2726,3153,3181,3201,3591,3681,3775,3914,3918,3955,3957,3959,3963,3969,4001,4011,4013,4031,4057,4250,4303,4486,4555,4597,4628,4823,5205,5495,5785,5933,6290,6354,6541,6661,6791,6821

#16
434,775,1354,1424,2221,2251,2430,2482,3085,3231,3237,3307,3769,6338,6517,6896


for(j in 1:length(sensorids.try)){
  baseURL<-'http://archive.luftdaten.info'
  id<-sensorids.try[j]  
  sensorfilename<-paste(date,sensorname,type,id,sep = "_")
  filename<-paste(sensorfilename,".csv",sep="")
  url2fetch <- paste(baseURL,date,filename,sep="/")
  r <- httr::GET(url2fetch)
  ifelse(r$status_code==200,print(id),j<-j+1)
} 