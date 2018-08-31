require(RCurl)



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
    stdata<-read.csv2(connect<-textConnection(url_content), header=T,stringsAsFactors = F)
    close(connect)
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

#371 sensors 
sensor_371<-c(92, 122 , 140 ,143 , 146 ,  151,164 , 171 , 175 , 177 , 179 , 181 , 185 , 187 , 189 , 191 , 209 , 211 , 215 , 217 , 219 , 227 , 231 , 271 , 286 , 287 , 295,299, 309, 359, 415, 422, 424, 426, 428, 430, 432, 434, 438, 444, 450, 466, 535, 547, 549, 553,   568,   576,   585,   609,   620,   641,   645,   647,   649,   663,   665,   673,   675,   677,   679,   689,   715,   727,   737,   751,   755,   757,   763,   767,   773,   775,   783,   789,   793,   795,   801,   809,   824,   842,   924,   946,   950,   956,   962,   968,   978,   986,   1026,   1092,   1122,   1148,   1186,   1204,   1216,   1228,   1282,   1300,   1308,   1312,   1324,   1334,   1336,   1352,   1354,   1356,   1364,   1400,   1422,   1424,   1434,   1477,   1483,   1485,   1487,   1533,   1553,   1583,   1599,   1627,   1635,   1657,   1703,   1715,   1727,   1739,   1757,   1761,   1949,   2003,   2007,   2023,   2099,   2147,   2153,   2157,   2199,   2221,   2251,   2281,   2299,   2318,   2446,   2454,   2478,   2480,   2488,   2544,   2590,   2598,  2614,   2694,   2700,   2820,   2870,   2912,   2996,   3010,   3034,   3066,   3085,   3117,   3143,   3145,   3199,   3229,   3231,   3237,   3307,   3419,   3465,   3559,   3591,   3696,   3769,   3853,   3885,   4208,   4250,   4313,   4383,   4458,   4460,   4486,   4508,   4640,   4718,   4827,   4837,   4935,   5127,   5167,   5339,   5341,   5343,   5355,   5477,   5750,   5756,   5929,   5933,   5957,   5991,   6059,   6338,   6400,   6414,   6455,   6479,   6487,   6509,   6517,   6549,   6553,   6576,   6582,   6655,   6763,   6791,   6809,   6854,   6896,   7003,   7007,   7037,   7076,   7078,   7092,   7100,   7142,   7150,   7189,   7193,   7295,   7352,   7505,   7561,   7573,   7587,   7601,   7609,   7619,   7651,   7691,   7735,   7759,   7853,   7887,   7933,   7939,   8104,   8167,   8175,   8177,   8183,   8225,   8236,   8258,   8275,   8289,   8317,   8349,   8431,   8435,   8458,   8530,   8594,   8696,   8739,   8867,   8881,   8913,   8938,   8946,   8954,   8970,   9005,   9124,   9166,   9208,   9212,   9218,   9258,   9302,   9322,   9358,   9380,   9430,   9475,   9485,   9524,   9649,   9759 ,  9793 ,  9840 ,  9842 ,  9868 ,  9900 ,  9914 ,  9936 ,  9962 ,  9968 ,  10116,  10134,  10311,  10321,  10418,  10471,  10477,  10507,  10529,  10546,  10548,  10567,  10573,  10675,  10677,  10697,  10847,  10867,  10955,  10963,  11137,  11197,  11263,  11269,   11272,   11285,   11309,   11313,   11435,   11461,   11510,   11598,   11618,   11626,   11651,   11653,   11819,   11895,   11933,   12022,   12040,   12086,   12098,   12118,   12173,   12277,   12294,   12298,   12441,   12531,   12581,   12768,   12826,   13083,  13173,  13189,   13463,   13552,   13670,   13747,   13764,   13766,   13837,   14015,   14205,   14310,   14338,   14356,  14580)

Sensor_119<-c(471, 473, 477, 479, 493, 496, 505, 507, 515, 519, 537, 705, 944 , 994 , 1126, 1140, 1737, 1783, 1927, 2273, 2622, 2726, 2838, 3153, 3181, 3201, 3405, 3681, 3775, 3914, 3918, 3941, 3953, 3955, 3959, 3961, 3963, 3969, 4001, 4011, 4013, 4031, 4057, 4303, 4555, 4597, 4628, 4998, 5205, 5481, 5495, 5785, 5873, 6125, 6241, 6290, 6354, 6545, 6661, 6745, 6747, 6779, 6813, 6819, 6821, 6828, 6920, 6939, 7173, 7346, 7410, 7422, 7448, 7689, 7707, 7731, 7815, 7823, 7851, 7867, 8209, 8327, 8371, 8413, 8419 , 8528 , 8620 , 8716 , 9082 , 9084 , 9154 , 9200 , 9465 , 10291, 10339, 10381, 10432, 10458, 10523, 10577, 10831, 10908, 10985, 11000, 11088, 11205, 11789, 12149, 12292, 12571, 12633, 12830, 13234, 13329, 13386, 13469, 13487, 13822, 14619 )

Sensors_104<-c(197, 201, 203, 205, 243, 305, 335, 341, 361, 363, 417, 454, 458, 460, 467, 485, 491, 495, 529, 554, 577, 651, 659, 669, 703, 735, 769, 805, 813, 966, 1076, 1090, 1296, 1306, 1344, 1509, 1585, 1589, 2201, 2269, 2320, 2492, 2644, 2652, 2664, 2702, 2730, 2746, 2786, 3137, 3255, 3567, 3651, 3847, 3985, 4244, 4601, 5054, 5307, 5646, 5981, 5993, 6412, 6485, 6551, 6759, 6890, 7144, 7148, 7285, 7306, 7328, 7761, 7843, 7895, 8241 , 8305 , 8337 , 8580 , 8598 , 8708 , 8893 , 9138 , 9651 , 9819 , 10299, 10503, 10569, 11145, 11465, 11498, 11741, 11935, 11996, 12169, 12177, 12478, 12844, 12892, 13311, 13760, 13770, 13776, 14038)

sensorids<-c(sensor_371,Sensor_119,Sensors_104)

sensor_in_stutgart<-c(125, 140, 146, 151, 164, 187, 191, 209, 215, 217, 227, 237, 286, 287, 295, 309, 415, 426, 430, 466, 549, 553, 576, 585, 609, 620, 645, 649, 671, 673, 675, 677, 723, 727, 751, 757, 763, 767, 773, 783, 789, 809, 956, 962, 968, 986, 143, 219, 2478, 2492, 2544, 2586, 2608, 2700, 2820, 2870, 2996, 3034, 3066, 3117, 3229, 3441, 3559, 3885, 3975, 4208, 4313, 4383, 4458, 4636, 4640, 4740, 4837, 4935, 5127, 5355, 5477, 5756, 5758, 5935, 5957, 6059, 6107, 6400, 6408, 6414, 6455, 6479, 6487, 175, 231, 438, 444, 658, 663, 737, 801, 4460, 4508, 422, 450, 529, 535, 641, 665, 721, 793, 924, 950, 1026, 1092, 1216, 1334, 1487, 1589, 1645, 2480, 2488, 2614, 3143, 3145, 3199, 4827, 5135, 5399, 5929, 547, 715, 978, 271, 3591, 4250, 4486)





#for the first day of the year 2018 
stutgartdata_jan2018_1st<-getsttutgartdata(date='2018-01-01',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensorids)

summary(stutgartdata_jan2018_1st)
stutgartdata_jan2018_1st[stutgartdata_jan2018_1st == 0] <- NA
summary(stutgartdata_jan2018_1st)

###deleting all NA columns (206)
stutgartdata_jan2018_1st<-na.omit(stutgartdata_jan2018_1st)

nrow(stutgartdata_jan2018_1st)  #388 rowns now 


##mapview
shpplot<-mapview(stutgart_shp.trans)
poin<-mapview(pointinsttutgart_1jan,zcol="P1",cex = "P1") 
alt<-mapview(DEM_stut200)
shpplot+poin+alt

#Proper data extract for each day 

##for the 1 jan 2018 
stutgartdata_jan2018_1jan<-getsttutgartdata(date='2018-01-01',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_1jan[stutgartdata_jan2018_1jan == 0] <- NA
summary(stutgartdata_jan2018_1jan)
stutgartdata_jan2018_1jan<-na.omit(stutgartdata_jan2018_1jan)
summary(stutgartdata_jan2018_1jan)

##For the 2nd january of the year 2018 
stutgartdata_jan2018_2jan<-getsttutgartdata(date='2018-01-02',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_2jan[stutgartdata_jan2018_2jan == 0] <- NA
summary(stutgartdata_jan2018_2jan)
stutgartdata_jan2018_2jan<-na.omit(stutgartdata_jan2018_2jan)

##3rd Janurary 2018
stutgartdata_jan2018_3jan<-getsttutgartdata(date='2018-01-03',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_3jan[stutgartdata_jan2018_3jan == 0] <- NA
summary(stutgartdata_jan2018_3jan)
stutgartdata_jan2018_3jan<-na.omit(stutgartdata_jan2018_3jan)

##4 January 2018
stutgartdata_jan2018_4jan<-getsttutgartdata(date='2018-01-04',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid =sensor_in_stutgart)
stutgartdata_jan2018_4jan[stutgartdata_jan2018_4jan == 0] <- NA
summary(stutgartdata_jan2018_4jan)
stutgartdata_jan2018_4jan<-na.omit(stutgartdata_jan2018_4jan)

##5th January 2018 
stutgartdata_jan2018_5jan<-getsttutgartdata(date='2018-01-05',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_5jan[stutgartdata_jan2018_5jan == 0] <- NA
summary(stutgartdata_jan2018_5jan)
stutgartdata_jan2018_5jan<-na.omit(stutgartdata_jan2018_5jan)

##6th January 2018 
stutgartdata_jan2018_6jan<-getsttutgartdata(date='2018-01-06',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_6jan[stutgartdata_jan2018_6jan == 0] <- NA
summary(stutgartdata_jan2018_6jan)
stutgartdata_jan2018_6jan<-na.omit(stutgartdata_jan2018_6jan)

#7th January 2018 
stutgartdata_jan2018_7jan<-getsttutgartdata(date='2018-01-07',sensorname="sds011",type="sensor",baseURL='http://archive.luftdaten.info',sensorid = sensor_in_stutgart)
stutgartdata_jan2018_7jan[stutgartdata_jan2018_7jan == 0] <- NA
summary(stutgartdata_jan2018_7jan)
stutgartdata_jan2018_7jan<-na.omit(stutgartdata_jan2018_7jan)

##comparing the station ids 
nrow(stutgartdata_jan2018_1jan)
nrow(stutgartdata_jan2018_2jan)
nrow(stutgartdata_jan2018_3jan)
nrow(stutgartdata_jan2018_4jan)
nrow(stutgartdata_jan2018_5jan)
nrow(stutgartdata_jan2018_6jan)
nrow(stutgartdata_jan2018_7jan)

#merging first 2 days data 
db1<-merge(stutgartdata_jan2018_1jan,stutgartdata_jan2018_2jan,by='sensor_id')

nrow(db1)
db1<-merge(db1,stutgartdata_jan2018_3jan,by='sensor_id')
db1<-merge(db1,stutgartdata_jan2018_4jan,by='sensor_id')
db1<-full_join(stutgartdata_jan2018_1jan,stutgartdata_jan2018_2jan,by='sensor_id')
nrow(db1)
nrow(stutgartdata_jan2018_2jan)
stutgartdata_jan2018_1jan[which(duplicated(stutgartdata_jan2018_1jan$sensor_id)),]

#Adding jan2
db1<-merge(stutgartdata_jan2018_1jan,stutgartdata_jan2018_2jan,by='sensor_id')
names(db1)
db1[7]<-NULL
db1[7]<-NULL
db1[7]<-NULL
names(db1)[5:6]<-c("jan1_pm10","jan1_pm2.5")
names(db1)[7:8]<-c("jan2_pm10","jan2_pm2.5")
#adding jan 3
db1<-left_join(db1,stutgartdata_jan2018_3jan,by='sensor_id')
names(db1)
db1[9]<-NULL
db1[9]<-NULL
db1[9]<-NULL
names(db1)[9:10]<-c("jan3_pm10","jan3_pm2.5")
#Adding jan 4 
db1<-left_join(db1,stutgartdata_jan2018_4jan,by='sensor_id')
names(db1)
db1[11]<-NULL
db1[11]<-NULL
db1[11]<-NULL
names(db1)[11:12]<-c("jan4_pm10","jan4_pm2.5")

#Adding jan 5 
db1<-left_join(db1,stutgartdata_jan2018_5jan,by='sensor_id')
names(db1)
db1[13]<-NULL
db1[13]<-NULL
db1[13]<-NULL
names(db1)[13:14]<-c("jan5_pm10","jan5_pm2.5")

#Adding jan 6 
db1<-left_join(db1,stutgartdata_jan2018_6jan,by='sensor_id')
names(db1)
db1[15]<-NULL
db1[15]<-NULL
db1[15]<-NULL
names(db1)[15:16]<-c("jan6_pm10","jan6_pm2.5")

#Adding jan 7 
db1<-left_join(db1,stutgartdata_jan2018_7jan,by='sensor_id')
names(db1)
db1[17]<-NULL
db1[17]<-NULL
db1[17]<-NULL
names(db1)[17:18]<-c("jan7_pm10","jan7_pm2.5")
head(db1,n=10)

summary(db1)
#remove NA's
db1.nona<-na.omit(db1)
nrow(db1.nona)
Jan_1stweek_data<-db1.nona
write.csv2(Jan_1stweek_data,"1stWeek_jan")


weekdata_1jan_pm10<-data.frame(sensor_id=db1.nona$sensor_id,location=db1.nona$location.x,lat=db1.nona$lat.x,lon=db1.nona$lon.x,jan1=db1.nona$jan1_pm10,jan2=db1.nona$jan2_pm10,jan3=db1.nona$jan3_pm10,jan4=db1.nona$jan4_pm10,jan5=db1.nona$jan5_pm10,jan6=db1.nona$jan6_pm10,jan7=db1.nona$jan7_pm10)

#Aggregate the data 
names(weekdata_1jan_pm10)
names(weekdata_1jan_pm10[,5:11])
weekdata_1jan_pm10$aver<-rowMeans(weekdata_1jan_pm10[,5:11])
rowMeans(weekdata_1jan_pm10[1,5:11])

write.csv2(weekdata_1jan_pm10,"PM10_jan_1stweek2018")


weekdata_1jan_pm2.5<-data.frame(sensor_id=db1.nona$sensor_id,location=db1.nona$location.x,lat=db1.nona$lat.x,lon=db1.nona$lon.x,jan1=db1.nona$jan1_pm2.5,jan2=db1.nona$jan2_pm2.5,jan3=db1.nona$jan3_pm2.5,jan4=db1.nona$jan4_pm2.5,jan5=db1.nona$jan5_pm2.5,jan6=db1.nona$jan6_pm2.5,jan7=db1.nona$jan7_pm2.5)
#Aggregate the data 
weekdata_1jan_pm2.5$aver<-rowMeans(weekdata_1jan_pm2.5[,5:11])
#manual check 
mean(as.numeric(weekdata_1jan_pm2.5[1,5:11]))

write.csv2(weekdata_1jan_pm2.5,"PM2.5_jan_1stweek2018")


##geospatial operations 
###For PM10 
coordinates(weekdata_1jan_pm10)<-c("lon","lat")
proj4string(weekdata_1jan_pm10)<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
mapview(weekdata_1jan_pm10)
plot(stutgart_shp.trans)
plot(weekdata_1jan_pm10,add=T)

shapef<-mapview(stutgart_shp.trans)
pointf<-mapview(weekdata_1jan_pm2.5,zcol="aver",cex = "aver") 
shpplot+poin

summary(weekda)
 require(rgdal)
writeOGR(weekdata_1jan_pm10,".","January_1st_week",driver="ESRI Shapefile" )
?writeOGR

#Dealing with the traffic data for valiable sused in Regression 
Sttutgart_roads<-readOGR("/Users/shivamgupta/sciebo/Sttutgart optimisation paper/Sttutgart data/Stuttgart_Shape_files_32632/Stuttgart_roads_OTM.shp")

plot(Sttutgart_roads[Sttutgart_roads$trafficvol>5000,])

#$Deleate ID 2492
plot(weekdata_1jan_pm10[weekdata_1jan_pm10$sensor_id==2492,])
weekdata_1jan_pm10<-weekdata_1jan_pm10[-which(weekdata_1jan_pm10$sensor_id==2492),]
weekdata_1jan_pm10$sensor_id
weekdata_1jan_pm2.5<-weekdata_1jan_pm2.5[-which(weekdata_1jan_pm2.5$sensor_id==2492),]
weekdata_1jan_pm10$sensor_id

#adding the atitiude variable to all the list of other variables 
data_with_predictors<-read.csv("lur_trainingset_23June.csv")
data_with_predictors<-left_join(data_with_predictors,stutgartdata_jan2018_7jan[,1:4],by='sensor_id')
names(data_with_predictors)

coordinates(data_with_predictors)<-cbind(data_with_predictors$lon,data_with_predictors$lat)
crs(data_with_predictors)<-"+proj=longlat +datum=WGS84 +no_defs "
data_with_predictors<-spTransform(data_with_predictors,crs(stutgart_shp))

plot(stutgart_shp)
plot(data_with_predictors,add=T)
crs(stutgart_shp)
crs(data_with_predictors)

#squareroot of altitude for original configuration 
data_with_predictors$SQRALT<-extract(DEM_stut200,data_with_predictors)
data_with_predictors$SQRALT<-sqrt(data_with_predictors$SQRALT)
head(data_with_predictors$SQRALT)

#All  predictors in table with pollution concentration
write.csv2(data_with_predictors,"All_Sttutgart_predictors_24June_2one")
names(data_with_predictors)

nrow(weekdata_1jan_pm10)
nrow(weekdata_1jan_pm2.5)
complete_1stweek_pm10<-merge(weekdata_1jan_pm10,as.data.frame(data_with_predictors)[3:94],by="sensor_id")
complete_1stweek_pm2.5<-merge(weekdata_1jan_pm2.5,as.data.frame(data_with_predictors)[3:94],by="sensor_id")

names(complete_1stweek_pm10)
names(complete_1stweek_pm2.5)

head(complete_1stweek_pm10)
head(complete_1stweek_pm2.5)
#For PM10
names(complete_1stweek_pm10@data)
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[12]<-NULL
complete_1stweek_pm10@data[2]<-NULL

names(complete_1stweek_pm10@data)[3:10]<-c("jan1", "jan2" ,"jan3", "jan4" ,"jan5", "jan6", "jan7", "aver")
names(complete_1stweek_pm10@data)

  #c("jan5_pm10","jan5_pm2.5")
#import to shgap
writeOGR(complete_1stweek_pm10,".","Complete_1stweek_pm10",driver="ESRI Shapefile",overwrite_layer = T )
write.csv2(complete_1stweek_pm10,"CompleteCSV_1stweek_pm10")

#For PM2.5
names(complete_1stweek_pm2.5@data)
#removing the repeated field because of merge
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[12]<-NULL
complete_1stweek_pm2.5@data[2]<-NULL

names(complete_1stweek_pm2.5@data)[3:10]<-c("jan1", "jan2" ,"jan3", "jan4" ,"jan5", "jan6", "jan7", "aver")
names(complete_1stweek_pm2.5@data)
writeOGR(complete_1stweek_pm2.5,".","Complete_1stweek_pm25",driver="ESRI Shapefile",overwrite_layer = T )
write.csv2(complete_1stweek_pm2.5,"CompleteCSV_1stweek_pm25")


