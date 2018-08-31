#Annealing for Sttutgart 

find_optimal_locations <- function (points,covars) {
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  #inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  return(meanreng<-mean(eng))
}

extract_points<-function(optim){
  optim$points->x
  coordinates(x)<- c("x","y")
  return(x)
}

points_stut_original<-data.frame(id=1:nrow(original_sttut_dataset),x=coordinates(original_stut_sp)[,1],y=coordinates(original_stut_sp)[,2])
candi_stut<-coordinates( candi_500m)
covars_all<-read.csv("New datasheets/lur_trainingset500m.csv")
head(covars_all)
covars_all$SQRALT<-covars_all.sp$SQRALT
covars_stut<-covars_all

covars_all[is.na(covars_all)] <- 0
#Annealing 
#Helsinki model 
#9.25-6.75 * 10-6 * NATURAL_500 + 6.34 * 10-7 * TRAFMAJORLOAD_50
covars_helsinki<-data.frame(covars_all$NATURAL_500,covars_all$TRAFMAJORLOAD_50)

schedule_stut <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.40,chains = 400)

helsinki_res<- optimUSER(points =points_stut_original , fun =find_optimal_locations,covars=covars_helsinki,candi = candi_stut,schedule = schedule_stut,plotit = T,track = T )
objSPSANN(helsinki_res)
plot(helsinki_res)



greece_res<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut,plotit = T,track = T )



objSPSANN(greece_res)
plot(greece_res)

greece_res_70points<-greece_res


sm <- as.matrix(covars_helsinki[1:10, ])
xtx<-(t(sm)%*%sm)
#inv.xtx<-chol2inv(chol(as.matrix(xtx)))
inv.xtx<-MASS::ginv((as.matrix(xtx)))
#inv.xtx<-solve(as.matrix(xtx))
eng <- vector("numeric", nrow(covars))
for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars_helsinki[i,])%*%inv.xtx%*%t(as.matrix(covars_helsinki[i,])))}
return(meanreng<-mean(eng))

?optimUSER

require(spsann)

#Greece
#13.98 + 2.04 * 10-8 * TRAFLOAD_500-1.77 * 10-7 * NATURAL_5000 + 0.017 * ROADLENGTH_25+ 1.52 * 10-5 * INDUSTRY_300 + 1.80 * 10-2 * MAJORROADLENGTH_50
covars_greece<-data.frame(covars_all$TRAFLOAD_500,covars_all$NATURAL_5000,covars_all$ROADLENGTH_25,covars_all$INDUSTRY_300,covars_all$MAJORROADLENGTH_50)

#A low initial.temperature, combined with a low initial.acceptance result in the algorithm to behave as a greedy algorithm, i.e. only better system configurations are accepted. 
#temp=20
schedule_stut20_acc05 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.05,chains = 400)
schedule_stut20_acc10 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.10,chains = 400)
schedule_stut20_acc15 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.15,chains = 400)
schedule_stut20_acc20 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.20,chains = 400)
schedule_stut20_acc25 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.25,chains = 400)
schedule_stut20_acc30 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.30,chains = 400)
schedule_stut20_acc35 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.35,chains = 400)
schedule_stut20_acc40 <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.40,chains = 400)

#temp=15
schedule_stut15_acc05 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.05,chains = 400)
schedule_stut15_acc10 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.10,chains = 400)
schedule_stut15_acc15 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.15,chains = 400)
schedule_stut15_acc20 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.20,chains = 400)
schedule_stut15_acc25 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.25,chains = 400)
schedule_stut15_acc30 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.30,chains = 400)
schedule_stut15_acc35 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.35,chains = 400)
schedule_stut15_acc40 <- scheduleSPSANN(initial.temperature = 15,initial.acceptance = 0.40,chains = 400)

#temp=10
schedule_stut10_acc05 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.05,chains = 400)
schedule_stut10_acc10 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.10,chains = 400)
schedule_stut10_acc15 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.15,chains = 400)
schedule_stut10_acc20 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.20,chains = 400)
schedule_stut10_acc25 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.25,chains = 400)
schedule_stut10_acc30 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.30,chains = 400)
schedule_stut10_acc35 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.35,chains = 400)
schedule_stut10_acc40 <- scheduleSPSANN(initial.temperature = 10,initial.acceptance = 0.40,chains = 400)

#temp=5

schedule_stut05_acc05 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.05,chains = 400)
schedule_stut05_acc10 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.10,chains = 400)
schedule_stut05_acc15 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.15,chains = 400)
schedule_stut05_acc20 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.20,chains = 400)
schedule_stut05_acc25 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.25,chains = 400)
schedule_stut05_acc30 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.30,chains = 400)
schedule_stut05_acc35 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.35,chains = 400)
schedule_stut05_acc40 <- scheduleSPSANN(initial.temperature = 5,initial.acceptance = 0.40,chains = 400)

#temp=1
schedule_stut01_acc05 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.05,chains = 400)
schedule_stut01_acc10 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.10,chains = 400)
schedule_stut01_acc15 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.15,chains = 400)
schedule_stut01_acc20 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.20,chains = 400)
schedule_stut01_acc25 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.25,chains = 400)
schedule_stut01_acc30 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.30,chains = 400)
schedule_stut01_acc35 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.35,chains = 400)
schedule_stut01_acc40 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.40,chains = 400)

#20
greece_res_20_05<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc05,track = T )

greece_res_20_10<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc10,track = T )

greece_res_20_15<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc15,track = T )

greece_res_20_20<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc20,track = T )

greece_res_20_25<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc25,track = T )

greece_res_20_30<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc30,track = T )

greece_res_20_35<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc35,track = T )

greece_res_20_40<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut20_acc40,track = T )

#15
greece_res_15_05<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc05,track = T )

greece_res_15_10<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc10,track = T )

greece_res_15_15<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc15,track = T )

greece_res_15_20<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc20,track = T )

greece_res_15_25<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc25,track = T )

greece_res_15_30<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc30,track = T )

greece_res_15_35<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc35,track = T )

greece_res_15_40<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut15_acc40,track = T )

#10
greece_res_10_05<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc05,track = T )

greece_res_10_10<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc10,track = T )

greece_res_10_15<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc15,track = T )

greece_res_10_20<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc20,track = T )

greece_res_10_25<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc25,track = T )

greece_res_10_30<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc30,track = T )

greece_res_10_35<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc35,track = T )

greece_res_10_40<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut10_acc40,track = T )


#5
greece_res_05_05<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc05,track = T )

greece_res_05_10<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc10,track = T )

greece_res_05_15<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc15,track = T )

greece_res_05_20<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc20,track = T )

greece_res_05_25<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc25,track = T )

greece_res_05_30<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc30,track = T )

greece_res_05_35<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc35,track = T )

greece_res_05_40<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut05_acc40,track = T )


#1
greece_res_01_05<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc05,track = T )

greece_res_01_10<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc10,track = T )

greece_res_01_15<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc15,track = T )

greece_res_01_20<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc20,track = T )

greece_res_01_25<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc25,track = T )

greece_res_01_30<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc30,track = T )

greece_res_01_35<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc35,track = T )

greece_res_01_40<- optimUSER(points = points_stut_original, fun =find_optimal_locations,covars=covars_greece,candi = candi_stut,schedule = schedule_stut01_acc40,track = T )



require(rgdal)
Stut_roads <- readOGR(dsn = "Sttutgart data/Stuttgart_Shape_files_32632/Stuttgart_roads_OTM.shp")
plot(Stut_roads)
plot(greece_res_01_40)
plot(stutgart_shp,main=paste("Predicition error =",round(objSPSANN(greece_res_01_40),5)))
extract_points(greece_res_01_40) %>%plot(.,pch=16,col="red",add=T)



plot(greece_res_01_35)
plot(stutgart_shp,main=paste("Predicition error =",round(objSPSANN(greece_res_01_35),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
p<-extract_points(greece_res_01_35) %>%plot(.,pch=16,col="red",add=T)


objective_value_temp20=c(objSPSANN(greece_res_20_05),objSPSANN(greece_res_20_10),objSPSANN(greece_res_20_15),objSPSANN(greece_res_20_20),objSPSANN(greece_res_20_25),objSPSANN(greece_res_20_30),objSPSANN(greece_res_20_35),objSPSANN(greece_res_20_40))

objective_value_temp20<-as.data.frame(objective_value_temp20)
objective_value_temp20<-t(objective_value_temp20)

prob=c(.05,.10,.15,.20,.25,.30,.35,.40)
objective_value_temp20<-data.frame(objective_value_temp20,prob)
names(objective_value_temp20)

p2 <- ggplot(objective_value_temp20,aes(y = objective_value_temp20, x = prob), size=1.5) +geom_point()+ geom_line()+geom_text(aes(label=round(objective_value_temp20,6)),hjust = 0.66,vjust=-1.3,nudge_x = 0,size=2.7)

p2<- p2+ labs(x="Probability of acceptance", y="Objective values")
p2



par(mfrow=c(2,4))
plot(stutgart_shp,main=paste("(5)",round(objSPSANN(greece_res_10_05),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_05) %>%plot(.,pch=16,col="red",add=T)



plot(stutgart_shp,main=paste("(10)",round(objSPSANN(greece_res_10_10),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_10) %>%plot(.,pch=16,col="red",add=T)


plot(stutgart_shp,main=paste("(15)",round(objSPSANN(greece_res_10_15),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_15) %>%plot(.,pch=16,col="red",add=T)



plot(stutgart_shp,main=paste("(20)",round(objSPSANN(greece_res_10_20),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_20) %>%plot(.,pch=16,col="red",add=T)


plot(stutgart_shp,main=paste("(25)",round(objSPSANN(greece_res_10_25),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_25) %>%plot(.,pch=16,col="red",add=T)

plot(stutgart_shp,main=paste("(30)",round(objSPSANN(greece_res_10_30),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_30) %>%plot(.,pch=16,col="red",add=T)


plot(stutgart_shp,main=paste("(35)",round(objSPSANN(greece_res_10_35),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_35) %>%plot(.,pch=16,col="red",add=T)

plot(stutgart_shp,main=paste("(40)",round(objSPSANN(greece_res_10_40),5)))
plot(Stut_minor,add=T,col="grey")
plot(Stut_majorroad,add=T)
extract_points(greece_res_10_40) %>%plot(.,pch=16,col="red",add=T)