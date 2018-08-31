
wellspread_optimisation_weights<-function (points,covars,weights) {
  
  #Weights 1st spread,2nd prediction
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  meanreng<-mean(eng)
  
  #spread
  min.nnew<-function(x,n,value=TRUE){ 
    if(value==TRUE){x[order(x)][n]} else {order(x)[n]}}
  mat<-SpatialTools::dist1(points[,2:3])
  out<-vector(length = nrow(mat))
  xi<-vector(length = ncol(mat))
  for(i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      xi[j] = mat[i, j]
      j=j+1
    }
    
    out[i] = min.nnew(xi,2)
    i=i+1
  }
  spread<-mean(out)
  inversespread<-1/spread
  energy<-((inversespread*weights[1])+(meanreng*weights[2]))
  
  return(list(energy,meanreng,inversespread))
}

austria_res_20_15.12august<-optimUSER(points =points_original[1:7,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.10,0.90),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.12august)

x.df<-data.frame(austria_res_20_15.12august$objective$energy,austria_res_20_15.12august$objective$predictionerror,austria_res_20_15.12august$objective$spread_value)
#(0.90*119.2439249) +(0.0006999112*0.10)
#107.3196024 

trace(optimUSER, edit=TRUE)
trace(spsann:::.prepare_output, edit=TRUE)

plot(austria_res_20_15)


#normal spread of sensors with 116 stations 
aug12.austria_res_20_15.90_10<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.90,0.10),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )

tail(data.frame(aug12.austria_res_20_15.90_10$objective$predictionerror,aug12.austria_res_20_15.90_10$objective$spread_value))
plot(aug12.austria_res_20_15.90_10)

aug12.austria_res_20_15.80_20<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.80,0.20),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
head(data.frame(aug12.austria_res_20_15.80_20$objective$predictionerror,aug12.austria_res_20_15.80_20$objective$spread_value))

plot(aug12.austria_res_20_15.80_20)

aug12.austria_res_20_15.70_30<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.70,0.30),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.70_30)

aug12.austria_res_20_15.60_40<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.60,0.40),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.60_40)

aug12.austria_res_20_15.50_50<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.50,0.50),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.50_50)

aug12.austria_res_20_15.40_60<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.40_60)

aug12.austria_res_20_15.30_70<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.30,0.70),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.30_70)

aug12.austria_res_20_15.20_80<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.20,0.80),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.20_80)

aug12.austria_res_20_15.10_90<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.10,0.90),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(aug12.austria_res_20_15.10_90)


#Chnaing the number of stations 
#50
aug12.austria_res_20_15.40_60.20<-optimUSER(points =points_original[1:20,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )

aug12.austria_res_20_15.40_60.30<-optimUSER(points =points_original[1:30,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )

aug12.austria_res_20_15.40_60.40<-optimUSER(points =points_original[1:40,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


aug12.austria_res_20_15.40_60.50<-optimUSER(points =points_original[1:50,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


aug12.austria_res_20_15.40_60.60<-optimUSER(points =points_original[1:60,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


aug12.austria_res_20_15.40_60.70<-optimUSER(points =points_original[1:70,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )

#Start here#

aug12.austria_res_20_15.40_60.80<-optimUSER(points =points_original[1:80,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


aug12.austria_res_20_15.40_60.90<-optimUSER(points =points_original[1:90,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


aug12.austria_res_20_15.40_60.100<-optimUSER(points =points_original[1:100,] ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


save.image("~/sciebo/Sttutgart optimisation paper/Post 2 August /14th August different number try.RData")

#Extending network 
aug12.austria_res_20_15.40_60.extend20<-optimUSER(points = list(fixed=points_original,free=20) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track  = T )

aug12.austria_res_20_15.40_60.extend40<-optimUSER(points = list(fixed=points_original,free=40) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track  = T )

aug12.austria_res_20_15.40_60.extend60<-optimUSER(points = list(fixed=points_original,free=60) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )

aug12.austria_res_20_15.40_60.extend80<-optimUSER(points = list(fixed=points_original,free=80) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track  = T )

aug12.austria_res_20_15.40_60.extend100<-optimUSER(points = list(fixed=points_original,free=100) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track  = T )

aug12.austria_res_20_15.40_60.extend120<-optimUSER(points = list(fixed=points_original,free=120) ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track  = T )

save.image("~/sciebo/Sttutgart optimisation paper/Post 2 August /15th August extending_austria included.RData")

#Sttutgart LUR
covars_LUR_stut<-data.frame(covars_all.sp$SQRALT,covars_all.sp$buildcount_500,covars_all.sp$INDUSTRY_300, covars_all.sp$MAJORROADLENGTH_1000,covars_all.sp$LDRES_1000)

stutLUR_res_schedule_1_10_400 <- scheduleSPSANN(initial.temperature = 1,initial.acceptance = 0.10,chains = 400)

stut_lur_spread_10_90<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.10,0.90),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_20_80<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.20,0.80),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_40_60<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.40,0.60),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_50_50<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.50,0.50),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_60_40<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.60,0.40),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_70_30<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.70,0.30),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_80_20<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.80,0.20),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_90_10<- optimUSER(points =points_original ,fun =wellspread_optimisation_weights,weights=c(0.90,0.10),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

save.image("~/sciebo/Sttutgart optimisation paper/Post 2 August /14th August Stut_LUR included.RData")

#extending the network
stut_lur_spread_30_70.extend20<- optimUSER(points = list(fixed=points_original,free=20) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70.extend40<- optimUSER(points = list(fixed=points_original,free=40) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70.extend60<- optimUSER(points = list(fixed=points_original,free=60) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70.extend80<- optimUSER(points = list(fixed=points_original,free=80) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70.extend100<- optimUSER(points = list(fixed=points_original,free=100) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

stut_lur_spread_30_70.extend120<- optimUSER(points = list(fixed=points_original,free=120) ,fun =wellspread_optimisation_weights,weights=c(0.30,0.70),covars=covars_LUR_stut,candi = candi_stut,schedule = stutLUR_res_schedule_1_10_400,track = T )

save.image("~/sciebo/Sttutgart optimisation paper/Post 2 August /14th August extending_Stut_LUR network.RData")