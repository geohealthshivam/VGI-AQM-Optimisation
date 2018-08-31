wellspreadoptimallocaitons<-function (points,covars) {
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
energy<-((1/spread)+meanreng)
  return(energy)
}
debug(wellspreadoptimallocaitons)
austria_res_20_15<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitons,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,plotit  = T )



austria_res_20_15.wellspreadoptimallocaitons1<-austria_res_20_15
plot(austria_res_20_15.wellspreadoptimallocaitons1)

#Changes to the function agan (mean to mean square)


wellspreadoptimallocaitonsmeansquaremultiply<-function (points,covars) {
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  energy_out<-mean(eng)
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
  energy<-((1/spread)+(meanreng*0.7))
  
  return(energy)
}

austria_res_20_15.wellspreadoptimallocaitonsmeansquaremultiply<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitonsmeansquaremultiply,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,track = T )




#weights 

wellspreadoptimallocaitonsmeansquaremultiply<-function (points,covars) {
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  energy_out<-mean(eng)
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
  energy<-((1/spread)+(meanreng*0.7))
  
  return(energy)
}

austria_res_20_15.wellspreadoptimallocaitonsweightpredicterror<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitonsmeansquaremultiply,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,track = T )


#weights on both
wellspreadoptimallocaitonsweightboth<-function (points,covars) {
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  energy_out<-mean(eng)
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
  energy<-((1/spread*0.2)+(meanreng*0.8))
  
  return(energy)
}



#min.nnew<-function(x,n,value=TRUE){ 
#  if(value==TRUE){x[order(x)][n]} else {order(x)[n]}}
austria_res_20_15.wellspreadoptimallocaitonsweightpredicterror<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitonsweightboth,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
austria_res_20_15.20_80<-austria_res_20_15.wellspreadoptimallocaitonsweightpredicterror

wellspreadoptimallocaitonsweightboth2<-function (points,covars) {
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  energy_out<-mean(eng)
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
  energy<-((1/spread*0.8)+(meanreng*0.2))
  
  return(energy)
}
austria_res_20_15.wellspreadoptimallocaitonsweightboth2<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitonsweightboth2,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,track = T )



austria_res_20_15.80_20<-austria_res_20_15.wellspreadoptimallocaitonsweightboth2

wellspread_optimisation_weights<-function (points,covars,weights) {
 
#Weights 1st spread,2nd prediction
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  energy_out<-mean(eng)
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
  energy<-((1/spread*weights[1])+(meanreng*weights[2]))
  
  return(energy)
}

austria_res_20_15.90_10<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.90,0.10),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.90_10)

austria_res_20_15.70_30<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.70,0.30),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.70_30)

austria_res_20_15.60_40<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.60,0.40),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.60_40)
austria_res_20_15.50_50<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.50,0.50),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.50_50)

austria_res_20_15.40_60<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.40,0.60),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.40_60)

austria_res_20_15.30_70<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.30,0.70),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.30_70)

austria_res_20_15.10_90<-optimUSER(points =points_original ,fun =wellspread_optimisation_weights,candi = candi_stut, weights=c(0.10,0.90),covars=covars_austria,schedule = schedule_stut20_acc15,track = T )
plot(austria_res_20_15.10_90)




try1<-function(i,j){
  i<-i+1
  j<-j+1
  z<-c(i,j)
  #return(z)
  return(list(z, i, j))
}
try1(1,1)


head(resUSER_pop_80$spsann$cellsize
     )


objSPSANN(aug15.austria_lur_spread_01_01)
#obj
0.004218969

objSPSANN(aug15.austria_lur_spread_01_05)
#obj
0.004224388

objSPSANN(aug15.austria_lur_spread_01_10)
#obj
0.004255462

objSPSANN(aug15.austria_lur_spread_01_15)
#obj
0.004234387

objSPSANN(aug15.austria_lur_spread_01_20)
#obj
0.004235477

objSPSANN(aug15.austria_lur_spread_01_25)
#obj
0.004268496

objSPSANN(aug15.austria_lur_spread_01_30)
#obj
0.004236818

