#previous annealing value 
#annel_val<-c(0.02062096,0.04147349,0.03502074,0.02136949,0.02079705,0.02131766,0.02068221,0.04548916,0.02600325,0.02087039,0.02048747,0.02094913,0.02050425,0.02089445,0.02069625)
#min(annel_val)

candi_stut
ddaa<-head(candi_stut,10)
class(candi.copy[1:12, 1:2])
x<-SpatialTools::dist2(as.matrix(candi.copy[12:112, 1:2]),as.matrix(candi.copy[18:25, 1:2]))

out<-vector(length = nrow(x))
xi<-vector(length = ncol(x))
xi1<-vector(length = ncol(x))

for(i in 1:nrow(x)){
  for (j in 1:ncol(x)){
    xi[j] = x[i, j]
    j=j+1
  }
  out[i] = min(xi)
i=i+1
}

print(mean(out^2))

#try one 
wellspreadoptimallocaitons<-function (points,covars,candis) {
  #spread
  mat<-SpatialTools::dist2(as.matrix(candis[, 1:2]),as.matrix(points[, 1:2]))
  out<-vector(length = nrow(mat))
  xi<-vector(length = ncol(mat))
  for(i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      xi[j] = mat[i, j]
      j=j+1
    }
    out[i] = min(xi)
    i=i+1
  }
  spread<-mean(out)
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  meanreng<-mean(eng)
  energy<-(spread*0.3)+(meanreng*0.7)
  return(energy)
}
debug(wellspreadoptimallocaitons)
austria_res_20_15<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitons,candis=candi_stut,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,plotit  = T )

#takes alot of time 6:30 and same clustered results 

#try 2
wellspreadoptimallocaitons<-function (points,covars,candis) {
  #spread
  mat<-SpatialTools::dist2(as.matrix(candis[, 1:2]),as.matrix(points[, 1:2]))
  out<-vector(length = nrow(mat))
  xi<-vector(length = ncol(mat))
  for(i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      xi[j] = mat[i, j]
      j=j+1
    }
    out[i] = min(xi)
    i=i+1
  }
  spread<-mean(out)
  #Prediction error 
  sm <- as.matrix(covars[points[, 1], ])
  xtx<-(t(sm)%*%sm)
  inv.xtx<-chol2inv(chol(as.matrix(xtx)))
  #inv.xtx<-MASS::ginv(as.matrix(xtx))
  eng <- vector("numeric", nrow(covars))
  for(i in 1:nrow(covars)){eng[i]<-sum(as.matrix(covars[i,])%*%inv.xtx%*%t(as.matrix(covars[i,])))}
  meanreng<-mean(eng)
  energy<-(spread*0.3)+(meanreng*0.7)
  return(energy)
}
debug(wellspreadoptimallocaitons)

austria_res_20_15<-optimUSER(points =points_original ,fun =wellspreadoptimallocaitons,candis=candi_stut,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,plotit  = T )

austria_res_20_15<-userfunc(points =points_original ,fun =wellspreadoptimallocaitons,candi = candi_stut,covars=covars_austria,schedule = schedule_stut20_acc15,plotit  = T )




#Test bed
head(complete_1stweek_pm10@coords)
mat<-SpatialTools::dist1(complete_1stweek_pm10.trans32@coords)

spatitoo<-SpatialTools::dist1(complete_1stweek_pm10.trans32@coords )

geosph<-geosphere::distm(complete_1stweek_pm10)

head(geosph)

head(mat)

geosphere::distm(complete_1stweek_pm10[10:15,])
SpatialTools::dist1(complete_1stweek_pm10@coords)



mat<-geosphere::distm(complete_1stweek_pm10[10:15,])
out<-vector(length = nrow(mat))
xi<-vector(length = ncol(mat))

for(i in 1:nrow(mat)){
  for (j in 1:ncol(mat)){
    xi[j] = mat[i, j]
    xi<-xi[xi!=0]
    j=j+1
  }
  
  out[i] = min(xi)
  i=i+1
}
xi
out






geosphere::distm(complete_1stweek_pm10[1:5,])
complete_1stweek_pm10[1:5,]@data























