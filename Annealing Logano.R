#Lugano, Switzerland 
#46.30 + 2.25 * 10-4 * HEAVYTRAFLOAD_50-0.57 * SQRALT-6.90 * 10-7 * NATURAL_5000


covars_lugano<-data.frame(covars_all.sp$HEAVYTRAFLOAD_50,covars_all.sp$SQRALT,covars_all.sp$NATURAL_5000)
summary(covars_lugano)

schedule_stut <- scheduleSPSANN(initial.temperature = 20,initial.acceptance = 0.40,chains = 400)

lugano_res<- optimUSER(points =points_original ,fun =find_optimal_locations,covars=covars_lugano,candi = candi_stut,schedule = schedule_stut,plotit = T,track = T )


lugano_res<- optimUSER(points =points_original ,fun =find_optimal_locations,covars=covars_lugano,candi = candi_stut,schedule = schedule_stut,plotit = T,track = T )

objSPSANN(lugano_res)
plot(lugano_res)




require(spsann)

plot(lm_all_aic)
