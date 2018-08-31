#value_types: P1 (PM10), P2 (PM2.5)

ESCAPEvalues<-data.frame(PM10=double(110))

ESCAPEvalues$PM10<-original_sttut_dataset$P1
ESCAPEvalues$PM25<-original_sttut_dataset$P2
ESCAPEvalues_pm25<-ESCAPEvalues
ESCAPEvalues_pm25$PM10<-NULL
ESCAPEvalues_pm25$PM25
#Pm2.5
#Stockholm County, Sweden (Not applicable water not there)
7.95-8.96 * 10-6 * WATER_500-1.48 * 10-7 * WATER_500_5000 + 1.37 * 10-5 * HEAVYTRAFLOAD_50 + 3.66 * 10-4 * ROADLENGTH_500

#Helsinki/Turku, Finland 
9.25-6.75 * 10-6 * NATURAL_500 + 6.34 * 10-7 * TRAFMAJORLOAD_50
ESCAPEvalues_pm25$Helsinki<-9.25-6.75 * 10-6 * original_sttut_dataset$NATURAL_500 + 6.34 * 10-7 * original_sttut_dataset$TRAFMAJORLOAD_50

#Copenhagen, Denmark (not applicable as green is not available)

9.12 + 1.96 * 10-4 * ROADLENGTH_500-2.20 * 10-3 * GREEN_100d

#Manchester, UK  
9.41 + 1.24 * 10-6 * HDRES_1000
ESCAPEvalues_pm25$Manchester<-9.41 + 1.24 * 10-6 * original_sttut_dataset$HDRES_1000
#London/Oxford, UK 
7.19 + 1.38 * 10-3 * INTMAJORINVDIST + 2.65 * 10-4 * ROADLENGTH_500
ESCAPEvalues_pm25$London<-7.19 + 1.38 * 10-3 * original_sttut_dataset$INTMAJORINVDIST + 2.65 * 10-4 * original_sttut_dataset$ROADLENGTH_500

#Netherlands/Belgium (Regional estimate not there)
9.46 + 0.42 * REGIONALESTIMATE + 0.01 * MAJORROADLENGTH_50 + 2.28 * 10-9 *
TRAFMAJORLOAD_1000

#Munich-Augsburg, Germany 

11.90 + 1.94 * 10-2 * MAJORROADLENGTH_50 + 4.95 * 10-4 * ROADLENGTH_300-14.30 *
URBGREEN_5000 + 7.41 * 10-9 * TRAFMAJORLOAD_1000

ESCAPEvalues_pm25$Munich<-11.90 + 1.94 * 10-2 * original_sttut_dataset$MAJORROADLENGTH_50 + 4.95 * 10-4 * original_sttut_dataset$ROADLENGTH_300-14.30 *
  original_sttut_dataset$URBGREEN_5000 + 7.41 * 10-9 * original_sttut_dataset$TRAFMAJORLOAD_1000


#Vorarlberg, Austria  (Building 100)
25.44 + 0.11 * BUILDINGS_100-0.65 * SQRALT

#Paris, France 

10.38 + 5.34 * 10-4 * MAJORROADLENGTH_500 + 2.75 * 10-7 * INDUSTRY_5000 + 1.46 * 10-4 *
TRAFMAJOR

ESCAPEvalues_pm25$Paris<-10.38 + 5.34 * 10-4 * original_sttut_dataset$MAJORROADLENGTH_500 + 2.75 * 10-7 * original_sttut_dataset$INDUSTRY_5000 + 1.46 * 10-4 *
  original_sttut_dataset$TRAFMAJOR

#Gyor, Hungary
23.98-1.71 * 10-2 * URBGREEN_5000 + 7.52 * 10-5 * ROADLENGTH_1000 + 5.90 * 10-8 *
TRAFMAJORLOAD_500
ESCAPEvalues_pm25$Gyor<-23.98-1.71 * 10-2 * original_sttut_dataset$URBGREEN_5000 + 7.52 * 10-5 * original_sttut_dataset$ROADLENGTH_1000 + 5.90 * 10-8 *
  original_sttut_dataset$TRAFMAJORLOAD_500

#Lugano, Switzerland 
46.30 + 2.25 * 10-4 * HEAVYTRAFLOAD_50-0.57 * SQRALT-6.90 * 10-7 * NATURAL_5000

ESCAPEvalues_pm25$Lugano<-46.30 + 2.25 * 10-4 * original_sttut_dataset$HEAVYTRAFLOAD_50-0.57 * original_sttut_dataset$SQRALT-6.90 * 10-7 * original_sttut_dataset$NATURAL_5000

#Turin, Italy 
24.90-7.03 * 10-6 * NATURAL_1000 + 9.40 * 10-7 * TRAFMAJORLOAD_50 + 1.63 * 10-7 *
LDRES_5000

ESCAPEvalues_pm25$Turin<-24.90-7.03 * 10-6 * original_sttut_dataset$NATURAL_1000 + 9.40 * 10-7 * original_sttut_dataset$TRAFMAJORLOAD_50 + 1.63 * 10-7 *
  original_sttut_dataset$LDRES_5000

#Rome, Italy 
16.08 + 4.56 * 10-6 * TRAFLOAD_25 + 3.81 * 10-3 * ROADLENGTH_100
ESCAPEvalues_pm25$Rome<-16.08 + 4.56 * 10-6 * original_sttut_dataset$TRAFLOAD_25 + 3.81 * 10-3 * original_sttut_dataset$ROADLENGTH_100

#Barcelona, Spain (Green 1000 not there )
16.21-4.08 * 10-6 * GREEN_1000 + 2.04 * 10-7 * original_sttut_dataset$TRAFLOAD_100 + 6.82 * 10-3 * original_sttut_dataset$INTINVDIST2

ESCAPEvalues_pm25$Barcelona<-16.21-4.08 * 10-6 * GREEN_1000 + 2.04 * 10-7 * TRAFLOAD_100 + 6.82 * 10-3 * INTINVDIST2

#Catalunya, Spain (Green 1000 not there )
14.88 + 9.91 * 10-4 * INTMAJORINVDIST-3.27 * 10-6 * GREEN_1000 + 5.36 * 10-7 * PORT_5000

#Athens, Greece 
13.98 + 2.04 * 10-8 * TRAFLOAD_500-1.77 * 10-7 * NATURAL_5000 + 0.017 * ROADLENGTH_25
+ 1.52 * 10-5 * INDUSTRY_300 + 1.80 * 10-2 * MAJORROADLENGTH_50

ESCAPEvalues_pm25$Athens<-13.98 + 2.04 * 10-8 * original_sttut_dataset$TRAFLOAD_500-1.77 * 10-7 * original_sttut_dataset$NATURAL_5000 + 0.017 * original_sttut_dataset$ROADLENGTH_25+ 1.52 * 10-5 * original_sttut_dataset$INDUSTRY_300 + 1.80 * 10-2 * original_sttut_dataset$MAJORROADLENGTH_50

#Heraklion, Greece 
ESCAPEvalues_pm25$Heraklion<-12.95 + 0.03 * original_sttut_dataset$ROADLENGTH_25 + 9.06 * 10-6 * original_sttut_dataset$HDRES_300

round(ESCAPEvalues_pm25)


#PM10
#Oslo, Norway
20.47-0.69*SQRALT + 0.29*MAJORROADLENGTH_25

#Stockholm County, Sweden
6.01 + 5.26E-6 * TRAFLOAD_50 + 3.74E-5 * HLDRES_300

#Copenhagen, Denmark
14.67 + 1.79E-3*MAJORROADLENGTH_300-2.49E-5*GREEN_3004 + 2.52E-7*PORT_5000

#Manchester
17.00-1.43E-4*URBGREEN_100 + 1.91E-8*HEAVYTRAFMAJORLOAD_1000-6.25E-6*NATURAL_300 + 7.87E-5*HDRES_100

#London/Oxford, UK
11.40 + 76.99*DISTINVMAJORC1 + 1.35E-3*HEAVYTRAFMAJOR + 1.30E-5*HLDRES_300

#Munich/Augsburg, Germany
18.47 + 3.89E-2*MAJORROADLENGTH_50-56.65*NATURAL_100 + 2.07E-2*ROADLENGTH_50

#Vorarlberg, Austria
41.25-1.16*SQRALT + 2.23E-2*BUILDINGS_300 + 5.04E-2*ROADLENGTH_25-4.76E-6*URBGREEN_5000 + 3.-2E2*DISTINVMAJORC2

#Paris, France
13.99 + 1.62E-3*MAJORROADLENGTH_500 + 1.49E-7*LDRES_5000 + 2.57E-4*TRAFMAJOR

#Gyor, Hungary
29.77 + 1.71E-2*HLDRES_1000 + 1.56E-2*MAJORROADLENGTH_100-1.15E-2*URBGREEN_5000

#Lugano, Switzerland
23.14 + 2.35E-6*TRAFLOAD_50-5.31E-6*NATURAL_1000-2.98E-5*URBGREEN_500
#Rome, Italy
28.02 + 1.11E-5*TRAFLOAD_25 + 3.37E-2*ROADLENGTH_50
#Barcelona, Spain
38.51-0.91*SQRALT + 3.06E-3*INTINVDIST +4.25E-2*ROADLENGTH_25


#Catalunya, Spain
38.42-2.33E-7*GREEN_5000 + 0.15*MAJORROADLENGTH_25 + 1.95E-6*HDRES_1000-0.43*SQRALT

#Heraklion, Greece 
34.80 + 1.55E-5*HDRES_500 + 37.71*DISTINVMAJORC1
