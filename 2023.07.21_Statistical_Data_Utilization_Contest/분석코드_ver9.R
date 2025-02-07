rm(list=ls())

setwd("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터")

total_data_2014 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2015 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2016 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2017 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2018 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2019 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2020 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")
total_data_2021 = read.csv("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")

total_data_2014[is.na(total_data_2014)] <- 0
total_data_2015[is.na(total_data_2015)] <- 0
total_data_2016[is.na(total_data_2016)] <- 0
total_data_2017[is.na(total_data_2017)] <- 0
total_data_2018[is.na(total_data_2018)] <- 0
total_data_2019[is.na(total_data_2019)] <- 0
total_data_2020[is.na(total_data_2020)] <- 0
total_data_2021[is.na(total_data_2021)] <- 0

total_data <- rbind(total_data_2014, total_data_2015, total_data_2016, total_data_2017, 
                    total_data_2018, total_data_2019, total_data_2020, total_data_2021)
total_data <- total_data[,1:27]


data_2014_name <- data.frame(total_data_2014[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2015_name <- data.frame(total_data_2015[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2016_name <- data.frame(total_data_2016[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2017_name <- data.frame(total_data_2017[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2018_name <- data.frame(total_data_2018[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2019_name <- data.frame(total_data_2019[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2020_name <- data.frame(total_data_2020[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_2021_name <- data.frame(total_data_2021[c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,
                                               5111,5476,5841,6206,6571,6936,7301,7666,8031,8396,8761,9126,9491,9856,
                                               10221,10586,10951,11316,11681,12046,12411,12776,13141,13506,13871,14236,14601,14966,
                                               15331,15696,16061,16426,16791,17156,17521,17886,18251,18616,18981,19346,19711,
                                               20076,20441,20806,21171,21536,21901,22266,22631,22996,23361,23726,24091,24456,24821,
                                               25186,25551,25916,26281,26646,27011,27376,27741,28106,28471,28836,29201,29566,29931,
                                               30296,30661,31026,31391,31756,32121,32486,32851,33216,33581,33946,34311),])

data_total_name <- rbind(data_2014_name$Region_lab,data_2015_name$Region_lab,data_2016_name$Region_lab,data_2017_name$Region_lab,
                         data_2018_name$Region_lab,data_2019_name$Region_lab,data_2020_name$Region_lab,data_2021_name$Region_lab)


install.packages("readxl")
library(readxl)

sea_surface_temp = read_excel("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\해수면온도데이터\\해수면온도데이터.xlsx")
sea_surface_temp <- data.frame(sea_surface_temp[, 1:9])
sea_surface_temp[is.na(sea_surface_temp)] <- 0


sea_surface_temp_korea_sur <- sea_surface_temp[,8]

total_data_NA_det <- total_data[-c(which(total_data$Day_preci == 0)),]
total_data_NA_det <- total_data[-c(which(total_data$Max_10m_preci == 0)),]
total_data_NA_det <- total_data[-c(which(total_data$Max_1h_preci == 0)),]
total_data_NA_det <- total_data_NA_det[,1:27]

sample_num <- sample(1:60299, 114, replace = F)

total_data_NA_det[sample_num,]

total_data <- total_data_NA_det[sample_num,]

fit <- lm(total_data$Avg_Temp ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_Temp ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_atmo_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_atmo_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Min_atmo_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_coa_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_coa_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Min_coa_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_rel_humid ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_wat_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_wat_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Min_wat_pre ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_wid_spd ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_wid_spd ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_mont_wid_spd ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Day_preci ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_1h_preci ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_10m_preci ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_new_cum_snw ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_cum_snw ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_tol_cld ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_m_flo_cld ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Max_amo_of_cld ~ sea_surface_temp_korea_sur)
summary(fit)
fit <- lm(total_data$Avg_gr_temp ~ sea_surface_temp_korea_sur)
summary(fit)

total_data <- total_data_NA_det

fit <- lm(total_data$Day_preci ~ total_data$Avg_Temp + total_data$Max_Temp + total_data$Avg_atmo_pre + total_data$Max_atmo_pre 
          + total_data$Min_coa_pre + total_data$Avg_rel_humid + total_data$Max_wat_pre + total_data$Min_wat_pre 
          + total_data$Avg_wid_spd + total_data$Max_wid_spd + total_data$Max_mont_wid_spd
          + total_data$Max_new_cum_snw + total_data$Max_cum_snw + total_data$Avg_tol_cld + total_data$Avg_m_flo_cld + total_data$Max_amo_of_cld + total_data$Avg_gr_temp)
summary(fit)

cor()


(-0.00964)*(-0.026870)+(-0.01106)*(-0.023863)+0.00306*1.619219 + 0.383374*(-0.02851) + (-0.01608)*(-0.023863) + (-0.03533)*0.046982 -0.006728388


