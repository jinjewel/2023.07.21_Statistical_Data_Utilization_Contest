rm(list=ls())
setwd("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터")
total_data_2014 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2014_total_data.csv", fileEncoding = "euc-kr")
total_data_2015 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2015_total_data.csv", fileEncoding = "euc-kr")
total_data_2016 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2016_total_data.csv", fileEncoding = "euc-kr")
total_data_2017 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2017_total_data.csv", fileEncoding = "euc-kr")
total_data_2018 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2018_total_data.csv", fileEncoding = "euc-kr")
total_data_2019 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2019_total_data.csv", fileEncoding = "euc-kr")
total_data_2020 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2020_total_data.csv", fileEncoding = "euc-kr")
total_data_2021 = read.csv("C:/Users/kcomwel/Desktop/공모전/통계청 통계활용대회/기상관측데이터/2021_total_data.csv", fileEncoding = "euc-kr")
total_data = rbind(total_data_2014, total_data_2015, total_data_2016, total_data_2017, total_data_2018, total_data_2019, total_data_2020, total_data_2021)[, 4:28]
total_data[is.na(total_data)]<-0
total_data_pca = prcomp(total_data)
total_data_pca
plot(total_data_pca)
summary(total_data_pca)
lm_total_data = lm(occur ~ Avg_Temp+Max_Temp+Avg_atmo_pre+Max_atmo_pre+Min_atmo_pre+Avg_coa_pre+Max_coa_pre+Min_coa_pre+Avg_rel_humid+Avg_wat_pre+Max_wat_pre+Min_wat_pre+Avg_wid_spd+Max_wid_spd+Max_mont_wid_spd+Day_preci+Max_10m_preci+Max_1h_preci+Max_new_cum_snw+Max_cum_snw+Avg_tol_cld+Avg_m_flo_cld+Max_amo_of_cld+Avg_gr_temp, data = total_data)
summary(lm_total_data)
#VIF
library(regclass)
VIF(lm_total_data)
#Avg_Temp, Max_Temp, Avg_atmo_pre, Avg_rel_humid의 VIF 값이 10 이상이므로 다중공선성이다.
#상관계수
eig = eigen(cor(total_data))
#상태수
sqrt(max(eig$values)/eig$values)
#상태지수
max(sqrt(max(eig$values)/eig$values))
#상태지수가 15가 넘으므로 공선성이 존재한다.
#누적비율 95 PC5까지 공선성을 확인?
print(total_data_pca)
plot(total_data_pca, type="l")