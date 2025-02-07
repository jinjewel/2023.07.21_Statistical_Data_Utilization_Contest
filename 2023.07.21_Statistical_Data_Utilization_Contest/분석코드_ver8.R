rm(list=ls())

setwd("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터")

total_data_2014 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2014_total_data.csv", fileEncoding = "euc-kr")
total_data_2015 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2015_total_data.csv", fileEncoding = "euc-kr")
total_data_2016 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2016_total_data.csv", fileEncoding = "euc-kr")
total_data_2017 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2017_total_data.csv", fileEncoding = "euc-kr")
total_data_2018 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2018_total_data.csv", fileEncoding = "euc-kr")
total_data_2019 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2019_total_data.csv", fileEncoding = "euc-kr")
total_data_2020 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2020_total_data.csv", fileEncoding = "euc-kr")
total_data_2021 = read.csv("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\기상관측데이터\\2021_total_data.csv", fileEncoding = "euc-kr")

total_data <- rbind(total_data_2014, total_data_2015, total_data_2016, total_data_2017, total_data_2018, total_data_2019, total_data_2020, total_data_2021)[, 4:28]
total_data[is.na(total_data)] <- 0

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
lm_total_data_Day_preci = lm(Day_preci ~ Avg_Temp+Max_Temp+Avg_atmo_pre+Max_atmo_pre+Min_atmo_pre+Avg_coa_pre+Max_coa_pre+Min_coa_pre+Avg_rel_humid+Avg_wat_pre+Max_wat_pre+Min_wat_pre+Avg_wid_spd+Max_wid_spd+Max_mont_wid_spd+Max_new_cum_snw+Max_cum_snw+Avg_tol_cld+Avg_m_flo_cld+Max_amo_of_cld+Avg_gr_temp, data = total_data)
lm_total_data_Max_10m_preci = lm(Max_10m_preci ~ Avg_Temp+Max_Temp+Avg_atmo_pre+Max_atmo_pre+Max_coa_pre+Min_coa_pre+Avg_rel_humid+Max_wat_pre+Min_wat_pre+Avg_wid_spd+Max_wid_spd+Max_mont_wid_spd+Max_new_cum_snw+Max_cum_snw+Avg_tol_cld+Avg_m_flo_cld+Max_amo_of_cld+Avg_gr_temp, data = total_data)
lm_total_data_Max_1h_preci = lm(Max_1h_preci ~ Avg_Temp+Max_Temp+Avg_atmo_pre+Max_atmo_pre+Max_coa_pre+Min_coa_pre+Avg_rel_humid+Max_wat_pre+Min_wat_pre+Avg_wid_spd+Max_wid_spd+Max_mont_wid_spd+Max_new_cum_snw+Max_cum_snw+Avg_tol_cld+Avg_m_flo_cld+Max_amo_of_cld+Avg_gr_temp, data = total_data)


#후진적 제거 방법 
Back_lm_total_data_Day_preci <- step(lm_total_data_Day_preci, direction = "backward")
summary(Back_lm_total_data_Day_preci)
Back_lm_total_data_Max_10m_preci <- step(lm_total_data_Max_10m_preci, direction = "backward")
summary(Back_lm_total_data_Max_10m_preci)
Back_lm_total_data_Max_1h_preci <- step(lm_total_data_Max_1h_preci, direction = "backward")
summary(Back_lm_total_data_Max_1h_preci)
data_1 <- total_data[-7]
data_2 <- data_1[-9]
data_3 <- data_2[-5]
data_4 <- data_3[-5]
data_5 <- data_4[-21]
data <- data_5[-12:-14]
zero <- rep(0, 275338)
a <- cbind(data, zero)

#해수면 온도
library(readxl)
ocean_temp = read_excel("C:\\Users\\USER\\OneDrive\\바탕 화면\\3인 공모전\\데이터\\해수면온도데이터\\해수면온도 데이터.xlsx")
ocean_temp_kr <- ocean_temp$`우리나라 주변 해수면_온도`
#total_data 표본추출
sample_data <- data[sample(nrow(data), 96), ]
#상관계수행렬
cor(ocean_temp_kr, sample_data)
#작은순으로 정렬
sort(abs(cor(ocean_temp_kr, sample_data)))
total_data_2014[is.na(total_data_2014)] <- 0
total_data_2015[is.na(total_data_2015)] <- 0
total_data_2016[is.na(total_data_2016)] <- 0
total_data_2017[is.na(total_data_2017)] <- 0
total_data_2018[is.na(total_data_2018)] <- 0
total_data_2019[is.na(total_data_2019)] <- 0
total_data_2020[is.na(total_data_2020)] <- 0
total_data_2021[is.na(total_data_2021)] <- 0

#연도별 강수량 합
rain_2014 <- sum(total_data_2014$Day_preci)
rain_2015 <- sum(total_data_2015$Day_preci)
rain_2016 <- sum(total_data_2016$Day_preci)
rain_2017 <- sum(total_data_2017$Day_preci)
rain_2018 <- sum(total_data_2018$Max_10m_preci)
rain_2019 <- sum(total_data_2019$Day_preci)
rain_2020 <- sum(total_data_2020$Day_preci)
rain_2021 <- sum(total_data_2021$Day_preci)

#연도별 수온 평균
ocean_2014 <- subset(ocean_temp, Year == "2014")
ocean_2015 <- subset(ocean_temp, Year == "2015")
ocean_2016 <- subset(ocean_temp, Year == "2016")
ocean_2017 <- subset(ocean_temp, Year == "2017")
ocean_2018 <- subset(ocean_temp, Year == "2018")
ocean_2019 <- subset(ocean_temp, Year == "2019")
ocean_2020 <- subset(ocean_temp, Year == "2020")
ocean_2021 <- subset(ocean_temp, Year == "2021")

#연도별 우리나라 주변 해수면_온도 평균
gotnaus_temp_2014 <- mean(ocean_2014$`우리나라 주변 해수면_온도`)
gotnaus_temp_2015 <- mean(ocean_2015$`우리나라 주변 해수면_온도`)
gotnaus_temp_2016 <- mean(ocean_2016$`우리나라 주변 해수면_온도`)
gotnaus_temp_2017 <- mean(ocean_2017$`우리나라 주변 해수면_온도`)
gotnaus_temp_2018 <- mean(ocean_2018$`우리나라 주변 해수면_온도`)
gotnaus_temp_2019 <- mean(ocean_2019$`우리나라 주변 해수면_온도`)
gotnaus_temp_2020 <- mean(ocean_2020$`우리나라 주변 해수면_온도`)
gotnaus_temp_2021 <- mean(ocean_2021$`우리나라 주변 해수면_온도`)

#연도별 우리나라 주변 해수면_편차 평균
gotnaus_temp_dev_2014 <- mean(ocean_2014$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2015 <- mean(ocean_2015$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2016 <- mean(ocean_2016$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2017 <- mean(ocean_2017$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2018 <- mean(ocean_2018$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2019 <- mean(ocean_2019$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2020 <- mean(ocean_2020$`우리나라 주변 해수면_편차`)
gotnaus_temp_dev_2021 <- mean(ocean_2021$`우리나라 주변 해수면_편차`)

#25년도까지 예측 해수면 온도 데이터
setwd("C:\\Users\\USER\\OneDrive\\바탕 화면")
library(readxl)
predict_ocean <- read_excel("C:\\Users\\USER\\OneDrive\\바탕 화면\\predict_ocean.xlsx")
predict_ocean_2023 <- subset(predict_ocean, Year == "2023")
predict_ocean_2024 <- subset(predict_ocean, Year == "2024")
predict_ocean_2025 <- subset(predict_ocean, Year == "2025")
predict_ocean_2023_temp <- mean(predict_ocean_2023$`우리나라 주변 해수면_온도`)
predict_ocean_2024_temp <- mean(predict_ocean_2024$`우리나라 주변 해수면_온도`)
predict_ocean_2025_temp <- mean(predict_ocean_2025$`우리나라 주변 해수면_온도`)




