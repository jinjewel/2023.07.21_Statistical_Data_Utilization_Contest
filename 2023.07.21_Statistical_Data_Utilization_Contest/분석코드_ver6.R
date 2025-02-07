rm(list=ls())
##################################################################################
install.packages("fpp")
library(fpp)
install.packages("readxl")
library(readxl)
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("stats")
library(stats)
install.packages("lmtest")
library(lmtest)
##################################################################################
## 전체 데이터
sea_surface_temp = read_excel("C:\\Users\\User\\Desktop\\3인 공모전\\데이터\\해수면온도데이터\\해수면온도데이터.xlsx")
## 데이터프레임으로 변경
sea_surface_temp <- data.frame(sea_surface_temp[, 1:9])
## 결측치 0으로 대체
sea_surface_temp[is.na(sea_surface_temp)] <- 0

##################################################################################

### 엘리뇨 관측 구역에서의 데이터
ellinyo_Watch_Zone_temp <- sea_surface_temp[,c(1,4)]
ellinyo_Watch_Zone_Devi_temp <- sea_surface_temp[,c(1,5)]
## 엘리뇨 관측구역에서의 그래프
x11()
par(mfrow=c(2, 1))
plot(ellinyo_Watch_Zone_temp$number, ellinyo_Watch_Zone_temp$엘니뇨.감시구역_온도, 
     type="l", ylab="엘니뇨 감시구역 온도", main="엘니뇨 감시구역 온도")
abline(v=c(6,7,8,18,19,20,30,31,32,42,43,44,54,55,56,66,67,68,78,79,80,90,91,92,102,103,104,114,115,116),col="red")
abline(v=c(1,2,12,13,14,24,25,26,36,37,38,48,49,50,60,61,62,72,73,74,84,85,86,96,97,98,108,109,110),col="blue")
abline(h=c(0.5))
plot(ellinyo_Watch_Zone_Devi_temp$number, ellinyo_Watch_Zone_Devi_temp$엘니뇨.감시구역_편차, 
     type="l", ylab="엘니뇨 감시구역 편차", main="엘니뇨 감시구역 편차")
abline(v=c(6,7,8,18,19,20,30,31,32,42,43,44,54,55,56,66,67,68,78,79,80,90,91,92,102,103,104,114,115,116),col="red")
abline(v=c(1,2,12,13,14,24,25,26,36,37,38,48,49,50,60,61,62,72,73,74,84,85,86,96,97,98,108,109,110),col="blue")
abline(h=c(0.5))

### 우리나라 관측 구역에서의 데이터
korea_Watch_Zone_temp <- sea_surface_temp[,c(1,8)]
korea_Watch_Zone_Devi_temp <- sea_surface_temp[,c(1,9)]
## 우리나라 관측 구역에서의 그래프
x11()
par(mfrow=c(2, 1))
plot(korea_Watch_Zone_temp$number, korea_Watch_Zone_temp$우리나라.주변.해수면_온도, 
     type="l", ylab="우리나라 주변 해수온 온도", main="우리나라 주변 해수온 온도")
abline(v=c(6,7,8,18,19,20,30,31,32,42,43,44,54,55,56,66,67,68,78,79,80,90,91,92,102,103,104,114,115,116),col="red")
abline(v=c(1,2,12,13,14,24,25,26,36,37,38,48,49,50,60,61,62,72,73,74,84,85,86,96,97,98,108,109,110),col="blue")
abline(h=c(0.5))
plot(korea_Watch_Zone_Devi_temp$number, korea_Watch_Zone_Devi_temp$우리나라.주변.해수면_편차, 
     type="l", ylab="우리나라 주변 해수온 편차", main="우리나라 주변 해수온 편차")
abline(v=c(6,7,8,18,19,20,30,31,32,42,43,44,54,55,56,66,67,68,78,79,80,90,91,92,102,103,104,114,115,116),col="red")
abline(v=c(1,2,12,13,14,24,25,26,36,37,38,48,49,50,60,61,62,72,73,74,84,85,86,96,97,98,108,109,110),col="blue")
abline(h=c(0.5))

##################################################################################
### 분석방향
# 14,15,16,18,19,23 -> 엘리뇨

##################################################################################

### 전 년도로 수온 변화 예측
plot(ellinyo_Watch_Zone_temp$number, ellinyo_Watch_Zone_temp$엘니뇨.감시구역_온도, 
     type="l", ylab="엘니뇨 감시구역 온도", main="엘니뇨 감시구역 온도")

### 분석시작
## 데이터 설정 
x <- ts(ellinyo_Watch_Zone_temp$엘니뇨.감시구역_온도)
x11()
tsdisplay(x)
## 로그변환 설정
lx <- log(x)
x11()
tsdisplay(lx)
## 차분
x11()
tsdisplay(diff(diff(lx,12)))
x11()
tsdisplay(diff(lx))
x11()
tsdisplay(diff(lx,12))
## 자동으로 계산한 계절성 아리마
auto.arima(lx)
## 수동으로 계산한 계절성 아리마 
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,0),periods=12))

arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,0),periods=12))

arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,3),periods=12))

arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,1),periods=12))

arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,3),periods=12))

arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,2),periods=12))

arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,3),periods=12))

arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,0),periods=12))

arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,3),periods=12))

## 계절성 아리마 결과
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,3),periods=12))

## 비 계절성 아리마 결과
auto.arima(lx)

### 검진
# 비 계절성 아리마
fit1 <- auto.arima(lx)
summary(fit1)
## p,d,q가 0,1,0 이므로 따로 계수는 없다.
coeftest(fit1)
confint(fit1)
## 모형에 대한 검진
x11()
tsdiag(fit1, gof.lag=24) 
# ACF가 1이상의 값에서 큰값이 하나 존재하고, 
# 수정된 포트맨토 검정에 대한 유의 확률도 0.05보다 큰값이 존재하므로 유의미하지 않음


# 계절성 아리마
fit2 <- arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,3),periods=12))
summary(fit2)
## p,d,q가 (1,1,0)*(0,1,3)12 이다.
coeftest(fit2)
confint(fit2)
## 모형에 대한 검진
x11()
tsdiag(fit2, gof.lag=24)
# ACF가 1이상의 값에서 쿤값이 하나 존재하나 무시가능하고, 
# 수정된 포트맨토 검정에 대한 유의 확률도 0.05보다 작으므로 귀무가설을 채택하여 잔차의 자기상관성이 없다고 할 수 있다.
# 따라서 AIC가 커도 계절성 아리마를 채택

### 예측
fc <- forecast(fit2, h=30, level=0.95)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x) # 로그변환 한 것을 다시 원데이터로 변환
summary(fc)
x11()
plot(fc, main="25년까지 예측한 모형")
##################################################################################
### 온난화로 인한 갑자기 수온이 상승한 년도부터 수온 변화 예측
## 데이터 설정 
x <- ts(ellinyo_Watch_Zone_temp$엘니뇨.감시구역_온도[85:114])
x11()
tsdisplay(x)
## 로그변환 설정
lx <- log(x)
x11()
tsdisplay(lx)
## 차분
x11()
tsdisplay(diff(diff(lx,12)))
x11()
tsdisplay(diff(lx))
x11()
tsdisplay(diff(lx,12))
## 자동으로 계산한 계절성 아리마
auto.arima(lx)
## 수동으로 계산한 계절성 아리마 
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,3),periods=12))

## 계절성 아리마 결과
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,0),periods=12))
## 비 계절성 아리마 결과
auto.arima(lx)


### 검진
# 비 계절성 아리마
fit1 <- auto.arima(lx)
summary(fit1)
## p,d,q가 0,1,0 이므로 따로 계수는 없다.
coeftest(fit1)
confint(fit1)
## 모형에 대한 검진
x11()
tsdiag(fit1, gof.lag=24) 
# ACF가 1이상의 값에서 유의미하게 작고, 
# 수정된 포트맨토 검정에 대한 유의 확률도 0.05보다 작으므로 귀무가설을 채택하여 잔차의 자기상관성이 없다고 할 수 있다.
# 따라서 AIC가 커도 계절성 아리마를 채택

# 계절성 아리마
fit2 <- arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,0),periods=12))
summary(fit2)
## p,d,q가 (1,1,0)*(0,1,3)12 이다.
coeftest(fit2)
confint(fit2)
## 모형에 대한 검진
x11()
tsdiag(fit2, gof.lag=24)
# ACF가 1이상의 값에서 유의미하게 작고, 
# 수정된 포트맨토 검정에 대한 유의 확률도 0.05보다 작으므로 귀무가설을 채택하여 잔차의 자기상관성이 없다고 할 수 있다.
# 따라서 AIC가 커도 계절성 아리마를 채택


### 예측
### 비계절성
fc <- forecast(fit1, h=30, level=0.95)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x) # 로그변환 한 것을 다시 원데이터로 변환
summary(fc)
x11()
plot(fc, main="25년까지 비계절으로 예측한 모형")

### 계절성 
fc <- forecast(fit2, h=30, level=0.95)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x) # 로그변환 한 것을 다시 원데이터로 변환
summary(fc)
x11()
plot(fc, main="25년까지 계절으로 예측한 모형")

# 결과적으로 전체 데이터에서도 계절성이 존재한다고 확인했으므로 
# 계절성 아리마를 채택, 앞으로 해수면 온도는 계속 증가한다고 예측할 수 있다.
### 23년 7월부터 25년 12월까지 예측한 엘리뇨 관측 구역에서의 온도
fc$mean 


#############################################################

### 우리나라 주변 수온 예측

### 온난화로 인한 갑자기 수온이 상승한 년도부터 수온 변화 예측
## 데이터 설정 
x <- ts(korea_Watch_Zone_temp$우리나라.주변.해수면_온도)
x11()
tsdisplay(x)
## 로그변환 설정
lx <- log(x)
x11()
tsdisplay(lx)
## 차분
x11()
tsdisplay(diff(diff(lx,12)))
x11()
tsdisplay(diff(lx))
x11()
tsdisplay(diff(lx,12))
## 자동으로 계산한 계절성 아리마
auto.arima(lx)
## 수동으로 계산한 계절성 아리마 
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,0),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,0),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,3),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,1),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,2),periods=12))
arima(lx, order=c(1,1,2),seasonal = list(order=c(0,1,3),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,0),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,0),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,3),periods=12))
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,3),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,1),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,2),periods=12))
arima(lx, order=c(0,1,2),seasonal = list(order=c(1,1,3),periods=12))

## 계절성 아리마 결과 (최근 년도 데이터를 사용한 결과)
arima(lx, order=c(0,1,1),seasonal = list(order=c(0,1,1),periods=12)) # 시그마 추정값 : 0.005575
arima(lx, order=c(0,1,1),seasonal = list(order=c(1,1,0),periods=12)) # 시그마 추정값 : 0.006142
## 비 계절성 아리마 결과( 최근 년도 데이터를 사용한 결과)
auto.arima(lx)


## 계절성 아리마 결과(전체 데이터)
arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
## 비 계절성 아리마 결과( 최근 년도 데이터를 사용한 결과)
auto.arima(lx)



### 검진
# 비 계절성 아리마
fit1 <- auto.arima(lx)
summary(fit1)
## p,d,q가 0,1,0 이므로 따로 계수는 없다.
coeftest(fit1)
confint(fit1)
## 모형에 대한 검진
x11()
tsdiag(fit1, gof.lag=24) 
# 사용 불가

# 계절성 아리마
fit2 <- arima(lx, order=c(1,1,1),seasonal = list(order=c(0,1,1),periods=12))
summary(fit2)
## p,d,q가 (1,1,0)*(0,1,3)12 이다.
coeftest(fit2)
confint(fit2)
## 모형에 대한 검진
x11()
tsdiag(fit2, gof.lag=24)
# ACF가 1이상의 값에서 유의미하게 작고, 
# 수정된 포트맨토 검정에 대한 유의 확률도 0.05보다 작으므로 귀무가설을 채택하여 잔차의 자기상관성이 없다고 할 수 있다.
# 따라서 AIC가 커도 계절성 아리마를 채택


### 예측
### 비계절성
fc <- forecast(fit1, h=30, level=0.95)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x) # 로그변환 한 것을 다시 원데이터로 변환
summary(fc)
x11()
plot(fc, main="25년까지 비계절으로 예측한 모형")

### 계절성 
fc <- forecast(fit2, h=30, level=0.95)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x) # 로그변환 한 것을 다시 원데이터로 변환
summary(fc)
x11()
plot(fc, main="25년까지 계절으로 예측한 모형")

# 결과적으로 전체 데이터에서도 계절성이 존재한다고 확인했으므로 
# 계절성 아리마를 채택, 앞으로 해수면 온도는 계속 증가한다고 예측할 수 있다.
### 23년 7월부터 25년 12월까지 예측한 우리나라 관측 구역에서의 온도
fc$mean 

