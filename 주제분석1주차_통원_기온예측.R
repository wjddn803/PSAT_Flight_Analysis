#################################Monthly Average!&Daily Average!#############################################
####TimeSeries DataExploring####
mean_temp_final100<-ts(mean_temp_final$Temp,frequency=36,start=c(2015,1,5),end=c(2018,5,5))
mean_temp_final200<-ts(mean_temp_final2$Temp,frequency=365,start=c(2015,1,1),end=c(2018,5,2))
mean_temp_final300<-ts(mean_temp_final3$Temp,frequency=12,start=c(2015,1),end=c(2018,5))

str(mean_temp_final100)
str(mean_temp_final200)
str(mean_temp_final300)

plot(mean_temp_final100)
plot(aggregate(mean_temp_final100,FUN=mean))#mean down!
plot(aggregate(mean_temp_final100,FUN=var))#variance up!

plot(mean_temp_final200)
plot(aggregate(mean_temp_final200,FUN=mean))#mean down!
plot(aggregate(mean_temp_final200,FUN=var))#variance up

plot(mean_temp_final300)
plot(aggregate(mean_temp_final300,FUN=mean))#mean down!
plot(aggregate(mean_temp_final300,FUN=var))#variance up


#####################################정상화 과정!############################################
########Standardize Variance########
#log(mean_temp_final100)하면 NaNs produced된다고 나온다. 다른 변환방법이 없을까?
#Box-Cox를 이용하자!
#Averaging 15days
plot(mean_temp_final100)
lamda1<-BoxCox.lambda(mean_temp_final100)
Box_temp_final100<-BoxCox(mean_temp_final100, lamda1)
plot(Box_temp_final100)
plot(mean_temp_final200)
lamda2<-BoxCox.lambda(mean_temp_final200)
Box_temp_final200<-BoxCox(mean_temp_final200, lamda2)
plot(Box_temp_final200)

plot(mean_temp_final300)
lamda3<-BoxCox.lambda(mean_temp_final300)
Box_temp_final300<-BoxCox(mean_temp_final300, lamda3)
plot(Box_temp_final300)

####주기설정####

lag.plot(Box_temp_final100,set=c(1:100),pch=".",main=Box_temp_final100,diag.col = "red",do.lines = T)#주기 36
lag.plot(Box_temp_final300,set=c(1:41),pch=".",main=Box_temp_final300,diag.col = "red",do.lines = T)#주기12

########추세차분을 해보자########
diff_Box_temp_final100<-diff(Box_temp_final100)
diff_Box_temp_final200<-diff(Box_temp_final200)
diff_Box_temp_final300<-diff(Box_temp_final300)

plot(diff_Box_temp_final100)
plot(diff_Box_temp_final200)
plot(diff_Box_temp_final300)

########계절성을 제거해보자########
diffseas_Box_temp_final100<-diff(diff_Box_temp_final100,lag=36)
diffseas_Box_temp_final200<-diff(diff_Box_temp_final200,lag=365)
diffseas_Box_temp_final300<-diff(diff_Box_temp_final300,lag=12)

plot(diffseas_Box_temp_final100,main="Both differencing_final100", ylab="Temperature",xlab="Time")
plot(diffseas_Box_temp_final200,main="Both differencing_final200", ylab="Temperature",xlab="Time")
plot(diffseas_Box_temp_final300,main="Both differencing_final300", ylab="Temperature",xlab="Time")

########Classical Decomposition으로 분해해보자########

####추세추출(MA filter)####
trend.temp100<-smooth.ma(mean_temp_final100, q=18)
trend.temp200<-smooth.ma(mean_temp_final200, q=182)
trend.temp300<-smooth.ma(mean_temp_final300, q=6)

plot(trend.temp100, type = "l", main = "Trend of temp100")
plot(trend.temp200, type = "l", main = "Trend of temp200")
plot(trend.temp300, type = "l", main = "Trend of temp300")

####계절성 추출(Seasonal smoothing)####
season.temp100<-season(mean_temp_final100,d=36)
season.temp200<-season(mean_temp_final200,d=365)
season.temp300<-season(mean_temp_final300,d=12)

plot(season.temp100, type = "l", main = "Seasonality of temp100")
plot(season.temp200, type = "l", main = "Seasonality of temp200")
plot(season.temp300, type = "l", main = "Seasonality of temp300")

####추세 제거####
fit100.1<- mean_temp_final100-trend.temp100
fit200.1<- mean_temp_final200-trend.temp200
fit300.1<- mean_temp_final300-trend.temp300

plot(fit100.1)
plot(fit200.1)
plot(fit300.1)

####계절성 제거####
fit100.2<-fit100.1-season.temp100
fit200.2<-fit200.1-season.temp200
fit300.2<-fit300.1-season.temp300

plot(fit100.2)
plot(fit200.2)
plot(fit300.2)

####한꺼번에 해주는 과정!####
decom_mean_temp_final100<-decompose(mean_temp_final100)
plot(decom_mean_temp_final100)
decom_mean_temp_final200<-decompose(mean_temp_final200)
plot(decom_mean_temp_final200)
decom_mean_temp_final300<-decompose(mean_temp_final300)
plot(decom_mean_temp_final300)

decom_Boxcox_temp_final100<-decompose(Box_temp_final100)
plot(decom_Boxcox_temp_final100)
decom_Boxcox_temp_final200<-decompose(Box_temp_final200)
plot(decom_Boxcox_temp_final200)
decom_Boxcox_temp_final300<-decompose(Box_temp_final300)
plot(decom_Boxcox_temp_final300)

#########잔차가 정상성 만족하는지 확인해보자~########
test(mean_temp_final100)
test(diffseas_Box_temp_final100)#얘가 제일 IID를 기각하지 못하므로 정상성을 만족하는 잔차!
test(fit100.2) 

test(mean_temp_final200)
test(diffseas_Box_temp_final200)
test(fit200.2) 
#그냥 전체적으로 다 정상성 만족못하고 잔차가 correlated되어있다. 

test(mean_temp_final300)
test(diffseas_Box_temp_final300)#얘가 제일 IID를 기각하지 못하므정 정상성을 만족하는 잔차!
test(fit300.2) 


####전체적으로 봤을 때 정상성이 월별, 10일 평균별 은 정상성이 잘 제거가 되는데 일별은 잘 제거되지 않는다. 
####일별 데이터의 잔차가 correlated되어 있으면 어떻게 해야할까?
####정상성을 만족하는 모델에 데이터를 적합하여 잔차를 정상화해보자!

auto.arima(Box_temp_final200)#응 AIC 7000이야.... 안되게ㅔㅆ다....
#일별은 포기하고 15일별, 월별을 예측하자
#우리가 아까 잔차가 정상성을 확인해서 ACF, PADF로 봐서 판단해도 되나 귀찮으니까 auto.arima를 쓰자


#Daily_15days
#분산안정화 시킨거
plot(Box_temp_final100)
test(Box_temp_final100) #ACF보니까 계절성, ACF값의 지수적으로 감소하지 않으므로 차분의 필요성 확인

auto.arima(Box_temp_final100)#ARIMA(1,0,1)(1,1,0)[36] with drift #AIC:703.22
lr<-Box_temp_final100
lr101.110<-Arima(lr, order=c(1,0,1),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(lr101.110)#AIC:701.2175
lr201.110<-Arima(lr, order=c(2,0,1),seasonal = list(order=c(1,1,0), period=36),method="ML") ;AIC(lr201.110)#AIC:703.1129
lr101.111<-Arima(lr, order=c(1,0,1),seasonal = list(order=c(1,1,1), period=36),method="ML") ;AIC(lr101.111)#AIC:703.1129
lr110.110<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML") ;AIC(lr101.111)#AIC:702.7922

test(lr101.110$residuals)#IID 나름?만족으로 나온다!!!!!!
flr101.110<-forecast::forecast(lr101.110, h=36)
plot(flr101.110)


#분산 안정화 안시킨거
plot(mean_temp_final100)
test(mean_temp_final100)
auto.arima(mean_temp_final100)#ARIMA(2,1,0)(0,1,1)[36] #AIC:337.84

lr<-mean_temp_final100

lr200.011<-Arima(lr, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr200.011)#AIC:700.8021
lr210.011<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr210.011)#AIC:707.958
lr210.001<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(lr210.001)#AIC:1079.579
lr210.111<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(lr210.111)#AIC:709.9437
lr210.110<-Arima(lr, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(lr210.110)#AIC:708.2355
lr110.011<-Arima(lr, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(lr110.011)#AIC:711.436

test(lr200.011$residuals)#iid나름만족!


flr200.011<-forecast::forecast(lr200.011, h=72)
plot(flr200.011)
flr200.011
daily15<-data.frame(flr200.110)
#모델선택과정은 밑으로 가세요





#Monthly
#분산안정화 시킨거
plot(Box_temp_final300)
test(Box_temp_final300)
auto.arima(Box_temp_final300)#ARIMA(0,0,0)(1,1,0)[12] AIC:238.33
lr2<-Box_temp_final300
lr000.110<-Arima(lr2, order=c(0,0,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr000.110)#AIC:238.3309
lr010.100<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(1,0,0), period=12),method="ML");AIC(lr010.100)#AIC:365.4827
lr010.110<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:252.6055
lr001.110<-Arima(lr2, order=c(0,0,1),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr001.110)#AIC:239.5652
lr100.110<-Arima(lr2, order=c(1,0,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr100.110)#AIC:239.397
lr010.010<-Arima(lr2, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:255.0397

flr000.110<-forecast::forecast(lr000.110,h=25)
plot(flr100.110)
flr100.110
monthly<-boxcox.inv(data.frame(flr000.110),lamda3)

#분산 안정화 안시킨거
plot(mean_temp_final300)
test(mean_temp_final300)
auto.arima(mean_temp_final300)#ARIMA(0,0,0)(0,1,0)[12], AIC:98.46
lr3<-mean_temp_final300
lr000.010<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr000.010)#AIC:98.45572
lr010.010<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr010.010)#AIC:105.1143
lr110.010<-Arima(lr3, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(lr110.010)#AIC:104.5494
lr010.110<-Arima(lr3, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(lr010.110)#AIC:106.0366
lr110.000<-Arima(lr3, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(lr110.000)#AIC:99.2997
flr000.010<-forecast::forecast(lr000.010,h=25)
plot(flr000.010)
monthly<-data.frame(flr000.010)

write.csv(monthly,file="C:/Users/Jungwoo Lim/Desktop/monthly_temp.csv")
write.csv(daily15,file="C:/Users/Jungwoo Lim/Desktop/daily15_temp.csv")






##############################################################################################
################# train data vs test data >> rmse 비교 >> 최적합의 모델 선택 #################
##############################################################################################
#daily15 model 선택
plot(mean_temp_final100)
lr<-mean_temp_final100
train<-(lr[1:88])
test<-(lr[89:113])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)

#auto.arima(lr)의 결과로 나온 모델(ARIMA(2,1,0)(0,1,1)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.200.011<-Arima(ts.train, order=c(2,0,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.200.011)#AIC:700.8021
train.210.011<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.210.011)#AIC:707.958
train.210.001<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(0,0,1), period=36),method="ML");AIC(train.210.001)#AIC:1079.579
train.210.111<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,1), period=36),method="ML");AIC(train.210.111)#AIC:709.9437
train.210.110<-Arima(ts.train, order=c(2,1,0),seasonal = list(order=c(1,1,0), period=36),method="ML");AIC(train.210.110)#AIC:708.2355
train.110.011<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,1), period=36),method="ML");AIC(train.110.011)#AIC:711.436


f.train.200.011<-forecast::forecast(train.200.011, h=72) #train data로 만든 모델로 예측값
f.train.210.011<-forecast::forecast(train.210.011, h=72) #train data로 만든 모델로 예측값
f.train.210.001<-forecast::forecast(train.210.001, h=72) #train data로 만든 모델로 예측값
f.train.210.110<-forecast::forecast(train.210.110, h=72) #train data로 만든 모델로 예측값
f.train.110.011<-forecast::forecast(train.110.011, h=72) #train data로 만든 모델로 예측값


AIC.train<-c(AIC(train.200.011),AIC(train.210.011),AIC(train.210.001),AIC(train.210.110),AIC(train.110.011))
AIC.train #train.200.011 >> best model

#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정
rmse(f.train.200.011$mean, test) #15.96727
rmse(f.train.210.011$mean, test) #15.95714
rmse(f.train.210.001$mean, test) #14.45432 >> best model
rmse(f.train.210.110$mean, test) #15.96111
rmse(f.train.110.011$mean, test) #16.00199 

plot(f.train.412.012) #AIC와 rmse를 모두 비교해본 결과 train.200.011 모델이 최적합 모델이었다.




#monthly 모델선택
plot(mean_temp_final300)
lr<-mean_temp_final300
train<-(lr[1:28])
test<-(lr[29:40])
ts.train<-ts(train)
ts.test<-ts(test)
auto.arima(lr)
#auto.arima(lr)의 결과로 나온 모델(ARIMA(2,1,0)(0,1,1)[36])을 기준으로 후보군 모델들을 설정하고, 트레이닝 데이터 이용해 SARIMA modeling.

train.000.010<-Arima(ts.train, order=c(0,0,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.000.010)#AIC:49.6849
train.010.010<-Arima(ts.train, order=c(0,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.010.010)#AIC:52.37072
train.110.010<-Arima(ts.train, order=c(1,1,0),seasonal = list(order=c(0,1,0), period=12),method="ML");AIC(train.110.010)#AIC:54.12538
train.010.110<-Arima(ts.train, order=c(0,1,0),seasonal = list(order=c(1,1,0), period=12),method="ML");AIC(train.010.110)#AIC:52.18324
train.110.000<-Arima(ts.train, order=c(0,0,0),seasonal = list(order=c(1,1,1), period=12),method="ML");AIC(train.110.000)#AIC:50.88942


f.train.000.010<-forecast::forecast(train.000.010, h=24) #train data로 만든 모델로 예측값
f.train.010.010<-forecast::forecast(train.010.010, h=24) #train data로 만든 모델로 예측값
f.train.110.010<-forecast::forecast(train.110.010, h=24) #train data로 만든 모델로 예측값
f.train.010.110<-forecast::forecast(train.010.110, h=24) #train data로 만든 모델로 예측값
f.train.110.000<-forecast::forecast(train.110.000, h=24) #train data로 만든 모델로 예측값


AIC.train<-c(AIC(train.000.010),AIC(train.010.010),AIC(train.110.010),AIC(train.010.110),AIC(train.110.000))
AIC.train #train.000.010 >> best model

#test data로 미리 빼놓았던 실제 데이터와 train data로 만들었던 여러 후보군 모델들로 도출해낸 예측값을 이용해 RMSE 측정
rmse(f.train.000.010$mean, test) #1.529104
rmse(f.train.010.010$mean, test) #1.415202
rmse(f.train.110.010$mean, test) #1.387894 >> best model
rmse(f.train.010.110$mean, test) #1.785915
rmse(f.train.110.000$mean, test) #1.561379 

plot(f.train.000.010) #AIC와 rmse를 모두 비교해본 결과 train.000.010 모델이 최적합 모델이었다.



