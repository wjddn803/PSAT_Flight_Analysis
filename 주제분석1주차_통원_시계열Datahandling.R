getwd()
setwd("C:/Users/Jungwoo Lim/Documents/2018년 과제/PSAT/통원팀/Data")


data<-read.csv("final_weather.csv",header=T)


install.packages("forecast")
library(forecast)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library("stringr")
install.packages("xts")
library("xts")
install.packages("tidyr")
library(tidyr)
install.packages("itsmr")
library("itsmr")
install.packages("Metrics")
library("Metrics")
install.packages("forecast")
library(forecast)
install.packages("TSA")
library(TSA)
install.packages("stats")
library(stats)
install.packages("bimixt")
library("bimixt")
####Data handling####

#NA 50% 이상인건 지워버리자
data$rainfall<-NULL
data$snowCover<-NULL
data$X3hrSnowCover<-NULL
data$groundStateCode<-NULL
data$domesticStateCode<-NULL
data$X<-NULL


#######################################################################################################
################################# Temperature Data Handling ###########################################
#######################################################################################################


dataf<-data
dataf<-subset(data,subset=is.na(data$temp)!=TRUE)
str(dataf)

#groupby랑 summarise쓰기 위해 key column을 만들어준다. 
#방향은 3가지로 만들었다. 

#변수들 중에 date가 10일미만이면 5, 20일미만이면 15, 31일미만이면 25로 코딩해주자!
#1일단위도 넣어보았다!
names(dataf)
temp<-data.frame(cbind(dataf$year,dataf$month,dataf$date,dataf$temp))
names(temp)<-c("year","month","date","temp")

temp$date_fix<-ifelse(temp$date<10,5,ifelse(temp$date<20,15,25))

temp$data.YMD<-paste(temp$year,temp$month,temp$date_fix,sep = "-")#연월일-일을 0,1,2로 나눈 경우
temp$data.YMD2<-paste(temp$year,temp$month,temp$date,sep = "-")#연월일인경우
temp$data.YMD3<-paste(temp$year,temp$month,sep = "-")#연월인경우


meanTemperature<-temp %>% group_by(data.YMD) %>% summarise(mean.temp=mean(temp))
meanTemperature2<-temp %>% group_by(data.YMD2) %>% summarise(mean.temp=mean(temp))
meanTemperature3<-temp %>% group_by(data.YMD3) %>% summarise(mean.temp=mean(temp))

meanTemperature<-na.omit(meanTemperature)
meanTemperature2<-na.omit(meanTemperature2)
meanTemperature3<-na.omit(meanTemperature3)



#평균데이터가 나왔으면 이제 이걸 날짜와 온도로 합치자 
#그리고 year, month, date가 factor로 되어있어서 형변환 시켜주자!

#Daily-5,15,25인경우
mean_temp<-str_split(meanTemperature$data.YMD,"-",n=3)
df<-data.frame(matrix(unlist(mean_temp),nrow=121,byrow=T))
mean_temp_final<-cbind(df,meanTemperature$mean.temp)
names(mean_temp_final)<-c("Year","Month","Date","Temp")
mean_temp_final$Year<-as.integer(as.character(mean_temp_final$Year))
mean_temp_final$Month<-as.integer(as.character(mean_temp_final$Month))
mean_temp_final$Date<-as.integer(as.character(mean_temp_final$Date))
mean_temp_final<-arrange(mean_temp_final,Year,Month,Date)

#Daily 인경우
mean_temp2<-str_split(meanTemperature2$data.YMD2,"-",n=3)
df2<- data.frame(matrix(unlist(mean_temp2),nrow=1218,byrow=T))
mean_temp_final2<-cbind(df2,meanTemperature2$mean.temp)
names(mean_temp_final2)<-c("Year","Month","Date","Temp")
mean_temp_final2$Year<-as.integer(as.character(mean_temp_final2$Year))
mean_temp_final2$Month<-as.integer(as.character(mean_temp_final2$Month))
mean_temp_final2$Date<-as.integer(as.character(mean_temp_final2$Date))
mean_temp_final2<-arrange(mean_temp_final2,Year,Month,Date)

#Monthly 인경우
mean_temp3<-str_split(meanTemperature3$data.YMD3,"-",n=2)
df3 <- data.frame(matrix(unlist(mean_temp3), nrow=41, byrow=T))
mean_temp_final3<-cbind(df3,meanTemperature3$mean.temp)
names(mean_temp_final3)<-c("Year","Month","Temp")
mean_temp_final3$Year<-as.integer(as.character(mean_temp_final3$Year))
mean_temp_final3$Month<-as.integer(as.character(mean_temp_final3$Month))
mean_temp_final3<-arrange(mean_temp_final3,Year,Month)

#######################################################################################################
################################# windSpeed Handling ##################################################
#######################################################################################################

data<-read.csv("final_data4.csv",header=T)


dataf<-subset(data,subset=is.na(data$windSpeed)!=TRUE)
str(dataf)

#groupby랑 summarise쓰기 위해 key column을 만들어준다. 
#방향은 3가지로 만들었다. 

#변수들 중에 date가 10일미만이면 5, 20일미만이면 15, 31일미만이면 25로 코딩해주자!
#1일단위도 넣어보았다!
#근데 월별 평균데이터로 봐도 풍속은 그다지 큰 차이가 나는 것 같지 않다. 시간별로도 한 번 봐야겠다.
#일단 월별 평균에다가 시간대로도 한번 봐야겠다. 

names(dataf)
windSpeed<-data.frame(cbind(dataf$year,dataf$month,dataf$date,dataf$windSpeed,dataf$time))
names(windSpeed)<-c("year","month","date","windSpeed","time")

windSpeed$date_fix<-ifelse(windSpeed$date<10,5,ifelse(windSpeed$date<20,15,25))

windSpeed$data.YMD<-paste(windSpeed$year,windSpeed$month,windSpeed$date_fix,sep = "-")#연월일-일을 0,1,2로 나눈 경우
windSpeed$data.YMD2<-paste(windSpeed$year,windSpeed$month,windSpeed$date,sep = "-")#연월일인경우
windSpeed$data.YMD3<-paste(windSpeed$year,windSpeed$month,sep = "-")#연월인경우
windSpeed$data.YMDT<-paste(windSpeed$year,windSpeed$month,windSpeed$date,windSpeed$time,sep="-")


meanwindSpeed<-windSpeed %>% group_by(data.YMD) %>% summarise(mean.windSpeed=mean(windSpeed))
meanwindSpeed2<-windSpeed %>% group_by(data.YMD2) %>% summarise(mean.windSpeed=mean(windSpeed))
meanwindSpeed3<-windSpeed %>% group_by(data.YMD3) %>% summarise(mean.windSpeed=mean(windSpeed))
meanwindSpeed4<-windSpeed %>% group_by(data.YMDT) %>% summarise(mean.windSpeed=mean(windSpeed))

meanwindSpeed<-na.omit(meanwindSpeed)
meanwindSpeed2<-na.omit(meanwindSpeed2)
meanwindSpeed3<-na.omit(meanwindSpeed3)
meanwindSpeed4<-na.omit(meanwindSpeed4)


#평균데이터가 나왔으면 이제 이걸 날짜와 온도로 합치자 
#그리고 year, month, date가 factor로 되어있어서 형변환 시켜주자!

#Daily-5,15,25인경우
mean_windSpeed<-str_split(meanwindSpeed$data.YMD,"-",n=3)
df<-data.frame(matrix(unlist(mean_windSpeed),nrow=121,byrow=T))
mean_windSpeed_final<-cbind(df,meanwindSpeed$mean.windSpeed)
names(mean_windSpeed_final)<-c("Year","Month","Date","windSpeed")
mean_windSpeed_final$Year<-as.integer(as.character(mean_windSpeed_final$Year))
mean_windSpeed_final$Month<-as.integer(as.character(mean_windSpeed_final$Month))
mean_windSpeed_final$Date<-as.integer(as.character(mean_temp_final$Date))
mean_windSpeed_final<-arrange(mean_windSpeed_final,Year,Month,Date)

#Daily 인경우
mean_windSpeed2<-str_split(meanwindSpeed2$data.YMD2,"-",n=3)
df2<- data.frame(matrix(unlist(mean_windSpeed2),nrow=1218,byrow=T))
mean_windSpeed_final2<-cbind(df2,meanwindSpeed2$mean.windSpeed)
names(mean_windSpeed_final2)<-c("Year","Month","Date","windSpeed")
mean_windSpeed_final2$Year<-as.integer(as.character(mean_windSpeed_final2$Year))
mean_windSpeed_final2$Month<-as.integer(as.character(mean_windSpeed_final2$Month))
mean_windSpeed_final2$Date<-as.integer(as.character(mean_windSpeed_final2$Date))
mean_windSpeed_final2<-arrange(mean_windSpeed_final2,Year,Month,Date)

#Monthly 인경우
mean_windSpeed3<-str_split(meanwindSpeed3$data.YMD3,"-",n=2)
df3 <- data.frame(matrix(unlist(mean_windSpeed3), nrow=41, byrow=T))
mean_windSpeed_final3<-cbind(df3,meanwindSpeed3$mean.windSpeed)
names(mean_windSpeed_final3)<-c("Year","Month","windSpeed")
mean_windSpeed_final3$Year<-as.integer(as.character(mean_windSpeed_final3$Year))
mean_windSpeed_final3$Month<-as.integer(as.character(mean_windSpeed_final3$Month))
mean_windSpeed_final3<-arrange(mean_windSpeed_final3,Year,Month)

#Timely 인경우
mean_windSpeed4<-str_split(meanwindSpeed4$data.YMDT,"-",n=4)
df4 <- data.frame(matrix(unlist(mean_windSpeed4), nrow=29208, byrow=T))
mean_windSpeed_final4<-cbind(df4,meanwindSpeed4$mean.windSpeed)
names(mean_windSpeed_final4)<-c("Year","Month","Date","Time","WindSpeed")
mean_windSpeed_final4$Year<-as.integer(as.character(mean_windSpeed_final4$Year))
mean_windSpeed_final4$Month<-as.integer(as.character(mean_windSpeed_final4$Month))
mean_windSpeed_final4$Date<-as.integer(as.character(mean_windSpeed_final4$Date))
mean_windSpeed_final4$Time<-as.integer(as.character(mean_windSpeed_final4$Time))
mean_windSpeed_final4<-arrange(mean_windSpeed_final4,Year,Month,Date,Time)













