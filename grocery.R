rm(list= ls())
setwd("")
set.seed(111)
transactions_data= read.csv("./transactions.csv",header = T,sep = ",")
head(transactions_data)
str(transactions_data)
transactions_data$date= as.Date(transactions_data$date,format="%d-%m-%Y")
install.packages('sqldf')
library(sqldf)
trans_grp= sqldf('select date,sum(transactions) as tot_trans from transactions_data group by Date')
str(trans_grp) 

head(trans_grp) 

plot(trans_grp,type="l",lwd=2,col="blue",xlab="date",ylab="tot_trans", main="Time series plot") 

transactions= ts(trans_grp$tot_trans,frequency = 352)
library(forecast)
Arima= auto.arima(transactions[1:1200])
summary(Arima)

trans_grp$scaledsales= scale(trans_grp$tot_trans)

wkday= weekdays(trans_grp$date)
trans_grp$wkday= weekdays(as.Date(trans_grp$date))
trans_grp$wkday= "weekday"
trans_grp$wkday[wkday=="Saturday"|wkday=="Sunday"]= "weekend"
library(dplyr)
weeksummary= trans_grp %>% group_by(wkday) %>% summarise(avetrans= mean(scaledsales))

str(trans_grp$wkday) 
summary(trans_grp$wkday)

trans= ts(trans_grp$tot_trans,frequency = 7)
Arima= auto.arima(trans[1:1200])
summary(Arima)

library(lubridate)
trans_grp$month= "month"
trans_grp$month= month(as.POSIXlt(trans_grp$date))

monthsummary= trans_grp %>% group_by(month) %>% summarise(monthlyavg= mean(scaledsales))

trans_grp$specialday= "normal"
dayval= day(trans_grp$date)
monthval= month(trans_grp$date)
trans_grp$day= "day"
trans_grp$day= day(as.POSIXlt(trans_grp$date))

# monthly summary
jansummary= subset(trans_grp, month==1) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(jansummary)

febsummary= subset(trans_grp, month==2) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(febsummary)

marsummary= subset(trans_grp, month==3) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(marsummary)

aprlsummary= subset(trans_grp, month==4) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(aprlsummary)

maysummary= subset(trans_grp, month==5) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(maysummary)

junsummary= subset(trans_grp, month==6) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(junsummary)

julysummary= subset(trans_grp, month==7) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(julysummary)

augsummary= subset(trans_grp, month==8) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(augsummary)

sepsummary= subset(trans_grp, month==9) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(sepsummary)

octsummary= subset(trans_grp, month==10) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(octsummary)

novsummary= subset(trans_grp, month==11) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(novsummary)

decsummary= subset(trans_grp, month==12) %>% group_by(day) %>% summarise(dailyavg= mean(scaledsales))
plot(decsummary)

#from the above plots i have assumed a threshold value as, 
#-0.5 to +0.5 of dailyavg noted as normal
#below -0.5 noted as lowsales
#above +0.5 noted as highsales and identified some special days too

trans_grp$specialday[(dayval==1 & monthval==1)]= "newyear"
trans_grp$specialday[(dayval==14 & monthval==2)]= "valantineday"
trans_grp$specialday[(dayval==26 & monthval==2)]= "lowsales"
trans_grp$specialday[(dayval==1 & monthval==3)]= "highsales"
trans_grp$specialday[(dayval<=2 & monthval==4)]= "highsales"
trans_grp$specialday[(dayval==1 & dayval==31 & monthval==5)]= "highsales"
trans_grp$specialday[(dayval==1 & monthval==6)]= "highsales"
trans_grp$specialday[(dayval==26 & dayval==27 & monthval==6)]= "lowsales"
trans_grp$specialday[(dayval<=2 & monthval==7)]= "highsales"
trans_grp$specialday[(dayval<=2 & monthval==8)]= "highsales"
trans_grp$specialday[(dayval==26 & monthval==8)]= "lowsales"
trans_grp$specialday[(dayval==7 & monthval==9)]= "highsales"
trans_grp$specialday[(dayval>=29 & dayval==23 & monthval==9)]= "lowsales"
trans_grp$specialday[(dayval>=21 & dayval<=29 & monthval==10)]= "lowsales"
trans_grp$specialday[(dayval==11 & monthval==11)]= "lowsales"
trans_grp$specialday[(dayval>=18 & dayval<=25 & monthval==12)]= "chrismasweek"
trans_grp$specialday[(dayval>25 & dayval<=31 &monthval==12)]= "newyearweek"

predmatrix= model.matrix(trans_grp$tot_trans~trans_grp$wkday+trans_grp$specialday,date=trans_grp)[,-1]
modarima= auto.arima(trans[1:1200], xreg = predmatrix[1:1200,])
summary(modarima)

# from the summary we have observed MAPE is reduced from 5.062977 to 4.951246

par(mfrow= c(1,2))
acf(modarima$residuals, lag.max = 20)
pacf(modarima$residuals,lag.max = 20)

Box.test(modarima$residuals, lag=20, type = c("Ljung-Box"))

transforecastarimax= forecast(modarima, xreg = predmatrix[1201:1650,])
plot(transforecastarimax)

