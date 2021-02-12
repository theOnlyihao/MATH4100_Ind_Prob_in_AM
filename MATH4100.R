####### MATH4100 Week 1: Statistical Analysis ####### 
##### library we used
####### MATH4100 Week 1: Statistical Analysis ####### 
##### library we used
library(timeDate)
library(dplyr)
library(ggplot2)

###### data cleaning 
setwd("...")
nurse_info<-read.csv("nurse_info.csv", header = TRUE)
accept_behaivor<-read.csv("accept_behavior.csv", header = TRUE)
app_behavior<-read.csv("app_behavior.csv", header = TRUE)
accept_behaivor<-data.frame(accept_behaivor,shift_date=as.Date(accept_behaivor$shift_time,"%Y-%m-%d"))
accept_behaivor<-accept_behaivor[,c(-3)]

########### accept behavior ##########
day<-accept_behaivor$shift_date
gmt_shift_time<-timeDate(day, FinCenter = "GMT")
day<-dayOfWeek(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,day)
head(accept_behaivor)
weekend<-isWeekend(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,weekend)
head(accept_behaivor)
holiday<-isHoliday(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,holiday)
isweekend<-(accept_behaivor$weekend=="TRUE")*1
accept_behaivor<-data.frame(accept_behaivor,isweekend)
isholiday<-(accept_behaivor$holiday=="TRUE")*1
accept_behaivor<-data.frame(accept_behaivor,isholiday)
accept_behaivor<-accept_behaivor[,c(-2,-7,-8)]
year_month<-format(accept_behaivor$shift_date,"%Y-%m")
accept_behaivor<-data.frame(accept_behaivor,year_month)
#holidayNYSE(accept_behaivor$shift_date)
?listHolidays
?holidayZURICH
?holidayNERC
?holidayNYSE

########### accept plot ###########
weekendshift1<-subset(accept_behaivor,accept_behaivor$day=="Sat" | accept_behaivor$day=="Sun",select = c(pid,day,year_month))
dummy <- toString(weekendshift1$year_month)
weekendshift1 <- data.frame(weekendshift1,dummy)
weekendshift1 <- weekendshift1[,c(-1)]
weekendshift1 <- count(weekendshift1,dummy,day,year_month)
weekendshift1 <- weekendshift1[,c(-1)]
for (i in 1:length(weekendshift1$day)){if(weekendshift1$day[i]=="Sat"){weekendshift1$day[i]<-6}else{weekendshift1$day[i]<-7}}
weekendshift1$combine <- paste(weekendshift1$year_month,weekendshift1$day)
weekendshift1 <- weekendshift1[,c(-1,-2)]
weekendshift1 <- weekendshift1[,c(2,1)]
ggplot(weekendshift1) + geom_bar(aes(x=combine,y=n)) 
weekendshift1 <- table(weekendshift1)
hist(weekendshift1)

########### app behavior ###########
gmt_session<-timeDate(app_behavior$sessionDate, FinCenter = "GMT")
day<-dayOfWeek(gmt_session)
app_behavior<-data.frame(app_behavior,day)
#plot(app_behavior$gmt_session~app_behavior$pid)
head(app_behavior)
weekend<-isWeekend(gmt_session)
app_behavior<-data.frame(app_behavior,weekend)
?isWeekend
holiday<-isHoliday(gmt_session)
app_behavior<-data.frame(app_behavior,holiday)
plot(weekend~accept_behaivor$pid)
head(weekend)
