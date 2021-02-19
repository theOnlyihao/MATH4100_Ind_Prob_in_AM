####### MATH4100 Week 1: Statistical Analysis ####### 
###### library ###### 
library(timeDate)
library(dplyr)
library(ggplot2)
library(batman)

###### data cleaning ###### 
setwd("...")
nurse_info<-read.csv("nurse_info.csv", header = TRUE)
accept_behaivor<-read.csv("accept_behavior.csv", header = TRUE)
app_behavior<-read.csv("app_behavior.csv", header = TRUE)

########### accept behavior ##########
accept_behaivor<-data.frame(accept_behaivor,accept_date=as.Date(accept_behaivor$accept_time,"%Y-%m-%d"))
accept_behaivor<-data.frame(accept_behaivor,shift_date=as.Date(accept_behaivor$shift_time,"%Y-%m-%d"))
accept_behaivor<-data.frame(accept_behaivor,release_date=as.Date(accept_behaivor$release_time,"%Y-%m-%d"))
#accept_behaivor<-accept_behaivor[,c(-3)]
gmt_accept_time<-timeDate(accept_behaivor$accept_date,FinCenter = "GMT")
gmt_shift_time<-timeDate(accept_behaivor$shift_date, FinCenter = "GMT")
gmt_release_time<-timeDate(accept_behaivor$release_date,FinCenter = "GMT")
accept_day<-dayOfWeek(gmt_accept_time)
shift_day<-dayOfWeek(gmt_shift_time)
release_day<-dayOfWeek(gmt_release_time)
accept_behaivor<-data.frame(accept_behaivor,accept_day,shift_day,release_day)
accept_behaivor<-accept_behaivor[,c(-2,-3,-4,-5)]
accept_behaivor<-accept_behaivor[,c(1,2,5,3,6,4,7)]
weekend_shift<-isWeekend(gmt_shift_time)
weekend_release<-isWeekend(gmt_release_time)
accept_behaivor<-data.frame(accept_behaivor,weekend_shift,weekend_release)
accept_behaivor<-accept_behaivor[,c(-9)]



weekend_shift<-subset(accept_behaivor,accept_behaivor$release_day==c("Sat","Sun"))
release_shift<-subset(accept_behaivor,accept_behaivor$release_day==c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
release_shift<-data.frame(release_shift)
accept_behaivor_no_release<-subset(accept_behaivor,is.na(accept_behaivor$release_date))


accept_behaivor<-data.frame(accept_behaivor,release_day_isweekend)
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
