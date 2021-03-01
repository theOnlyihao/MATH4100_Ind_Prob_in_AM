####### MATH4100 Week 1: Statistical Analysis ####### 
###### library ###### 
library(timeDate)
library(dplyr)
library(ggplot2)
library(batman)
library(class)

###### data cleaning ###### 
setwd("~/R Code")
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
#?isWeekend
holiday<-isHoliday(gmt_session)
app_behavior<-data.frame(app_behavior,holiday)
plot(weekend~accept_behaivor$pid)
head(weekend)

#Barplots
counts = table(accept_behaivor_no_release$weekend_shift)
barplot(counts, main="Non-Released Accpeted Shifts by Weekend or Weekday", xlab="Is A Weekend Shift", ylab="Accepted Shifts")

counts1 = table(app_behavior$pid)
barplot(counts1, main="Logins vs pid", xlab="pid", ylab="logins")
mean(counts1)
max(counts1)
min(counts1)
sd(counts1)

#time of sessions vs viewed shifts

sessiondates= strptime(app_behavior$sessionDate,format='%Y-%m-%d %H:%M:%S', tz="GMT")
sessiontime = format(sessiondates, '%H:%M:%S');
app_behavior=data.frame(app_behavior,sessiontime);


viewedshifts_scaled =app_behavior$viewedShifts;
app_behavior=data.frame(app_behavior,viewedshifts_scaled);
plot(app_behavior$sessiontime,app_behavior$viewedshifts_scaled, ylim=c(0,50), main='Time of day vs viewed shifts', xlab = ' ', ylab='viewed shifts', las = 2);

#Separate outliars
#STEPS:
#1st-IQR*1.5 and 3rd quantile+IQR*1.5
#IQR = difference between Q3 and Q1

Q = quantile(app_behavior$viewedShifts, probs=c(.25, .75), na.rm = FALSE);
iqr <- IQR(app_behavior$viewedShifts);
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(app_behavior, app_behavior$viewedShifts > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))

plot(eliminated$sessiontime,eliminated$viewedshifts_scaled, main='Time of day vs viewed shifts', xlab = ' ', ylab='viewed shifts', las = 2);



#Conclusion - Most of the nurses are clicking on a few shifts per sessions (makes sense).


#take average logins sessions max - min per nurse

#Standardize Set
#Standardize Set
View(app_behavior)
standarized.app<-scale(app_behavior[,c(-1,-2,-5,-6,-7,-8,-9)])
app.clustering<-kmeans(standarized.app,3)
app.clustering$cluster
plot(standarized.app, col=(app.clustering$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="")
app.clustering<-kmeans(standarized.app,5)
app.clustering$cluster
plot(standarized.app, col=(app.clustering$cluster+1), main="K-Means Clustering Results with K=5", xlab="", ylab="")

