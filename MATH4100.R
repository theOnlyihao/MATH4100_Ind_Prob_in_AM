####### MATH4100 Week 1: Statistical Analysis ####### 
##### library we used
library(timeDate)
library(dplyr)

###### data cleaning 
setwd(...)
nurse_info<-read.csv("nurse_info.csv", header = TRUE)
accept_behaivor<-read.csv("accept_behavior.csv", header = TRUE)
app_behavior<-read.csv("app_behavior.csv", header = TRUE)

########### accept behavior ##########
gmt_shift_time<-timeDate(shift_time, FinCenter = "GMT")
shift_day<-dayOfWeek(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,shift_day)
head(accept_behaivor)
weekend<-isWeekend(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,weekend)
head(accept_behaivor)
holiday<-isHoliday(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,holiday)
?timeSequence

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
