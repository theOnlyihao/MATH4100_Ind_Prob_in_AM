####### MATH4100 Week 1: Statistical Analysis ####### 
setwd("/Users/yihao/Desktop/Yihao/Wentworth/Mathmatics /MATH4100 Industrial Prob in applied math/nurse_and_behavior_data")
nurse_info<-read.csv("nurse_info.csv", header = TRUE)
accept_behaivor<-read.csv("accept_behavior.csv", header = TRUE)
app_behavior<-read.csv("app_behavior.csv", header = TRUE)
accept_behaivor<-accept_behaivor[,c(-2,-4,-5)]
accept_behaivor<-accept_behaivor[1:200,] #we took first 200 observations to pratice the method we learned. 
plot(accept_behaivor$pid~accept_behaivor$shift_time)
typeof(accept_behaivor$shift_time)
dim(accept_behaivor)
shift_time<-accept_behaivor$shift_time
#accept_behaivor<-data.frame(accept_behaivor, date=as.Date.POSIXlt(shift_time,date="%Y-%m-%d %H:%M:%S"))
#strptime(shift_time,"%Y-%m-%d %H:%M:%S", tz="EST")
accept_behaivor<-data.frame(accept_behaivor,shift_time=strptime(shift_time,"%Y-%m-%d %H:%M:%S", tz="EST"))
accept_behaivor<-accept_behaivor[,c(-2)]
dim(accept_behaivor)
accept_behaivor<-as.matrix(accept_behaivor)
?kmeans
kmeans(accept_behaivor,3)


####### Week 2 #######
library(timeDate)
gmt_shift_time<-timeDate(shift_time, FinCenter = "GMT")
gmt_shift_time<-dayOfWeek(gmt_shift_time)

gmt_session<-timeDate(app_behavior$sessionDate, FinCenter = "GMT")
gmt_session<-dayOfWeek(gmt_session)
app_behavior<-data.frame(app_behavior,gmt_session)
#plot(app_behavior$gmt_session~app_behavior$pid)
