###### library ###### 
library(timeDate)
library(dplyr)
library(ggplot2)
library(batman)
library(class)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(xlsx)
library(dbscan)
library(fpc)
library(magrittr)

###### wd and data sets ###### 
setwd("~/AppliedMathematics")
nurse_info<-read.csv("nurse_info.csv", header = TRUE)
accept_behaivor<-read.csv("accept_behavior.csv", header = TRUE)
app_behavior<-read.csv("app_behavior.csv", header = TRUE)

##### accept behavior #####
accept_behaivor<-data.frame(accept_behaivor,accept_date=as.Date(accept_behaivor$accept_time,"%Y-%m-%d"))
accept_behaivor<-data.frame(accept_behaivor,shift_date=as.Date(accept_behaivor$shift_time,"%Y-%m-%d"))
accept_behaivor<-data.frame(accept_behaivor,release_date=as.Date(accept_behaivor$release_time,"%Y-%m-%d"))

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
holiday<-isHoliday(gmt_shift_time)
accept_behaivor<-data.frame(accept_behaivor,holiday)
isweekend<-(accept_behaivor$weekend=="TRUE")*1
accept_behaivor<-data.frame(accept_behaivor,isweekend)
isholiday<-(accept_behaivor$holiday=="TRUE")*1
accept_behaivor<-data.frame(accept_behaivor,isholiday)
year_month<-format(accept_behaivor$shift_date,"%Y-%m")
accept_behaivor<-data.frame(accept_behaivor,year_month)
day.diff<-accept_behaivor$shift_date-accept_behaivor$accept_date
accept_behaivor<-data.frame(accept_behaivor,day.diff)
day.difference<-subset(accept_behaivor,day.diff>=20)
table(day.difference$weekend_shift)
day.diff<-as.numeric(day.diff)
quantile(day.diff,.97)
###### accept behavior weekend shift and released shifts and results #####
weekend_shift<-subset(accept_behaivor,accept_behaivor$release_day==c("Sat","Sun"))
release_shift<-subset(accept_behaivor,accept_behaivor$release_day==c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
release_shift<-data.frame(release_shift)
accept_behaivor_no_release<-subset(accept_behaivor,is.na(accept_behaivor$release_date))
accept_behaivor_no_release<-accept_behaivor_no_release[,-c(5,6)]
acc.day<-table(accept_behaivor_no_release$accept_day)
acc.day<-acc.day[c(2,6,7,5,1,3,4)]
barplot(acc.day)
weekend.shift<-table(accept_behaivor_no_release$accept_day==c("Sat","Sun"))
rownames(weekend.shift)<-c("Weekday Shifts","Weekend Shifts")
barplot(weekend.shift)
barplot(table(accept_behaivor_no_release$year_month))
pid.logins<-table(accept_behaivor_no_release$pid)
barplot(pid.logins,xlab="pid",ylab="total login activities",main="pid logins in accept shifts")

###### app behavior (standardized & clustering method) #####
standarized.app<-scale(app_behavior[,c(-1,-2,-5,-6,-7,-8,-9)])
### k-mean clustering
app.clustering<-kmeans(standarized.app,3)
plot(standarized.app, col=(app.clustering$cluster+1), main="K-Means Clustering Results with K=3", xlab="view shifts", ylab="clicked shifts")
### PCA
pca.app<-PCA(standarized.app,scale.unit = TRUE, graph = FALSE)
app.desc<-dimdesc(pca.app, axes = c(1,2), proba = 0.05)
fviz_eig(pca.app, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(pca.app, col.var = "black")
fviz_pca_var(pca.app, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
### dbscan method
v<-sort(sample(1:nrow(standarized.app),50000))
new.app<-standarized.app[v,]
kNNdistplot(new.app, k = 3)
abline(h=0.02, col = "red", lty=2)
dbscan.app<-dbscan(new.app,eps=0.02,MinPts = 3)
plot(dbscan.app,new.app)
dbscan.app$cluster

###### nurse info feature & combine 3 datasets nurse info ######
new.app<-data.frame(app_behavior$pid,standarized.app,app.clustering$cluster)
nurse_info<-data.frame(nurse_info, apply.time=as.Date(nurse_info$IP.Apply.Timestamp,"%Y-%m-%d"), first.accept.time=as.Date(nurse_info$first_accept_timestamp,"%Y-%m-%d"),first.shift=as.Date(nurse_info$first_shift_timestamp,"%Y-%m-%d"),fifth.shift=as.Date(nurse_info$fifth_shift_timestamp,"%Y-%m-%d"),termination_date=as.Date(nurse_info$termination_timestamp,"%Y-%m-%d"))
nurse_info<-nurse_info[,-c(5,6,7,8,9)]
nurse_info<-nurse_info[,c(1,2,3,4,8,9,10,11,12,5,6,7)]
date.diff<- nurse_info$first.accept.time - nurse_info$apply.time
nurse_info<-data.frame(nurse_info,date.diff)
nurse_info<-nurse_info[,c(1,2,3,4,5,6,13,7,8,9,10,11,12)]
nurse_info_no_termination<-subset(nurse_info,is.na(nurse_info$termination_date))
nurse_info_no_termination<-nurse_info_no_termination[,-10]

########### JV Thursday 3/11 ##############


# Specific Nurse, day of week they log-in         ##### Should scale to each nurse, put in a new dataset.
accept_behavior_50377 <- dplyr::filter(accept_behaivor, pid %in% c(50377))

counts <- table(accept_behavior_50377$shift_day)
  barplot(counts, main="Popular Usage Days for Each Nurse",
  xlab="Day of Week")

# Qualifications vs. Total Accepts
  qual_plot <- table()
  
# On holidays, how long ago was the shift created
holiday_behavior <- dplyr::filter(accept_behaivor, holiday %in% c(TRUE))
holiday_behavior <- transform(holiday_behavior, day.diff = as.numeric(day.diff))
days_diff <- holiday_behavior$day.diff

df_y<-data.frame(days_diff)
head(df_y,2000)
holiday_positive <- subset(holiday_behavior, holiday_behavior$day.diff>0)

hist(holiday_positive$day.diff, breaks = 10000, freq = 100, main = "Histogram of Holiday Shifts", xlim = c(0,40), xlab = "Difference in Days")
mean(holiday_positive$day.diff)
mean(holiday_positive$day.diff >= mean(holiday_positive$day.diff))
mean(holiday_positive$day.diff <= quantile(holiday_positive$day.diff, 0.2))
quantile(holiday_positive$day.diff, 0.8)
quantile(holiday_positive$day.diff, 0.2)


# On non-holidays, how long ago was the shift created
non_holiday_behavior <- dplyr::filter(accept_behaivor, holiday %in% c(FALSE))
non_holiday_behavior <- transform(non_holiday_behavior, day.diff = as.numeric(day.diff))
days_diff <- non_holiday_behavior$day.diff

df_y<-data.frame(days_diff)
head(df_y,2000)
non_holiday_positive <- subset(non_holiday_behavior, non_holiday_behavior$day.diff>0)

hist(non_holiday_positive$day.diff, breaks = 10000, freq = 100, main = "Histogram of Non-Holiday Shifts", xlim = c(0,40), xlab = "Difference in Days")
mean(non_holiday_positive$day.diff)
mean(non_holiday_positive$day.diff >= mean(non_holiday_positive$day.diff))
mean(non_holiday_positive$day.diff <= quantile(non_holiday_positive$day.diff, 0.2))
quantile(non_holiday_positive$day.diff, 0.8)
quantile(non_holiday_positive$day.diff, 0.2)


#Finding time since last login
accept_behaivor$todays_date <- "2020-12-5"
todays_date_ <- as.Date(accept_behaivor$todays_date)
accept_behaivor$todays_date <- todays_date_
#accept_behaivor<-accept_behaivor[,c(-10)]
accept_behaivor$last_login<-accept_behaivor$todays_date - accept_behaivor$accept_date
accept_behaivor$last_login<-as.numeric(accept_behaivor$last_login)
accept_behaivor <- arrange(accept_behaivor, pid)
plot(accept_behaivor$pid, accept_behaivor$last_login)


#Test showing all entries of one PID
accept_behavior_diff_new <- dplyr::filter(accept_behaivor, pid %in% c(accept_behaivor[1,1]))
accept_behavior_diff_new <- accept_behavior_scaled_diff[order(accept_behavior_scaled_diff$last_login),]
accept_behavior_diff_new<-(accept_behavior_scaled_diff[,c(-5)])

accept_behavior_diff_new<-data.frame(matrix(NA, nrow = 0, ncol = 14))
names(accept_behavior_diff_new)<-c(colnames(temp))

# Difference Begin


#Finding average login difference for each PID
for(i in 1:length(nurse_info))
{
  #accept_behavior_scaled_diff <- dplyr::filter(accept_behaivor, pid %in% c(vec[i]))
  temp<-dplyr::filter(accept_behaivor, pid %in% c(vec[i]))
  
  temp <- arrange(temp, last_login)
  
  #temp<-subset(temp,temp$last_login==min(temp$last_login)) #Error when there is only 1 value for the PID, no minimum to find.
  
  accept_behavior_diff_new[i,]<-temp[1,]
}
accept_behavior_diff_new[1,]<-temp[1,]

accept_behavior_scaled_diff_keep <- accept_behavior_scaled_diff

####################################################################################################################

# Test finding average difference in logins
accept_behavior_scaled_diff <- dplyr::filter(accept_behaivor, pid %in% c(accept_behaivor[1,1]))

for(p in 1:length(accept_behavior_scaled_diff)) 
  {
  accept_behavior_scaled_diff$each_diff[p,] <- accept_behavior_scaled_diff[p+1,]-accept_behavior_scaled_diff[p,]
  }


