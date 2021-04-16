###### library ###### 
library(timeDate)
library(dplyr)
library(ggplot2)
library(batman)
library(class)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(openxlsx)
library(dbscan)
library(fpc)
library(magrittr)
library(sqldf)
library(tidyverse)  
library(zoo)
library(cluster)
library(MASS)
library(pca3d)
library(e1071)
library(tree)
library(randomForest)

###### wd and data sets ###### 
setwd("/Users/yihao/Yihao/Wentworth/Mathmatics /MATH4100 Industrial Prob in applied math/nurse_and_behavior_data")
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

#release plots
release.shift<-table(accept_behaivor$release_day==c("Sat","Sun"))
rownames(release.shift)<-c("Weekday released","Weekend released")
barplot(release.shift)

day.diff<-accept_behaivor_no_release$shift_date-accept_behaivor_no_release$accept_date
day.diff<-as.numeric(day.diff)
accept_behaivor_no_release<-data.frame(accept_behaivor_no_release,day.diff)
#day.difference<-subset(accept_behaivor,day.diff>=20)
#table(day.difference$weekend_shift)
#day.diff<-as.numeric(day.diff)
#quantile(day.diff,.97)

#average day_diff
avg.day_diff<-aggregate(day.diff~pid,data=accept_behaivor_no_release,mean)
class(avg.day_diff)
summary(avg.day_diff)

########## app_behavior feature engineering #########
app_behavior <- data.frame(app_behavior,sesstion_date=as.Date(app_behavior$sessionDate,"%Y-%m-%d"))
app_behavior <- app_behavior[,c(-2)]
app_behavior <- app_behavior[c(1,4,2,3)]
avg.views <- aggregate(viewedShifts~pid,data=app_behavior,mean)
avg.clicks <- aggregate(clickedShifts~pid,data = app_behavior,mean)
total.views <- aggregate(viewedShifts~pid,app_behavior,FUN = "sum")
total.clicks <- aggregate(clickedShifts~pid, app_behavior,FUN = "sum")
maxday <- aggregate(sesstion_date~pid, data = app_behavior, FUN = max)
minday <- aggregate(sesstion_date~pid, data = app_behavior, FUN = min)
#lifetime_sesstion <- count(app_behavior$sesstion_date,vars = "pid")
days_between_first_and_last_app_open <- maxday$sesstion_date - minday$sesstion_date
days_between_first_and_last_app_open <- data.frame(maxday$pid,days_between_first_and_last_app_open)
colnames(days_between_first_and_last_app_open)<-c("pid","days_between_first_and_last_app_open")
clicks_per_view <- total.clicks$clickedShifts / total.views$viewedShifts
clicks_per_view <- data.frame(total.views$pid, clicks_per_view)
colnames(clicks_per_view) <- c("pid","clicks_per_view")
new_app_behavior <- left_join(total.views,total.clicks,by="pid")
new_app_behavior <- left_join(new_app_behavior,avg.views, by="pid")
new_app_behavior <- left_join(new_app_behavior,avg.clicks, by="pid")
new_app_behavior <- left_join(new_app_behavior,days_between_first_and_last_app_open,by="pid")
new_app_behavior <- left_join(new_app_behavior,clicks_per_view,by="pid")
colnames(new_app_behavior)[2:3] <- c("total.viewedShifts","total.clickedShifts")
colnames(new_app_behavior)[4:5] <- c("average.viewedShifts","average.clickedShifts")

###### nurse info feature & combine 3 datasets nurse info #####
nurse_info<-data.frame(nurse_info, apply.time=as.Date(nurse_info$IP.Apply.Timestamp,"%Y-%m-%d"), first.accept.time=as.Date(nurse_info$first_accept_timestamp,"%Y-%m-%d"),first.shift=as.Date(nurse_info$first_shift_timestamp,"%Y-%m-%d"),fifth.shift=as.Date(nurse_info$fifth_shift_timestamp,"%Y-%m-%d"),termination_date=as.Date(nurse_info$termination_timestamp,"%Y-%m-%d"))
nurse_info<-nurse_info[,-c(5,6,7,8,9)]
nurse_info<-nurse_info[,c(1,2,3,4,8,9,10,11,12,5,6,7)]
date.diff<- nurse_info$first.accept.time - nurse_info$apply.time
nurse_info<-data.frame(nurse_info,date.diff)
nurse_info<-nurse_info[,c(1,2,3,4,5,6,13,7,8,9,10,11,12)]
day.diff.first_fifth<-nurse_info$fifth.shift - nurse_info$first.shift
nurse_info<-data.frame(nurse_info,day.diff.first_fifth)
nurse_info<-nurse_info[,c(1,2,3,4,5,6,7,8,9,14,10,11,12,13)]

######### Normalized Nurse Info and apply clustering techniques ########
new_nurse_info3 <- left_join(nurse_info,avg.day_diff, by="pid")
#new_nurse_info4 <- left_join(new_nurse_info3, accept_behaivor_no_release, by="pid")
#new_nurse_info4 <- new_nurse_info4[,-c(24,25)]

new_nurse_info5 <- left_join(new_nurse_info3,new_app_behavior,by="pid")
new_nurse_info5 <- new_nurse_info5[,-c(5,6,8,9,12)]
new_nurse_info5$day.diff <- na.locf(new_nurse_info5$day.diff,fromLast = TRUE)
colnames(new_nurse_info5)[10] <- "average_days_difference_between_shifts_days_and_accept_days"
accept_behaivor_no_release$accepts = c(rep(1,))
pid_vs_accepted_shifts = aggregate(accept_behaivor_no_release$accepts, by=list(pid=accept_behaivor_no_release$pid), FUN=sum)
new_nurse_info5 <- left_join(new_nurse_info5,pid_vs_accepted_shifts,by="pid")
colnames(new_nurse_info5)[17] <- "total_accepted_shifts"
new_nurse_info5$total_accepted_shifts <-na.locf(new_nurse_info5$total_accepted_shifts,fromLast = TRUE)
new_nurse_info5 <- new_nurse_info5[,c(1:9,11,12,13,14,16,15,10,17)]
#attach(new_nurse_info3)
#attach(new_nurse_info6)
new_nurse_info6 <- new_nurse_info5[,-c(1:4,7)]
nurse_info6.scale <- new_nurse_info6 %>%
  mutate_at(c("date.diff", "day.diff.first_fifth", "Prior.Work.History..years.", 
              "Prior.Work.History..distinct.jobs.", "average.viewedShifts" , "average.clickedShifts","days_between_first_and_last_app_open", 
              "clicks_per_view","average_days_difference_between_shifts_days_and_accept_days","total_accepted_shifts",
              "total.viewedShifts","total.clickedShifts"), ~(scale(.) %>% as.vector))
nurse_info6.scale$Prior.Work.History..years. <- na.locf(nurse_info6.scale$Prior.Work.History..years.,fromLast = TRUE)
nurse_info6.pca <- prcomp(nurse_info6.scale,scale. = FALSE)
gr <- factor(new_nurse_info6[,1])
pca3d(nurse_info6.pca, group = gr)
fviz_eig(nurse_info6.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_biplot(nurse_info6.pca, label = "var", ggtheme = theme_minimal())
fviz_pca_var(nurse_info6.pca, col.var = "black")
autoplot(nurse_info6.pca)
set.seed(7000)
autoplot(kmeans(nurse_info6.scale,2),data = nurse_info6.scale, main = "PID Clusters: PCA + K-Mean Clusters")
cluster.data<-kmeans(nurse_info6.scale,2)
cluster<-cluster.data$cluster

kNNdistplot(nurse_info6.scale, k = 10)
abline(h=3, col = "red", lty=2)
db.fpc<-fpc::dbscan(nurse_info6.scale,eps=2.25,MinPts = 3)
fviz_cluster(db.fpc,nurse_info6.scale, stand = FALSE, geom = "point",main = "PID Clusters: PCA + DBSCAN Clusters")

#new_nurse_info5 <- new_nurse_info5[,-c(7)]
new_nurse_info5 <- data.frame(new_nurse_info5,cluster)

accpet_shift_date <- data.frame(accept_behaivor_no_release$pid,accept_behaivor_no_release$shift_date)
colnames(accpet_shift_date)<-c("pid","shift_date")

byweek.app<-app_behavior %>% 
  group_by(week = format(sesstion_date, '%Y-%U')) 
byweek.shift<-accept_behaivor_no_release %>% 
  group_by(week = format(shift_date, "%Y-%U"))

new_nurse_info5_clustered <- new_nurse_info5[,c(1:9,11,12,13,14,16,15,10,17,18)]
#new_nurse_info7 <- left_join(new_nurse_info5,byweek.app,by="pid")
#new_nurse_info7 <- left_join(new_nurse_info7,byweek.shift,by="pid")

#write.xlsx(new_nurse_info5, file = "new_nurse_info5.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)

########## Supervised Learning Techniques ############
median.avgDaydiff <- median(new_nurse_info5_clustered$average_days_difference_between_shifts_days_and_accept_days)
new.avgDaydiff <- as.numeric(new_nurse_info5_clustered$average_days_difference_between_shifts_days_and_accept_days > median.avgDaydiff)
newdata <- data.frame(new_nurse_info5_clustered,new.avgDaydiff)
newdata <- newdata[,-c(2:4,7)]
newdata$Prior.Work.History..years. <- na.locf(newdata$Prior.Work.History..years.,fromLast = TRUE)
View(newdata)
sapply(newdata,class)
col.name <- c("date.diff","day.diff.first_fifth","Prior.Work.History..distinct.jobs.","total.clickedShifts","days_between_first_and_last_app_open","total.viewedShifts","cluster")
newdata[col.name] <- sapply(newdata[col.name],as.numeric)
View(cor(newdata))
newdata.1 <- subset(newdata, cluster == "1")
newdata.1 <- data.frame(newdata.1)
newdata.2 <- subset(newdata, cluster == "2")
newdata.2 <- data.frame(newdata.2)
set.seed(6000)
v<-sort(sample(1:nrow(newdata.1),3000))
a<-sort(sample(1:nrow(newdata.2),300))
newdata.1test<-newdata.1[v,]
newdata.1train<-newdata.1[-v,]
newdata.2test<-newdata.2[a,]
newdata.2train<-newdata.2[-a,]
dim(newdata.1test)
dim(newdata.1train)
### logistic regression ###
attach(newdata.1)
glm_fits1<-glm(new.avgDaydiff~average_days_difference_between_shifts_days_and_accept_days+date.diff+day.diff.first_fifth+Prior.Work.History..years.+
                Prior.Work.History..distinct.jobs.+total.clickedShifts+total.viewedShifts,newdata.1train,family = binomial)
summary(glm_fits1)
coef(glm_fits1)
glm_prob1<-predict(glm_fits1,newdata.1test,type = "response")
glm_pred1<-rep("0",3000)
glm_pred1[glm_prob1>0.5]="1"
test_response1<-newdata.1$new.avgDaydiff[v]
table(glm_pred1,test_response1)
mean(glm_pred1==test_response1)

attach(newdata.2)
glm_fits2<-glm(new.avgDaydiff~average_days_difference_between_shifts_days_and_accept_days+date.diff+day.diff.first_fifth+Prior.Work.History..years.+
                 Prior.Work.History..distinct.jobs.+total.clickedShifts+total.viewedShifts,newdata.2train,family = binomial)
summary(glm_fits2)
coef(glm_fits2)
glm_prob2<-predict(glm_fits2,newdata.2test,type = "response")
glm_pred2<-rep("0",300)
glm_pred2[glm_prob2>0.5]="1"
test_response2<-newdata.2$new.avgDaydiff[a]
table(glm_pred2,test_response2)
mean(glm_pred2==test_response2)
length(test_response2)
length(glm_pred2)
### LDA ###
lda.fit1<-lda(new.avgDaydiff~average_days_difference_between_shifts_days_and_accept_days+date.diff+day.diff.first_fifth+Prior.Work.History..years.+
               Prior.Work.History..distinct.jobs.+total.clickedShifts+total.viewedShifts,newdata.1train)
lda.fit1
plot(lda.fit1)
lda.pred1<-predict(lda.fit1,newdata.1test)
names(lda.pred1)
lda.class1<-lda.pred1$class
sum(lda.pred1$posterior[,1]>0.5)
sum(lda.pred1$posterior[,1]<0.5)
table(lda.class1,test_response1)
mean(lda.class1==test_response1)
mean(lda.class1!=test_response1)

lda.fit2 <- lda(new.avgDaydiff~average_days_difference_between_shifts_days_and_accept_days+date.diff+day.diff.first_fifth+Prior.Work.History..years.+
                  Prior.Work.History..distinct.jobs.+total.clickedShifts+total.viewedShifts,newdata.2train)
plot(lda.fit2)
lda.pred2 <- predict(lda.fit2,newdata.2test)
lda.class2 <- lda.pred2$class
sum(lda.pred2$posterior[,1]>0.5)
sum(lda.pred2$posterior[,2]<0.5)
table(lda.class2, test_response2)
mean(lda.class2==test_response2)
mean(lda.class2!=test_response2)

### Tree ###
newdata1 <- newdata.1[,-c(15)]
newdata1.train <- sort(sample(1:nrow(newdata.1),3000))
newdata1.test <- newdata1[-newdata1.train,"average_days_difference_between_shifts_days_and_accept_days"]
tree.avgDay<-tree(average_days_difference_between_shifts_days_and_accept_days~.,newdata1, subset = newdata1.train)
plot(tree.avgDay)
text(tree.avgDay,pretty = 0)
summary(tree.avgDay)
yhat<-predict(tree.avgDay, newdata = newdata1[-newdata1.train,])
plot(yhat,newdata1.test)
abline(0,1)
mean((yhat-newdata1.test)^2)

cv.avgDay<-cv.tree(tree.avgDay)
plot(cv.avgDay$size,cv.avgDay$dev,type='b')
prune.avgDay<-prune.tree(tree.avgDay,best=5)
plot(prune.avgDay)
text(prune.avgDay,pretty=0)
yhat1<-predict(prune.avgDay,newdata=newdata1[-newdata1.train,])
plot(yhat1,newdata1.test)
abline(0,1)
mean((yhat1-newdata1.test)^2)

bag.avgDay<-randomForest(average_days_difference_between_shifts_days_and_accept_days~.,newdata1,subset=newdata1.train,mtry=14,importance=TRUE)
yhat.bag<-predict(bag.avgDay,newdata=newdata1[-newdata1.train,])
plot(yhat.bag, newdata1.test)
abline(0,1)
mean((yhat.bag-newdata1.test)^2)
summary(bag.avgDay)
importance(bag.avgDay)

newdata2 <- newdata.2[,-c(15)]
newdata2.train <- sort(sample(1:nrow(newdata.1),300))
newdata2.test <- newdata2[-newdata2.train,"average_days_difference_between_shifts_days_and_accept_days"]
tree.avgDay2<-tree(average_days_difference_between_shifts_days_and_accept_days~.,newdata2, subset = newdata2.train)
plot(tree.avgDay2)
text(tree.avgDay2,pretty = 0)
summary(tree.avgDay2)
yhat2<-predict(tree.avgDay2, newdata = newdata2[-newdata2.train,])
plot(yhat2,newdata2.test)
abline(0,1)
mean((yhat2-newdata2.test)^2)

cv.avgDay2<-cv.tree(tree.avgDay2)
plot(cv.avgDay2$size,cv.avgDay2$dev,type='b')
prune.avgDay2<-prune.tree(tree.avgDay2,best=6)
plot(prune.avgDay2)
text(prune.avgDay2,pretty=0)
yhat3<-predict(prune.avgDay2,newdata=newdata2[-newdata2.train,])
plot(yhat3,newdata2.test)
abline(0,1)
mean((yhat3-newdata2.test)^2)

bag.avgDay2<-randomForest(average_days_difference_between_shifts_days_and_accept_days~.,newdata2,subset = newdata2.train,mtry=14,importance=TRUE, na.action=na.roughfix)
yhat.bag2<-predict(bag.avgDay2,newdata=newdata2[-newdata2.train,])
plot(yhat.bag2, newdata2.test)
abline(0,1)
mean((yhat.bag2-newdata2.test)^2)
summary(bag.avgDay2)
importance(bag.avgDay2)




