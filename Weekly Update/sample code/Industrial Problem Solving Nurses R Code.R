setwd("~/R Code")

#import data
NurseInfo = read.table("nurse_info.csv",sep=",",header=TRUE)
NurseBehavior = read.table("app_behavior.csv",sep=",",header=TRUE)
NurseAccept = read.table("accept_behavior.csv",sep=",",header=TRUE)

attach(NurseInfo)
attach(NurseBehavior)
attach(NurseAccept)

summary(NurseBehavior$viewedShifts)
