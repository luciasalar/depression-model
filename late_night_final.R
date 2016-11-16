#read depression data
fix_his <- read.csv("depression_fix_nohis.csv", header = T, fill=TRUE,row.names=NULL)


#sum depression score
fix_his$dep_sum <- rowSums(fix_his[3:13])

fix_his<-fix_his[!(fix_his$userid < 10),]

all_h <- fix_his

######datafile: 1 year: sub_360, 3 years: sub_3year, change the file name in read to obtain different graph
sub_360 <- read.csv("sub_3year.csv", header = T, fill=TRUE,row.names=NULL)
weibo_2016<- merge(all_h, sub_360, by.x="userid", by.y="userid", all.y =TRUE)

#merge with detailed time 
senti_weibo_clean <- read.csv("senti_weibo_clean.csv", header = T, fill=TRUE,row.names=NULL)
detailed_time <- senti_weibo_clean[,c('time','weibo','userid')] 
sentiment_dep_360_time <- merge(detailed_time, sub_360, by=c("userid","weibo"), all.y=TRUE)

#sentiment_dep_360_time <- sentiment_dep_360_time[!is.na(sentiment_dep_360_time$userid),]
sentiment_dep_360_time <- sentiment_dep_360_time[!duplicated(sentiment_dep_360_time$weibo),]


#time of posting weibo and depression score (178) should be divided by count 
late_night <- sentiment_dep_360_time[grep("23:| 01:| 02:| 03:| 04:| 05:", sentiment_dep_360_time$time.x),]

library(plyr)
#count the number of weibo at midnight for each user
mn <- as.data.frame(ddply(late_night,.(userid),nrow))
colnames(mn) <- c("userid", "count_m")

#day count 
sub_360[,"weibo_count"] <-1
day_count <- aggregate(weibo_count ~userid, data=sub_360, sum)

colnames(day_count) <- c("userid", "weibo_count")

late_night<- merge(mn, day_count, by.x="userid", by.y="userid", all.x=TRUE)
#merge number of weibo at midnight with the table
#late_night<- merge(mn, sentiment_dep_360_count, by.x="day", by.y="day", all.x=TRUE)

#the percentage of late night weibo 
late_night<- transform(late_night, late_per = count_m / weibo_count)

#merge with depression score
all <- read.csv("depression_fix.csv", header = T, fill=TRUE,row.names=NULL)
all<-all[!(all$user_id < 100),]
late_night2<- merge(late_night, all, by.x = 'userid',by.y = 'user_id', all=TRUE)

late_night2 <- late_night2[complete.cases(late_night2$weibo_count),]
late_night2[is.na(late_night2)] <- 0

late_night2$dep_sum <- rowSums(late_night2[,11:21])

cor.test(late_night2$dep_sum,late_night2$late_per)
#0.06206001 
#p-value = 0.02687

cor.test(late_night2$dep_R4,late_night2$late_per)
#0.0274865  p-value = 0.3273

cor.test(late_night2$dep_R4,late_night2$dep_sum)

late.plot = ggplot(data = late_night2, aes(x = dep_sum)) + 
        geom_smooth(aes(y = late_per))+ 
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Percentage of Weibo at night")+
        theme(axis.text.x = element_text(size =12))+
        labs(x = "depression score")+
        theme(axis.text.y = element_text(size =12),
              axis.title=element_text(size=16,face="bold"))
 

volume.plot = ggplot(data = late_night2, aes(x = dep_sum)) + 
        geom_smooth(aes(y = weibo_count ))+ 
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Weibo volume")+
        theme(axis.text.x = "element_text"(size =12))+
        labs(x = "depression score")+
        theme(axis.text.y = element_text(size =12),
              axis.title=element_text(size=16,face="bold"))
 

cor.test(late_night2$weibo_count,late_night2$dep_sum)
#-0.1695727 p-value = 1.158e-09

multiplot(late.plot,volume.plot)
