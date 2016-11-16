library(scales)
library(ggplot2)

all <- read.csv("weibo_all_clean.csv", header = T, fill=TRUE,row.names=NULL)
all1 <- read.csv("weibo_all_clean2.csv", header = T, fill=TRUE,row.names=NULL)
# 
all<- rbind(all, all1)
# 
### number of weibo everyday for 1 year, change file name to sub_3year to obtain 3 year result
sub_360 <- read.csv("sub_360_clean.csv", header = T, fill=TRUE,row.names=NULL)
all <- read.csv("depression_fix_nohis.csv", header = T, fill=TRUE,row.names=NULL)
weibo_2016_15 <- merge(sub_360, all, by ="userid")
weibo_2016_15 <- weibo_2016_15[!duplicated(weibo_2016_15$weibo),]
weibo_2016_15$dep_sum <- rowSums(weibo_2016_15[9:19], na.rm=TRUE)

######high group
senti_weibo_h<- subset(weibo_2016_15,(weibo_2016_15$dep_sum >= 27))
senti_weibo_h[,"day_count"] <-1
weibo_day_h <- aggregate(day_count ~day, senti_weibo_h, sum)

senti_weibo_h <- merge(weibo_day_h,senti_weibo_h, by = "day", all = TRUE, sort = FALSE)

senti_weibo_h2 <- senti_weibo_h

library(data.table)
setDT(senti_weibo_h2)
user_number_h<- senti_weibo_h2[order(day), count_h := cumsum(!duplicated(userid))][,count_h := max(count_h), by = day]

#divided by number of accumulative weibo user everyday
weibo_2016_15_h2 <- transform(user_number_h, day.aver =day_count.x / count_h)

weibo_2016_15_h3 <- weibo_2016_15_h2[!duplicated(weibo_2016_15_h2$day),]


#plot depressed group 
plot(weibo_2016_15_h3$day,weibo_2016_15_h3$day.aver)

#weibo_2016_15_h3$time<- as.POSIXct(weibo_2016_15_h3$time)
q = ggplot(data = weibo_2016_15_h3, aes(x = day, y = day.aver)) +
        geom_smooth()+ scale_y_continuous("Weibo Volume")


#non-depressed group

senti_weibo_l<- subset(weibo_2016_15,(weibo_2016_15$dep_sum <= 27))
senti_weibo_l[,"day_count"] <-1
weibo_day_l <- aggregate(day_count ~day, senti_weibo_l, sum)

senti_weibo_l <- merge(weibo_day_l,senti_weibo_l, by = "day", all = TRUE, sort = FALSE)

setDT(senti_weibo_l)
user_number_l<- senti_weibo_l[order(day), count := cumsum(!duplicated(userid))][,count := max(count), by = day]

#divided by number of accumulative weibo user everyday
weibo_2016_15_l2 <- transform(user_number_l, day.aver_l =day_count.x / count)

weibo_2016_15_l3 <- weibo_2016_15_l2[!duplicated(weibo_2016_15_l2$day),]

weibo_2016_15_h3 <- as.data.frame(weibo_2016_15_h3)
#high_time <- weibo_2016_15_h3[,c('time','day.aver')]
weibo_2016_15_l3 <- as.data.frame(weibo_2016_15_l3)

weibo.day.volume.h<-weibo_2016_15_h3[,c('day','day.aver')] 
weibo.day.volume.l<-weibo_2016_15_l3[,c('day','day.aver_l')] 

weibo_day_time <- merge(weibo.day.volume.h, weibo.day.volume.l, by='day', all=T)
weibo_day_time[is.na(weibo_day_time)] <- 0

#plot non depressed group 
# plot(weibo_2016_15_l3$day.aver_l)
# 
#weibo_day_time$time<- as.POSIXct(weibo_day_time$time)
# p = ggplot(data = weibo_2016_15_l3, aes(x = time, y = day.aver_l)) + geom_smooth(colour="#FF9999")
# p + scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("1 months")) + theme(axis.text.x = element_text(angle = 45,size =12))+ scale_y_continuous("Weibo Volume")

p = ggplot(data = weibo_day_time, aes(x = -day)) + 
        geom_smooth(aes(y = day.aver, colour = "high symptom"))+
        geom_smooth(aes(y = day.aver_l, colour = "low symptom"))+
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Weibo volume")+
        theme(axis.text.x = element_text(size =14),
              axis.text.y = element_text(size =14),
              axis.title=element_text(size=16))
t.test(weibo_day_time$day.aver,weibo_day_time$day.aver_l)