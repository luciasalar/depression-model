#analysis of depression scale data (n= 807)   https://www.wenjuan.com/s/YvMzm2/
#all users
all <- read.csv("depression_fix.csv", header = T, fill=TRUE,row.names=NULL)
all$X <- NULL
fix_his <- all

fix_his <- fix_his[!duplicated(fix_his$user_id),]
fix_his[is.na(fix_his)] <- 0

# #sum depression score
fix_his$dep_score <- rowSums(fix_his[7:17])

#remove na
fix_his[is.na(fix_his)] <- 0

#general stats

mean(fix_his$dep_score)
sd(fix_his$dep_score)

#23.57
#6.0731

all_h <- fix_his
#Cronbach's alpha
all2<- all_h


#mean score of the three groups
dep_h_y<- subset(all_h, all_h$dep_history == 1)  
mean(dep_h_y$dep_score)
sd(dep_h_y$dep_score)
#26.00637
#7.131994

#participants in each group
length(which(all_h$dep_history == 1))
length(which(all_h$dep_history == 2))
length(which(all_h$dep_history == 3))

#difference between big change and no big change group
Yes <- subset(all_h, Big_change == 1)
No <- subset(all_h, Big_change == 2)

t.test(Yes$dep_score,No$dep_score)

all_h <- fix_his

#anova
all_h$changes <- 0
all_h$changes[all_h$change_L==1]<-1
all_h$changes[all_h$change_R==1]<-2
all_h$changes[all_h$change_J==1]<-3

length(which(all_h$changes == 0))

mean(all_h$dep_score[all_h$changes==0])

all_h$changes <- as.factor(all_h$changes)
fit <- aov(dep_score ~ changes, data=all_h)
summary(fit)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
# changes        1   1291  1290.9    34.2 6.07e-09 ***
# Residuals   1534  57907    37.7     

#turkey's test
posthoc <- TukeyHSD(fit, which= 'changes', conf.level=0.95)
plot(fit)

all_h <- fix_his
#remove group 0, those who choose not to answer
all_h <- subset(all_h, dep_history > 0)
all_h$dep_history<- as.factor(all_h$dep_history)
fit2 <- aov(dep_score ~ dep_history, data=all_h)
summary(fit2)

posthoc2 <- TukeyHSD(fit2, which= 'dep_history', conf.level=0.95)
posthoc2

#               Df Sum Sq Mean Sq F value   Pr(>F)    
# dep_history    1    917   917.4   24.15 9.89e-07 ***
# Residuals    1534  58281    38.0                     
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# $dep_history
# diff       lwr        upr     p       adj
# 2-1 -4.0140647 -5.239043 -2.7890869 0.0000000
# 3-1 -0.4147901 -1.647447  0.8178668 0.7095965
# 3-2  3.5992746  2.847919  4.3506297 0.0000000

#Question: Did you develop a deprssion episode before>
#no
dep_h_n<- subset(all_h, all_h$dep_history == 2)  
mean(dep_h_n$dep_score)
#21.99293
#4.908073

# dont know
dep_h_d<- subset(all_h, all_h$dep_history == 3)  
mean(dep_h_d$dep_score)
sd(dep_h_d$dep_score)
#25.59427
#6.577663

#yes
dep_h_y<- subset(all_h, all_h$dep_history == 1)  
mean(dep_h_y$dep_score)
sd(dep_h_y$dep_score)
#26.02532
# 7.113233

#participants in three kinds of changes in life
length(which(all_h$change_L == 1))
length(which(all_h$change_R == 1))
length(which(all_h$change_J == 1))
#156
#148
# 52

#overlap answers, some participants have multiple symptoms
length(which(all_h$change_L == 1 & all_h$change_R == 1))
length(which(all_h$change_R == 1 & all_h$change_J == 1 ))
length(which(all_h$change_J == 1 & all_h$change_L == 1 ))
length(which(all_h$change_J == 1 & all_h$change_L == 1 & all_h$change_R == 1 ))
#28
#12
#24
#8

