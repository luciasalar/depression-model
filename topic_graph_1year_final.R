library(scales)
library(ggplot2)
library(lsr)

#read depression data
fix_his <- read.csv("depression_fix_nohis.csv", header = T, fill=TRUE,row.names=NULL)

#sum depression score
fix_his$dep_sum <- rowSums(fix_his[3:13])

#some of the users did not gave us Weibo account, therefore, their user id is 1-10, here we remove the users 
#withou weibo account

fix_his<-fix_his[!(fix_his$userid < 10),]

all_h <- fix_his

### analyze the four topics

######topic: happy

######datafile: 1 year: sub_360, 3 years: sub_3year, change the file name sub_360_clean(1 year)/sub_3year (3 years) to obtain different graph
#parameters for ggplot, sub_360 span:0.6, sub_3year span:0.35
sub_360 <- read.csv("sub_360_clean.csv", header = T, fill=TRUE,row.names=NULL)
weibo_2016<- merge(all_h, sub_360, by.x="userid", by.y="userid", all.y =TRUE)
weibo_2016 <- weibo_2016[complete.cases(weibo_2016),]
weibo_2016 <- weibo_2016[!duplicated(weibo_2016$weibo),]

#happy #############1075p, 7715 

weibo_2016$happy<-0
weibo_2016$happy[grep("好喜欢|美好的|好开心|可爱的|好可爱|很喜欢|最喜欢|新年快乐|好漂亮|好吃的|吐舌头|还不错|很开心|喜欢你|太棒了|有意思|我爱你|
                      太好了|谢谢你|happy|心情好|很不错|haha|好音乐|漂亮的|做鬼脸|high|节日快乐|感觉真好|喜欢吃|温柔的|喜欢我|真好看|太神奇了|
                      好好看|很好笑|好好笑|真不错|是最好的|很好吃|哇咔咔|好东西|我很喜欢|好感动|感动了|我喜欢你|好舒服|嘿嘿嘿|很好看|开开心心|
                      很漂亮|很可爱|很高兴|好厉害|很有意思|极好的|挺不错|很舒服|好听的|新年祝福|很可爱|太厉害|小可爱|笑起来|lol|很美好|很厉害|
                      yeah|很好听|幸运的|都很好|天气真好|太爽了|嘻嘻哈哈|很感动|灿烂的|无忧无虑|高兴的|热爱生活|最好吃|无与伦比|享受生活|太精彩|
                      很满意|这么帅|神清气爽|微微一笑|好样的|让人感动|越来越好|好消息|感谢大家|都不错|幸福快乐|很喜欢你|积极向上|好成绩|真漂亮|
                      好高兴|好脾气|很美丽|我很开心|好极了|无压力|满足感|mua|棒极了|真的很不错|哇哈哈哈|美美|欣喜|谢谢|太好了|极好|嘻嘻|笑翻|贴心|真爽|很爽|祝贺",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1 

indi_senti <- aggregate(happy~day+symptom_group, aggregate(happy~userid+day+symptom_group, weibo_2016,mean), mean)

happy.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = happy, colour = "high symptom"), method="loess",span=0.35)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = happy, colour = "low symptom"), method="loess",span=0.35)+
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Positive affect")+
        theme(axis.text.x = element_text(size =12))
plot(happy.plot)

#geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = somatic, colour = "high symptom"), method="gam")+ 
#geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = somatic, colour = "low symptom"), method="gam")+

#variance 
var.test(indi_senti$happy[indi_senti$symptom_group==0],indi_senti$happy[indi_senti$symptom_group==1])
#F = 0.19537, num df = 1094, denom df = 1091, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$happy[indi_senti$symptom_group==0],indi_senti$happy[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)
#t = 1.6206, df = 1500.7, p-value = 0.1053
#alternative hypothesis: true difference in means is not equal to 0
cohensD(indi_senti$happy[indi_senti$symptom_group==0],indi_senti$happy[indi_senti$symptom_group==1])

cor.test(indi_senti$happy[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])

cor.test(indi_senti$happy[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])



p<-c(.6681,.9739,.4002,.0022)

p.adjust(p, method = "holm")
#[1] 1.0000 1.0000 1.0000 0.0088
#####negative affect
weibo_2016$negative<-0
weibo_2016$negative[grep("没有朋友|没有希望|想自杀|害怕|泪流满面|怎么办|不开心|不喜欢|不容易|舍不得|不舒服|好难受|并没有|好不容易|他妈的|你怎么|神经病|不愿意|不适合|受不了|失去了|
                         怎么这么|还不如|我不想|真他妈|好难过|乱七八糟|最讨厌|不高兴|烦死了|可怕的|不得不|都不好|我不喜欢|mlgb|qnmlgb|tmd|凭什么|我讨厌|
                         放弃了|原谅我|讨厌的|不好看|做不到|无能为力|帮帮我|没意思|失败了|痛苦的|好可怕|受不了|不太好|崩溃了|不要脸|太累了|发脾气|没必要|
                         完蛋了|不值得|一塌糊涂|好可怜|不好了|浪费时间|累死了|气死我了|浪费了|饿死了|fuck|安慰自己|迟到了|如何是好|绝望的|是怎么回事|气死了|
                         好想哭|胡思乱想|不甘心|不好吃|不习惯|生气了|这种人|很不好|更可怕|拒绝了|受伤的|不快乐|自私的|不在乎|失望了|也没有|好恶心|恨不得|sad|
                         不够好|抛弃了|很讨厌|不好玩|好心疼|怪怪的|停电了|我都不知道|没出息|你居然|你到底|买不起|不下去了|肆无忌惮|有点累|不理解|受伤了|
                         算了吧|没有本事|抵不过|突如其来|太不容易|不适合|心力交瘁|很严重|冷冷的|烦心事|无所事事|看不起|患得患失|无言以对|真不容易|分手了|
                         对不起|竭斯底里|吓死了|压力大|不满意|很无奈|吓死我了|无法呼吸|无可奈何|掉眼泪|去死吧|没有办法|有点烦|流眼泪|真的很难|不幸福|
                         好吓人|很生气|真恶心|不愉快|很可怕|撕心裂肺|很遗憾|很难受|很痛苦|力不从心|坏脾气|欲哭无泪|真受不了|去你妈|很不爽|口是心非|心烦意乱|难过了",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1

indi_senti <- aggregate(negative~day+symptom_group, aggregate(negative~userid+day+symptom_group, weibo_2016,mean), mean)

negative.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = negative, colour = "high symptom"), method="loess",span=0.60)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = negative, colour = "low symptom"), method="loess",span=0.60)+
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Negative affect")+
        theme(axis.text.x = element_text(size =12))
plot(negative.plot)

#variance in 1 year
var.test(indi_senti$negative[indi_senti$symptom_group==0],indi_senti$negative[indi_senti$symptom_group==1])
#F = 0.18018, num df = 1094, denom df = 1091, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$negative[indi_senti$symptom_group==0],indi_senti$negative[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)

#alternative hypothesis: true difference in means is not equal to 0

cor.test(indi_senti$negative[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])

cor.test(indi_senti$negative[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])



p<-c(.9,.2353,.0515,.3498)

p.adjust(p, method = "holm")
#####family
weibo_2016$family<-0
weibo_2016$family[grep("我爸|我妈|父母|我父亲|我母亲|我奶奶|我爷爷|我外公|我外婆|舅|亲戚|嫂子|大姨|小姨|姑姑|小叔子|大伯|儿子|女儿老公|老婆|男朋友|女朋友|bf|gf|妻子|丈夫|爱人|
                       爸爸|妈妈|老爸|老妈|老妈子|我的妈妈|姨妈|奶奶家|姨奶奶|奶奶家|外婆家|我家|我们家|咱们家|伯母|亲家母|亲家公|祖母|祖父|友人|亲友团|亲友们|哥哥|妹妹|闺蜜|好友|我朋友|男友|女友|朋友|
                       表哥|表姐|表弟|我老公|我老婆|侄子|侄女|媳妇|亲妈|我弟|我妹|老弟|老妹|咱家|姥姥|姥爷",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1

indi_senti <- aggregate(family~day+symptom_group, aggregate(family~userid+day+symptom_group, weibo_2016,mean), mean)

family.3yr.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = family, colour = "high symptom"), method="loess",span=0.60)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = family, colour = "low symptom"), method="loess",span=0.60)+        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Family")+
        theme(axis.text.x = element_text(size =12))
plot(family.plot)

#variance in 1 year
var.test(indi_senti$family[indi_senti$symptom_group==0],indi_senti$family[indi_senti$symptom_group==1])
#F = 0.1945, num df = 1094, denom df = 1091, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$family[indi_senti$symptom_group==0],indi_senti$family[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)

#alternative hypothesis: true difference in means is not equal to 0

#cohen's d
cohensD(indi_senti$family[indi_senti$symptom_group==0],indi_senti$family[indi_senti$symptom_group==1])

cor.test(indi_senti$family[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])

cor.test(indi_senti$family[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])


p<-c(.02, .13)
p.adjust(p, method = "holm")

#####somatic
weibo_2016$somatic<-0
weibo_2016$somatic[grep("不舒服|睡不着|肚子疼|拉肚子|肚子痛|口腔溃疡|腰酸背痛|神经衰弱|扁桃体|嗓子疼|颈椎|喉咙痛|偏头痛|肠胃炎|肠绞痛|心绞痛|神经痛|心慌|心律不齐|呼吸困难|头昏脑胀|
                        腰酸背疼|偏头疼|刺痛感|头疼脑热|胃溃疡|高烧不退|太累了|累死了|有点累|睡眠质量不好|不睡觉|睡眠不住|我累了|昏昏欲睡|不眠之夜|安眠药|发高烧|胸口痛|心跳加速|头很痛|头很疼|溃疡|胃难受|消化不良|
                        睡不着|累得要死|闹肚子|累死累活|烧起来|退烧药|睡不到|寝食难安|上吐下泻|彻夜未眠|疼痛不堪|食欲不佳|失眠病|失眠了|胸闷|头晕|肩周炎|肩膀痛|脖子痛|剧痛|生病了|想吐|发高烧|胃难受|胸涨|
                        拉肚子|肠胃|作呕|头好晕|胃酸|腰疼|腹疼|做噩梦|烧心|没胃口|反胃|胃不舒服|胃疼|胃痛|酸胃|心悸|抽筋|疼痛",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1

indi_senti <- aggregate(somatic~day+symptom_group, aggregate(somatic~userid+day+symptom_group, weibo_2016,mean), mean)

somatic.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        #       geom_point(data = subset(indi_senti, symptom_group==1), aes(y = somatic, colour = "high symptom")+
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = somatic, colour = "high symptom"), method="loess",span=0.60)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = somatic, colour = "low symptom"), method="loess",span=0.60)+        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("somatic")+
        theme(axis.text.x = element_text(size =12))
plot(somatic.plot)


#variance in 1 year
var.test(indi_senti$somatic[indi_senti$symptom_group==0],indi_senti$somatic[indi_senti$symptom_group==1])
#F = 0.1433, num df = 1094, denom df = 1091, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$somatic[indi_senti$symptom_group==0],indi_senti$somatic[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)

#alternative hypothesis: true difference in means is not equal to 0

#cohen's d
cohensD(indi_senti$somatic[indi_senti$symptom_group==0],indi_senti$somatic[indi_senti$symptom_group==1])

cor.test(indi_senti$somatic[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])

cor.test(indi_senti$somatic[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])


p<-c(.0001,.001,.002,.04)

p.adjust(p, method = "holm")
#######language style
#####first person pronoun
weibo_2016$self<-0
weibo_2016$self[grep("我|自己",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1

indi_senti <- aggregate(self~day+symptom_group, aggregate(self~userid+day+symptom_group, weibo_2016,mean), mean)

self.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = self, colour = "high symptom"), method="loess",span=0.35)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = self, colour = "low symptom"), method="loess",span=0.35)+        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("1st person pronoun")+
        theme(axis.text.x = element_text(size =12))


plot(self.plot)


#variance in 1 year
var.test(indi_senti$self[indi_senti$symptom_group==0],indi_senti$self[indi_senti$symptom_group==1])
#F = 0.22959, num df = 364, denom df = 364, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$self[indi_senti$symptom_group==0],indi_senti$self[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)
#t = 0.55103, df = 1464.5, p-value = 0.5817
#alternative hypothesis: true difference in means is not equal to 0

cor.test(indi_senti$self[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])
#low 0.08298147   ##p-value = 0.1135
cor.test(indi_senti$self[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])
#high -0.09091914    p-value = 0.0828

t.test(indi_senti$self[indi_senti$symptom_group==0][1:365],indi_senti$self[indi_senti$symptom_group==1][1:365], var.equal=F, paired=FALSE)
#t = -1.4644, df = 483.4, p-value = 0.1437                                                                                                       t = -1.4644, df = 483.4, p-value = 0.1437
t.test(indi_senti$self[indi_senti$symptom_group==0][366:721],indi_senti$self[indi_senti$symptom_group==1][366:721], var.equal=F, paired=FALSE)
#t = -1.4165, df = 454.23, p-value = 0.1573
t.test(indi_senti$self[indi_senti$symptom_group==0][722:1095],indi_senti$self[indi_senti$symptom_group==1][722:1095], var.equal=F, paired=FALSE)

t.test(indi_senti$self[indi_senti$symptom_group==0][1:270],indi_senti$self[indi_senti$symptom_group==1][1:270], var.equal=F, paired=FALSE)
#t = 2.2412, df = 383.47, p-value = 0.02559  difference in the recent 9 months


#######
weibo_2016$them<-0
weibo_2016$them[grep("他|她|他们|她们",weibo_2016$weibo)]<-1

weibo_2016$symptom_group<-0
weibo_2016$symptom_group[weibo_2016$dep_sum>27]<-1

indi_senti <- aggregate(them~day+symptom_group, aggregate(them~userid+day+symptom_group, weibo_2016,mean), mean)

them.plot= ggplot(data = indi_senti, aes(x = -day)) + 
        geom_smooth(data = subset(indi_senti, symptom_group==1), aes(y = them, colour = "high symptom"), method="loess",span=0.35)+ 
        geom_smooth(data = subset(indi_senti, symptom_group==0), aes(y = them, colour = "low symptom"), method="loess",span=0.35)+        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("3rd person pronoun")+
        theme(axis.text.x = element_text(size =12))


plot(them.plot)


#variance in 1 year
var.test(indi_senti$them[indi_senti$symptom_group==0],indi_senti$them[indi_senti$symptom_group==1])
#F = 0.17401, num df = 1094, denom df = 1091, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1

#t-test 1 year
t.test(indi_senti$them[indi_senti$symptom_group==0],indi_senti$them[indi_senti$symptom_group==1], var.equal=F, paired=FALSE)
#t = 3.7621, df = 536.05, p-value = 0.000187
#alternative hypothesis: true difference in means is not equal to 0
t.test(indi_senti$them[indi_senti$symptom_group==0][1:366],indi_senti$them[indi_senti$symptom_group==1][1:366], var.equal=F, paired=FALSE)

#cohen's d
cohensD(indi_senti$them[indi_senti$symptom_group==0][1:366],indi_senti$them[indi_senti$symptom_group==1][1:366])

cor.test(indi_senti$them[indi_senti$symptom_group==0], -indi_senti$day[indi_senti$symptom_group==0])
#low 0.003671567    ##p-value = 0.9443
cor.test(indi_senti$them[indi_senti$symptom_group==1], -indi_senti$day[indi_senti$symptom_group==1])
#-0.05826022  p-value = 0.2669


###sentiment
sub_360 <- read.csv("sub_360_clean.csv", header = T, fill=TRUE,row.names=NULL)
weibo_2016<- merge(all_h, sub_360, by.x="userid", by.y="userid", all.y=TRUE)
weibo_2016 <- weibo_2016[!duplicated(weibo_2016),]

n <- weibo_2016[!duplicated(weibo_2016$userid),]

#select weibo#############################
weibo_2016_c_recent_l<- subset(weibo_2016,(weibo_2016$dep_sum <=27))
weibo_2016_c_recent_h<- subset(weibo_2016,(weibo_2016$dep_sum > 27))

#weibo weibo_2016 count per day from each group
#weibo_2016 count per day from each group

indi_senti_l <- aggregate(sentiment~day, aggregate(sentiment~userid+day, weibo_2016_c_recent_l,mean), mean)
indi_senti_h <- aggregate(sentiment~day, aggregate(sentiment~userid+day, weibo_2016_c_recent_h,mean), mean)

#merge day count with weibo_2016 count
all_count<- merge(indi_senti_l, indi_senti_h, by = "day", all =TRUE)
all_count[is.na(all_count )] <- 0
colnames(all_count) <- c("day","day.aver_l","day.aver_h")

##divide weibo_2016 count by day count
# all_count <- transform(all_count, day.aver_l = sentiment_l / day_count_l)
# all_count <- transform(all_count, day.aver_h = sentiment_h / day_count_h)
# all_count[is.na(all_count )] <- 0

sentiment.plot = ggplot(data = all_count, aes(x = -day)) + 
        geom_smooth(aes(y = day.aver_l, colour = "low symptom"))+ 
        geom_smooth(aes(y = day.aver_h, colour = "high symptom"))+ 
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Valence")+
        theme(axis.text.x = element_text(size =12),
              axis.title=element_text(size=16,face="bold"))



t.test(all_count$day.aver_l[1:1095],all_count$day.aver_h[1:1095], var.equal=F, paired=FALSE)
#t = 5.0761, df = 1512.3, p-value = 4.329e-07
cohensD(all_count$day.aver_l[1:1095],all_count$day.aver_h[1:1095])
#0.2169413
var.test(all_count$day.aver_l[1:1095],all_count$day.aver_h[1:1095], var.equal=F, paired=FALSE)

t.test(all_count$day.aver_l[1:365],all_count$day.aver_h[1:365], var.equal=F, paired=FALSE)
#t = 5.5321, df = 544.83, p-value = 4.921e-08
cohensD(all_count$day.aver_l[1:365],all_count$day.aver_h[1:365])
#0.4095009


#sentiment score and dep score

#count the number of weibo in each id
library(plyr)
s <- as.data.frame(ddply(weibo_2016,.(userid),nrow))
colnames(s) <- c("userid", "count")

#merge number of weibo with depression score
weibo_2016<- merge(s, weibo_2016, by.x="userid", by.y="userid", all.x=TRUE)

#aggregate sentiment score according to userid

sentiment_dep <- aggregate(sentiment ~userid, data=weibo_2016, sum)
dep_weibo<- merge(weibo_2016, sentiment_dep, by.x="userid", by.y="userid", all.x=TRUE)
dep_weibo <- transform(dep_weibo, aver.senti = sentiment.y / count)
user_no<- dep_weibo[!duplicated(dep_weibo$userid),]

dep_weibo<- dep_weibo[complete.cases(dep_weibo),]

cor.test(dep_weibo$aver.senti,dep_weibo$dep_sum)


#1 year -0.07792012  p-value < 2.2e-16
# 
# 



#72 users

####positive and negative valence 
sub_360 <- read.csv("sub_360_clean.csv", header = T, fill=TRUE,row.names=NULL)
weibo_2016<- merge(all_h, sub_360, by.x="userid", by.y="userid", all.y=TRUE)
weibo_2016 <- weibo_2016[!duplicated(weibo_2016),]

n <- weibo_2016_c_recent_h[!duplicated(weibo_2016_c_recent_h$userid),]

#select weibo#############################
weibo_2016_c_recent_l<- subset(weibo_2016,(weibo_2016$dep_sum <=27))
weibo_2016_c_recent_h<- subset(weibo_2016,(weibo_2016$dep_sum > 27))

pos.l <- subset(weibo_2016_c_recent_l, sentiment > 0)
neg.l <- subset(weibo_2016_c_recent_l, sentiment < 0)

pos.h <- subset(weibo_2016_c_recent_h, sentiment > 0)
neg.h <- subset(weibo_2016_c_recent_h, sentiment < 0)

#sum the pos valence
indi_senti_l <- aggregate(sentiment~day, aggregate(sentiment~userid+day, pos.l,mean), mean)
indi_senti_h <- aggregate(sentiment~day, aggregate(sentiment~userid+day, pos.h,mean), mean)

#sum neg valence
indi_senti_ln <- aggregate(sentiment~day, aggregate(sentiment~userid+day, neg.l,mean), mean)
indi_senti_hn <- aggregate(sentiment~day, aggregate(sentiment~userid+day, neg.h,mean), mean)

#merge pos in high and low group
all_count<- merge(indi_senti_l, indi_senti_h, by = "day", all =TRUE)
all_count[is.na(all_count )] <- 0
colnames(all_count) <- c("day","day.pos.aver_l","day.pos.aver_h")

#merge neg in high and low group

all_count2<- merge(indi_senti_ln, indi_senti_hn, by = "day", all =TRUE)
all_count2[is.na(all_count2)] <- 0
colnames(all_count2) <- c("day","day.neg.aver_l","day.neg.aver_h")

#plot 
pos.valence.plot = ggplot(data = all_count, aes(x = -day)) + 
        geom_smooth(aes(y = day.pos.aver_l, colour = "low symptom"))+ 
        geom_smooth(aes(y = day.pos.aver_h, colour = "high symptom"))+ 
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Positive valence")+
        theme(axis.text.x = element_text(size =12),
              axis.title=element_text(size=16,face="bold"))

pos.valence.plot


neg.valence.plot= ggplot(data = all_count2, aes(x = -day)) + 
        geom_smooth(aes(y = day.neg.aver_l, colour = "low symptom"))+ 
        geom_smooth(aes(y = day.neg.aver_h, colour = "high symptom"))+ 
        scale_color_manual(values=c("red","blue"))+
        scale_y_continuous("Negative valence")+
        theme(axis.text.x = element_text(size =12),
              axis.title=element_text(size=16,face="bold"))

neg.valence.plot


t.test(pos.l$sentiment, pos.h$sentiment)

t.test(all_count$day.pos.aver_l[1:365],all_count$day.pos.aver_h[1:365], var.equal=F, paired=FALSE)
#t = 0.31086, df = 506.29, p-value = 0.756

# t = 0.27842, df = 15470, p-value = 0.7807
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#         -0.004529665  0.006029547
# sample estimates:
#         mean of x mean of y 
# 0.4905053 0.4897554 
t.test(neg.l$sentiment, neg.h$sentiment)
# t = 3.2553, df = 13125, p-value = 0.001135
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#         0.004425296 0.017819854
# sample estimates:
#         mean of x  mean of y 
# -0.4551432 -0.4662658 

