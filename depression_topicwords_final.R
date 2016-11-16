#segment words
#install.packages("rmmseg4j", repos="http://R-Forge.R-project.org",type='source')
require(tm)
require(ggplot2)
library(rmmseg4j)
library(wordcloud)
library(proxy)

#this script generate a 

#we used a separate set of data to generate topics, 

w <- read.csv("weibo_scrapy.csv", header = F, fill=TRUE,row.names=NULL)
colnames(w) <- c("name", "gender", "location", "intro", "weibo_num","follow","fans","like","retweet","comment","time","phone","weibo")
write.csv(w,file='w.csv')

#clean all the non original data
w2 <- w[- grep("映客iOS|微博等级|网易云音乐|荔枝FM|明星势力榜|QQ音乐|Feelapp|手机NBA中文官网|微博红包|粉丝红包|微卡券|一直播|LOFTER|微博会员中心|微博活动|百词斩图背单词|让红包飞手机版|天下3|微博等级|分享按钮|背单词", w$phone),]
w3 <- w2[- grep("围观世界杯领现金|围观世界杯|微博踢球|微话题|你画我猜欢乐版|天下3|分享按钮|微盘|投票|微博电视雷达|疯狂开宝箱|知命|全民K歌|唱吧|微博之夜|勋章馆|专题|微专题|测试|多推|新浪博客|啪啪|樱桃小丸子|多推|Nikepluschina|腾讯新闻客户端|滴滴出行", w2$phone),]
#remove public account
w3 <- w3[- grep("治疗|科普|医院", w3$name),]
w3 <- w3[- grep("滴滴出行|🛫", w3$weibo),]

#remove LOCATION, BRAKETS, PUNCUTATION, NUMBERS AND ENGLISH 
w3$weibo <- gsub("\\S+·\\S+", "", w3$weibo)
w3$weibo <- gsub("\\S+ 我在:\\S+", "", w3$weibo)
w3$weibo <- gsub("\\S+ 我在这里:\\S+", "", w3$weibo)
#w3$weibo <- gsub("\\S+ 我在.?:\\S+", "", w3$weibo)
w3$weibo <- gsub("\\s*\\[[^\\)]+\\]","", w3$weibo)
w3$weibo <- gsub("\\s*\\《[^\\)]+\\》","", w3$weibo)
w3$weibo <- gsub("[[:punct:]]", "", w3$weibo)
w3$weibo <- gsub("[[:digit:]]+", "", w3$weibo)
w3$weibo <- gsub("分享图片", "", w3$weibo)
w3$weibo <- gsub("([A-Za-z]+).*", "", w3$weibo)

#remove empty rows

#order a column
# dd[ order(dd[,4], dd[,1]), ]
# dput(w3[1:10,13])

w4 <- w3[-which(w3$weibo == "" | w3$weibo == " " | w3$weibo == "  "), ]

#remove stopwords
w4$weibo <- gsub("的|了|在|是|我|有|和|就|人|都|一个|上|也|很|你|会|着|看|自己|我们|啊|啦", "", w4$weibo)

w5 <- w4

ss <- as.character(w5$weibo)

seg <- mmseg4j(ss)

#NGramTokenizer(' 中华人民共和国成立于1949 年')

#corpus
corpus.dep <- Corpus(VectorSource(seg))
tdm.dep <- TermDocumentMatrix(corpus.dep, control = list(weighting = weightTfIdf))

m1 <- as.matrix(tdm.dep)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
par(family='STKaiti')
cloud_1 <- wordcloud(d$word, d$freq, min.freq = 50, random.order = F, ordered.colors = F, 
                     colors = rainbow(length(row.names(m1))))

write.csv(d, "depression_topic.csv")

###we rated positive, negative and health related words in depression_topic.csv, see dep_word.xls in data folder







