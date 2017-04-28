#### 11.1.1 宏观收入分析
# 收入指标日总况
revenue <- read.csv("付费总况.csv")
head(revenue)
library(reshape)
md <- melt(revenue,id='日期')
head(md)
library(recharts)
echartr(md,日期,value,variable,type=c(rep('vbar',2),rep('line',4))) %>%
  setY1Axis(series=3:6) %>%     # 设置Y轴副坐标轴
  setSymbols('emptycircle') %>%
  setToolbox(show=FALSE)
# 终端品牌的付费人数统计
brand <- data.frame(
  '品牌'= c("OPPO","HUAWEI","vivo", "Xiaomi", "samsung", "HONOR",  
           "Meizu","GIONEE","LeEco","Coolpad"),
  '付费人数'=c(711, 519, 486, 477, 388, 260, 156, 
           57, 53, 46))
echartr(brand,品牌,付费人数,type = "vbar") %>%
  setToolbox(show=FALSE)

#####  11.2.2 数据探索分析
w <- read.csv("经济系统数据.csv",T) # 导入数据
# 1. 相关性分析
# 研究货币量与活跃用户、新增用户间的关系
w <- w[,-1] # 删除日期
w1 <- w[,c(1:10,16:17)] #选择货币量类与活跃用户类、新增用户类等字段的数据
(r1 <- round(cor(w1[,1:6],w1[,7:12]),2)) # 计算相关系数
library(corrplot)
corrplot(r1,method = "ellipse") # 绘制相关系数图
# 研究货币量与付费用户间的关系
w2 <- w[,c(1:6,11:15)] # 选择货币量与付费用户的字段数据
(r2 <- round(cor(w2[,1:6],w2[,7:11]),2)) # 计算相关系数
corrplot(r2,method = "ellipse") # 绘制相关系数图
# 货币量与用户玩牌数据间的关系研究
w3 <- w[,c(1:6,18:26)] # 选择货币量与用户玩牌数据户的字段数据
(r3 <- round(cor(w3[,1:6],w3[,7:11]),2)) # 计算相关系数
corrplot(r3,method = "ellipse") # 绘制相关系数图
# 2. 降维及聚类分析
w.sc<-scale(w)   # 将数据标准化
w.pr<-princomp(w.sc) # 建立主成分分析模型
summary(w.pr) # 提取主成分信息
screeplot(w.pr,type="line") # 绘制主成分的碎石图
abline(h=1)
x<-data.frame(w.pr$loadings[,1:5]) 
dist.e=dist(x,method="euclidean")  #计算其欧氏距离矩阵
model1=hclust(dist.e,method="complete")   #建立系统聚类模型
plot(model1,main='聚类树图')          #画聚类树图

##### 11.2.3 模型构建
# 构建衍生指标
cadr <- round(w$纯消耗/w$游戏币发放,2)
smpc <- (w$沉睡保险箱结余+w$沉睡身上携带)/(w$累增用户-w$月活跃用户数)
afam <- (w$活跃保险箱余额+w$活跃身上携带)/w$月活跃用户数
slac <- round(smpc/afam,2)
unusual <- ifelse(w$日活跃.月活跃>0.2,"正常","异常")
u <- data.frame(cbind(cadr,slac,unusual))
u$cadr <- as.numeric(u$cadr)
u$slac <- as.numeric(u$slac)
# 建立决策树模型
library(rpart)
r<-rpart(unusual ~ .,data=u,method="class",parms=list(split="information"))
r
rattle::fancyRpartPlot(r)

#### 11.3.2 案例：付费用户RFM模型研究
# 导入RFM数据
rfm_data <- read.csv("RFM_data.csv")
# 将最后付费日期转换成日期格式
rfm_data$last_date <- paste(substr(rfm_data$last_date,1,4),substr(rfm_data$last_date,5,6),
                            substr(rfm_data$last_date,7,8),sep="/")
rfm_data$last_date <- as.Date(rfm_data$last_date,"%Y/%m/%d")
# 增加距离统计日的相隔天数
rfm_data$time_internal <- max(rfm_data$last_date)-rfm_data$last_date
# 查看数据的前六行
head(rfm_data)
# 查看不同指标的10%数据情况
quantile(rfm_data$time_internal,probs = seq(0,1,0.1)) 
quantile(rfm_data$pay_mnt,probs = seq(0,1,0.1))
quantile(rfm_data$pay_cnt,probs = seq(0,1,0.1))
# 建立新的衍生指标
rfm_data$tagR <- ifelse(rfm_data$time_internal <= 0,1,0)
rfm_data$tagF <- ifelse(rfm_data$pay_cnt <= 1,0,1 )
rfm_data$tagM <- ifelse(rfm_data$pay_mnt <= 6,0,1)
head(rfm_data)
# 给用户打上类型标签
rfm_data$type <- "一般挽留用户"
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==1,'type'] <- "重要保持客户"
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==0 & rfm_data$tagM==1,'type'] <- "重要发展客户"
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==1 & rfm_data$tagM==0,'type'] <- "一般保持客户"
rfm_data[rfm_data$tagR==1 & rfm_data$tagF==0 & rfm_data$tagM==0,'type'] <- "一般客户"
rfm_data[rfm_data$tagR==0 & rfm_data$tagF==1 & rfm_data$tagM==1,'type'] <- "重要挽留客户"
rfm_data[rfm_data$tagR==0 & rfm_data$tagF==1 & rfm_data$tagM==0,'type'] <- "一般客户"
rfm_data[rfm_data$tagR==0 & rfm_data$tagF==0 & rfm_data$tagM==0,'type'] <- "低价值客户"
head(rfm_data)
# 统计不同用户群的人数
library(sqldf)
rfm_data_sum <- sqldf("select type,count(distinct player_id) as usr_cnt,
                      sum(pay_mnt) as pay_mnt_sum from rfm_data group by type") 
# 按照付费金额进行降序排列
rfm_data_sum <- rfm_data_sum[order(rfm_data_sum$pay_mnt_sum,decreasing = T),]
rfm_data_sum$usr_cnt_rate <- paste0(round(rfm_data_sum$usr_cnt*100/
                                            sum(rfm_data_sum$usr_cnt),2),"%")
rfm_data_sum$pay_mnt_sum_rate <- paste0(round(rfm_data_sum$pay_mnt_sum*100/
                                                sum(rfm_data_sum$pay_mnt_sum),2),"%")
knitr::kable(rfm_data_sum)
# 绘制柱状图
library(RColorBrewer)
par(mfrow=c(2,1))
barplot(rfm_data_sum$pay_mnt_sum,col=brewer.pal(12,"Set3")[1:8],
        border = F,main="付费金额统计")
barplot(rfm_data_sum$usr_cnt,col = brewer.pal(9,"Set1")[1:8],
        border = F,names.arg = rfm_data_sum$type,
        main="付费人数统计")
par(mfrow=c(1,1))