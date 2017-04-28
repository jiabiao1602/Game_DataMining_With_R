### 7.2 漏斗模型
# 7.2.2 分析案例—新手教程漏斗模型
# 导入新手教程关键路径用户数 
funnel <- read.csv("新手教程路径留存人数统计.csv",T)
# 查看funnel数据集
funnel
# 绘制漏斗图
# 对数据进行降序排序
funnel_order <- funnel[order(funnel$用户数),]
# 绘制漏斗图
barplot(funnel_order$用户数,horiz = T,axes=F,border=F,space=0.5,
        col="steelblue1",xlim=c(-700,700),
        main="新手教程关键路径漏斗图")
barplot(-funnel_order$用户数,horiz=T,add=T,border=F,space=0.5,
        axes=F,col="steelblue1")
# 增加步骤名称
text(x=rep(-600,6),y=seq(1,8.5,length.out = 6),
     labels=funnel_order$事件行为,cex=0.8,font=3,col="black")
# 增加总体的转化率
text(x=rep(0,6),y=seq(1,8.5,length.out = 6),
     labels=paste0(round(funnel_order$用户数*100/max(funnel_order$用户数),2),"%"),
     cex=0.8,font=3,col="violetred3")
# 增加上一步的转化率
text(x=rep(max(funnel$用户数),6),y=seq(2,7.5,length.out = 5),
     labels=paste0(round((1-(-diff(funnel$用户数)/funnel$用户数[1:5]))*100,2),"%"),
     cex=0.8,font=4,col="red3")

# 7.3.2 路径分析的主要算法
# sunburst事件路径图
# 加载sunburstR包
if(!require(sunburstR)) devtools::install_github("timelyportfolio/sunburstR")
# 导入sequences数据
sequences <- read.csv(
  system.file("examples/visit-sequences.csv",package="sunburstR")
  ,header = FALSE
  ,stringsAsFactors = FALSE
)
# 查看前六行
head(sequences)
# 绘制sunburst事件路径图
sunburst(sequences)
# 计算某一路径的占比
# 查看选中路径的点击次数
(targetclick <- sequences[sequences$V1=="home-search-product-end","V2"])
# 统计所有路径的点击次数
(allclick <- sum(sequences$V2))
# 计算目标路径的占比
paste0(round(targetclick/allclick,4)*100,"%")

# 7.3.3 分析案例—游戏点击事件路径分析
data_click <- read.csv("data_click.csv",header=F) # 导入点击事件数据
head(data_click) # 查看前六行
# 提取有过11034点击行为的用户数据
targetuserid <- unique(data_click[data_click$V3=="11034","V1"])
data_click_new <- data_click[data_click$V1 %in% targetuserid,]
# 将数据转化成事务型
library(arulesSequences)
tmp_data <- data.frame(click=data_click_new$V3)
tmp_data$click <- as.factor(tmp_data$click)
data_click_tran <- as(tmp_data,'transactions')
transactionInfo(data_click_tran)$sequenceID <- data_click_new$V1
transactionInfo(data_click_tran)$eventID<-data_click_new$V2
data_click_tran
summary(data_click_tran)
# 查看事务型数据的前六行
inspect(data_click_tran[1:6])
head(as(data_click_tran,"data.frame"))
# 绘制商品的频率图
itemFrequencyPlot(data_click_tran, topN=20)
# 利用arulesSquences包中的cspade函数实现cSPADE算法
myrules <-  cspade(data_click_tran,parameter=list(support=0,maxlen=2),
                   control=list(verbose=TRUE))
myrules <- sort(myrules,by="support") # 按照support进行排序
targetclick <- paste0(".*click=11034","[^\\}]*\\}>") # 设置规则表达式
finalrules <-myrules[grep(targetclick ,as(myrules,"data.frame")$sequence)]
inspect(finalrules[1:3]); # 查看序列的前三条
nrow(finalrules) # 计算序列个数 
#转换成数据框 
finalrules.data.frame <- as(finalrules[-1],"data.frame") 
# 查看前三行数据
head(finalrules.data.frame) 
# 计算支持度的占比
finalrules.data.frame$percentage <- finalrules.data.frame$support/
  sum(finalrules.data.frame$support) 
# 计算支持度的累计百分比
finalrules.data.frame$sum.percentage <- cumsum(finalrules.data.frame$percentage) 
# 筛选累计百分比小于75%的序列数据
finalrules.data.frame <- finalrules.data.frame[
  finalrules.data.frame$sum.percentage <=0.75,]
# 查看符合结果的序列个数
nrow(finalrules.data.frame)
# 查看前六行数据
head(finalrules.data.frame)
# 提取关键点击按钮的事件id
clickid <- substr(finalrules.data.frame$sequence,9,13)
# 查看关键事件id
clickid
# 计算关键点击按钮i的引导能力conf
conf <- rep(1,length(clickid))
for(i in 1:length(clickid)) {
  n <- myrules@info$nsequences 
  nclickid_support <- finalrules.data.frame[i,"support"]
  conf[i] <- nclickid_support*n/
    nrow(data_click[data_click$V3==clickid[i],])
}

# 将关键事件id和支持度占比组成新数据框result
result <- data.frame(click=clickid,
                     percentage=round(finalrules.data.frame$percentage,3),
                     conf=conf)
# 查看前六行
head(result)

# 绘制支持度占比的垂直金子塔图
library(reshape)
md <- melt(result,id="click") # 对result数据进行重组
md$value[md$variable == "conf"] <- -md$value[md$variable == "conf"] 
md <- md[order(md$variable,decreasing=T),] # 按照variable变量进行降序排序
# 绘制垂直金字塔图
library(recharts)
echartr(md,click,value,variable,type="vbar",subtype="stack") %>%
  setTitle("引导用户进入开始打牌11034的重点事件id分析") %>%
  setXAxis(axisLine=list(onZero=TRUE)) %>%
  setYAxis(axisLabel=list(
    formatter=JS('function (value) {return Math.abs(value);}')))

