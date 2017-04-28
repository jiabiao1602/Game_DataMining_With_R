##### 10.1  ####
# 绘制气泡图
channeldata <- read.csv("data_10.1.csv",T)
# 选取新增用户、ARPPU、次日留存率这三列数据
channeldata1 <- channeldata[,c(1,3,5,6)]
# 设置不同渠道的颜色
library(RColorBrewer)
col <- brewer.pal(11,"Spectral")[2:11]
# 设置气泡的最大值和最小值
cex.max <- 10;
cex.min <- 1 
# 设置散点大小
a <- (cex.max-cex.min)/(max(channeldata1$ARPPU)-min(channeldata1$ARPPU))
b <- cex.min-a*min(channeldata1$ARPPU)
cex <- a*channeldata1$ARPPU+b
# 绘制气泡图
attach(channeldata1)
plot(新增用户,次日留存率,cex=cex,col=col,pch=20,
         main="各渠道新增用户-次日留存率-ARPPU",
         xlab="日均新增用户",
         ylab="新增用户次日留存率")
text(新增用户,次日留存率,渠道名称,col="black")
detach(channeldata1)


#### 10.2 ####
# 渠道LTV监控
# 导入渠道日运营数据
channel <- read.csv("渠道日概况.csv",header=TRUE,fill=TRUE)
# 对新增用户、活跃用户、付费用户分渠道绘制箱线图
par(mfrow=c(3,1))
boxplot(新增用户~渠道名称,data=channel,ylab="新增用户",
            col=colorspace::rainbow_hcl(10),varwidth=TRUE)
boxplot(活跃用户~渠道名称,data=channel,ylab="活跃用户",
            col=colorspace::heat_hcl(10),varwidth=TRUE)
boxplot(付费用户~渠道名称,data=channel,ylab="付费用户",
            col=colorspace::terrain_hcl(10),varwidth=TRUE)
par(mfrow=c(1,1))
# 分渠道绘制一月收入曲线图
# 将日期变量转换为日期格式
channel$日期 <- as.Date(channel$日期)
# 绘制分面板曲线图
library(lattice)
xyplot(收入~日期|渠道名称,data=channel,type="l",
         lwd=2,layout=c(5,2),
         xlim=c(min(channel$日期)-1,max(channel$日期)+1),
         main="渠道收入一月曲线图")
# 利用Median-IQR方法分析ARPPU、ARPU、新增次日留存率和新增七日留存率
# 自定义求中位数和四分位距函数
mystats <- function(x) c(Median=median(x,na.rm=T),IQR=IQR(x,na.rm=T))
# 求出各指标的统计值
library(doBy)
result <- summaryBy(ARPPU+ARPU+新增次日留存率+新增7日后留存率~渠道名称,
          data=channel,FUN = mystats)
result
# 对各指标绘制四象限图，查看渠道分布情况
# 将需要绘制四象限图的指标赋予name对象
name <- c("ARPPU","ARPU","新增次日留存率","新增7日留存率")
# 利用for循环绘制四个四象限分布图
par(mfrow = c(2,2))
for(i in 1:4){
  plot(result[,2*i],result[,2*i+1],type = "n",
       xlim=c(0.95*min(result[,2*i]),1.05*max(result[,2*i])), # 设置横轴坐标轴范围
       ylim=c(0.95*min(result[,2*i+1]),1.05*max(result[,2*i+1])), #设置纵轴坐标轴范围
       main = paste0(name[i],"四象限图"), 
       xlab = "中位数值",ylab = "四分位距值") #绘制散点图 
  abline(v = mean(result[,2*i]),lty = 2,col = "green") # 添加垂直直线
  abline(h = mean(result[,2*i+1]),lty = 2,col = "blue")  # 添加水平直线
  text(result[,2*i],result[,2*i+1],result[,1],
       col = "black",cex = 0.8,font=2) # 在图中打印出渠道名称
}
par(mfrow=c(1,1))

# 建立LTV监控体系
# 导入渠道用户LTV统计数据
LTV <- read.csv("渠道用户LTV统计.csv",header = T)
head(LTV)
# 对31日的渠道A LTV数据绘制条形图
data1 <- LTV[LTV$日期=='2016/1/31' & LTV$渠道名称=="渠道A",4:12] # 提取数据
# 绘制渠道A用户在不同时期的LTV柱状图
barplot(as.matrix(data1),col = "grey",border = F,
        main="渠道A用户生命周期价值",xlab="不同生命周期价值")
# 添加LTV值
text(seq(0.6,10.4,length.out = 9),as.matrix(data1)/2,
     labels = as.matrix(data1),
     col="black",font = 2)
# 绘制点图
x <- LTV[LTV$日期=='2016/1/31',c(4,6,10:12)] 
rownames(x) <- LTV[LTV$日期=='2016/1/1',2]   
x <- as.matrix(x)
library(lattice)
dotplot(x,groups = FALSE,layout = c(1,5),
        aspect=0.5,origin=0,type=c("p","h"),
        main="不同渠道的LTV对比",
        xlab="LTV")

# 渠道新增用户留存监控
# 导入7日留存率明细数据
retention <- read.csv("7日留存率明细.csv",header = T)
head(retention)
# 对渠道A做7日留存率的正态性检验
shapiro.test(retention[retention$渠道名称=="渠道A","第7日留存率"])
# 对渠道H做7日留存率的正态性检验
shapiro.test(retention[retention$渠道名称=="渠道H","第7日留存率"])

# 绘制QQ图
par(mfrow=c(1,2))
# 绘制渠道A的QQ图
qqnorm(retention[retention$渠道名称=="渠道A","第7日留存率"],
       pch=7,
       main="Channel A Normal Q-Q plot")
qqline(retention[retention$渠道名称=="渠道A","第7日留存率"],
       col="red",lty=2)
# 绘制渠道H的QQ图
qqnorm(retention[retention$渠道名称=="渠道H","第7日留存率"],
       pch=7,
       main="Channel H Normal Q-Q plot")
qqline(retention[retention$渠道名称=="渠道H","第7日留存率"],
       col="red",lty=2)
par(mfrow=c(1,1))

# 绘制P质量监控图
# 导入渠道H新增用户在第7日的留存统计数据
residentuser <- read.csv("渠道H新增第7日留存用户统计.csv",header=T)
# 绘制渠道A的7日留存率质量监控图
library(qcc)
residentuser$date <- as.Date(residentuser$date)
attach(residentuser)
sol <- qcc(actionuser,newuser,labels=date,
           type="p",nsigmas = 3,
           title="渠道H新增第7日留存率质量监控图",
           xlab="date",ylab="7日留存率")
detach(residentuser)

#### 8.3 ####
# 渠道用户质量打分模型
# 自定义channel_score( )实现指标打分模型
# 渠道得分函数
channel_score <- function(data,amount=T){
  
  # 进行指标的波动性打分  
  library(reshape)
  data <- cast(data,渠道名称~自然周)
  # 利用apply函数分渠道求出当前周与上周的差值
  x <- t(apply(data[,-1],1,diff))
  # 利用as.data.frame函将x转换成数据框形式
  x <- as.data.frame(x,row.names = as.character(data[,1]))
  # 利用colnames函数对x列名重新赋值
  colnames(x) <-  colnames(data[3:ncol(data)])
  # 找出最近四周的最大值
  # 自定义函数mystat求最近四周的最大值
  mystat <- function(x){
    m <- rep(0,(ncol(data)-1))
    for(i in 1:(ncol(data)-1)){
      if(i <=3){
        m[i] <- max(x[1:i])
      } else {
        m[i] <- max(x[(i-3):i])
      }
    }
    return(m)
  }
  # 利用apply函按分渠道求最近四周最大值
  y <- t(apply(data[,-1],1,mystat))
  # 利用as.data.frame函数将y转换成数据框形式
  y <- as.data.frame(y,row.names = as.character(data[,1]))
  # 利用colnames函数对y列名重新赋值
  colnames(y) <-  colnames(data[2:ncol(data)])
  # 计算波动变化得分
  reliability_score <- 5*round(x/y[,-1],3)
  reliability_score
  if(amount) {
    # 进行指标的量级打分
    # 利用colSums函数进行按列求和
    x <- colSums(data[,-1])
    x <- as.data.frame(matrix(rep(x,nrow(data)),nrow = nrow(data),byrow = T))
    amount_score <- 5*round(data[,-1]/x,3)
    rownames(amount_score) <- rownames(reliability_score)
    amount_score
    
    # 计算指标得分
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum+amount_score[,-1] #起始分+波动变化得分+量级得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  } else {
    # 利用cumsum函数进行对波动变化值进行累积求和
    rs_cumsum <- apply(reliability_score,1,cumsum)
    rs_cumsum <- as.data.frame(t(rs_cumsum))
    score <- 10+rs_cumsum #起始分+波动变化得分
    score[,colnames(data)[2]] <- 10
    score <- score[,c(ncol(score),1:ncol(score)-1)] # 改变列的顺序
  }
  return(score)
}
# 导入周渠道概况数据
load("rawdata.RData")
str(rawdata)
# 对字段自然周转化成有序因子变量
levels <- c(paste0("第",1:length(unique(rawdata$自然周)),"周"))
rawdata$自然周 <- factor(rawdata$自然周,
                      levels = levels,
                      ordered = T)
str(rawdata)
# 对游戏A的渠道得分进行计算
# 计算周收入得分
channel_revenue <- channel_score(rawdata[rawdata$游戏名称=="游戏A",
                                         c("渠道名称","自然周","周收入")])
# 计算周活跃用户得分
channel_active <- channel_score(rawdata[rawdata$游戏名称=="游戏A",
                                         c("渠道名称","自然周","周活跃用户数")])
# 计算第7日留存率得分
channel_newretation <- channel_score(rawdata[rawdata$游戏名称=="游戏A",
                                        c("渠道名称","自然周","第7日留存率")],amount = F)
# 计算周付费率得分
channel_payrate <- channel_score(rawdata[rawdata$游戏名称=="游戏A",
                                             c("渠道名称","自然周","周付费率")],amount = F)
# # 计算ARPU得分
channel_arppu <- channel_score(rawdata[rawdata$游戏名称=="游戏A",
                                         c("渠道名称","自然周","周ARPPU")],amount = F)
# 计算综合得分
channel_total <- round(0.3*channel_revenue+0.2*channel_active+0.2*channel_payrate+
                         0.15*channel_arppu+0.15*channel_newretation,2)
# 查看综合得分结果
channel_total
# 绘制游戏A在各渠道的生存曲线
channel_total$渠道名称 <- rownames(channel_total) # 增加渠道名称变量
library(reshape)
md <- melt(channel_total,id="渠道名称") #对数据进行重塑
md$week <- ifelse(nchar(as.character(md$variable))==4,
                  substr(md$variable,2,3),substr(md$variable,2,2)) #增加周数变量
md$week <- as.numeric(md$week)
library(lattice)
xyplot(value~week|渠道名称,data=md,type=c("l","g"),
       lwd=2,layout=c(6,2)) #绘制面板曲线图