# 6.1.2 双指标数据可视化
# 绘制散点图
w <- read.csv("可视化数据.csv",T)
plot(w$活跃用户~w$新增用户,col="violetred2",pch=16,
     main="活跃用户 vs 新增用户散点图")
# 添加线性拟合直线
abline(lm(w$活跃用户~w$新增用户),col="blue",lwd=2,lty=2)

# 6.1.3 三指标数据可视化
# 绘制气泡图
plot(w$活跃用户~w$新增用户,col="violetred2",pch=16,
     cex=w$付费率,
     main="活跃用户 vs 新增用户气泡图")

# 6.2.3 时间序列数据预测
revenue <- read.csv("收入数据.csv",T) #导入数据
head(revenue)                         #查看前六行
revenue.ts <- ts(revenue[,2],frequency = 12,start = c(2014,1)) # 转换成时间序列对象
is.ts(revenue.ts)                    #判断是否为时间序列对象
start(revenue.ts)                    #查看开始日期
end(revenue.ts)                      #查看结束日期
frequency(revenue.ts)                #查看一个周期的频数
# 绘制时序图
plot.ts(revenue.ts,xlab="时间",ylab="收入")
abline(lm(revenue.ts~time(revenue.ts)),col="red",lty=2,lwd=2)
# 使用单位根检验方法验证其平稳性
library(fUnitRoots)
unitrootTest(revenue.ts)
# 进行一阶差分运算
revenue.ts.dif <- diff(revenue.ts)
# 验证其平稳性
unitrootTest(revenue.ts.dif)
# 对时间序列对象进行拆解
revenue.ts.decompose <- decompose(revenue.ts)
plot(revenue.ts.decompose)
revenue.ts.decompose$seasonal 
# 利用HoltWinters函数对数据进行建模
HoltWintersModel <-  HoltWinters(revenue.ts,alpha=TRUE,beta=TRUE,gamma=TRUE)
HoltWintersModel
plot(HoltWintersModel,lty= 1,lty.predicted = 2)
# 利用forecast.HoltWinters函数对未来6个月进行预测
library(forecast)
HoltWintersForecast <- forecast.HoltWinters(HoltWintersModel,h=6,level=c(80,95))
HoltWintersForecast
# 对残差进行检验
acf(HoltWintersForecast$residuals,lag.max = 20)
# 绘制模拟预测图
plot(HoltWintersForecast)

# 利用auto.arima函数建立预测模型
library(forecast)
fit <- auto.arima(revenue.ts)
fit
fit.forecast <- forecast(fit,h=6)
fit.forecast
plot(fit.forecast)

## 6.3 游戏数据相关分析
# 6.3.1 相关分析基本原理
# 计算相关系数
data(sleep,package = "VIM")
round(cor(sleep,use="complete.obs"),3)
# 对BodyWgt与BrainWgt的相关显著性进行检验
sleepClean <- na.omit(sleep)
attach(sleepClean)
cor.test(BodyWgt,BrainWgt)
# 利用corr.test函数计算相关矩阵和显著性检验
library(psych)
corr.test(sleepClean)

# 6.3.2 相关系数可视化
# 利用corrgram函数绘制相关系数图
library(corrgram)
corrgram(sleepClean,order=T,lower.panel=panel.shade,
         upper.panel=panel.pie,text.panel=panel.txt,
         main="利用corrgram函数绘制相关系数图")
corrgram(sleepClean,order=T,lower.panel=panel.cor,
         upper.panel=panel.pie,text.panel=panel.txt,
         main="利用corrgram函数绘制相关系数图")
# 利用corrplot包绘制相关系数图
library(corrplot)
M <- cor(sleepClean)
corrplot(M,main="利用corrplot函数绘制相关系数图")
corrplot.mixed(M, lower = "ellipse", upper = "number",
               tl.pos="lt",diag="u") 

# 6.3.3 活跃时间段相关分析
# 导入登陆数据
logindata <- read.csv("data\\logindata.csv")
str(logindata)
dim(logindata)
library(caret)
dmy<-dummyVars(~.,data=logindata)
dmyTsrf<-data.frame(predict(dmy,newdata=logindata))
dim(dmyTsrf)
str(dmyTsrf)
# 导入自定义的求相关系数函数
source("code//CorrelationFunction.R")
corMasterList<-flattenSquareMatrix(cor.prob(dmyTsrf))
# 按照相关系数的绝对值进行降序排序
corList<-corMasterList[order(-abs(corMasterList$cor)),]
print(head(corList,10))
# 提取与 "是否付费.是"的相关系数大于0.04的记录
selectedSub <- subset(corList,(abs(cor)>0.04 & i %in%  c('是否付费.是')))
bestsub <- as.character(selectedSub$j)
# 绘制相关系数图
library(corrplot)
corrplot.mixed(cor(dmyTsrf[,c('是否付费.是',bestsub)]),
         lower = "ellipse", upper = "number",
         tl.pos="lt",diag="u")

# 6.4.3 玩家偏好对应分析
w <- read.csv("玩家喜好分析数据.csv",T) # 导入不同类别玩家的购物数据
rownames(w) <- w[,1]  #将道具名称赋予行名称
library(ca)  
w.ca <- ca(w[,-1])    # 建立简单对应分析
w.ca                  # 查看结果
names(w.ca)           # 查看因子分析输出的对象列表
w.ca$colnames         # 查看列名称
w.ca$colcoord         # 查看列的标准坐标
# 绘制对应分析的散点图
plot(w.ca$rowcoord[,1],w.ca$rowcoord[,2],pch = 16,col = rgb(0.6,0.3,0.2),
     xlim = c(min(w.ca$rowcoord[,1]),max(w.ca$rowcoord[,1])+0.3),
     main = "玩家购买物品偏好分析")
text(w.ca$rowcoord[,1],w.ca$rowcoord[,2],labels = w.ca$rownames,
     cex = 0.8,pos = 4,col = rgb(0.6,0.3,0.2))
points(w.ca$colcoord[,1],w.ca$colcoord[,2],pch = 17,col = "#007e7e")
text(w.ca$colcoord[,1],w.ca$colcoord[,2],labels = w.ca$colnames,
     cex = 0.8,pos = 4,col = "#007e7e")
