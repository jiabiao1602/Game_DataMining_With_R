##  5.1 数据抽样
# 5.1.2 类失衡处理方法:SMOTE
# 利用Thyroid Disease 数据来研究
# 下载数据
hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data',
                 header=F)
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', 
                  header=F, sep='\t')[[1]]
# 对对象names删除冒号和句号
names <- gsub(pattern =":|[.]", replacement="", x = names)
# 对对象hyper的列进行重命名
colnames(hyper)<-names
colnames(hyper)
# 我们将第一列的列名从 hypothyroid, negative改成target，并将该列中的因子negative变成0，其他值变成1
colnames(hyper)[1]<-"target"
colnames(hyper)
hyper$target<-ifelse(hyper$target=="negative",0,1)
# 检查下0、1的结果
table(hyper$target)
prop.table(table(hyper$target))
# 利用SMOTE对类失衡问题进行处理
# 将变量target变成因子型
hyper$target <- as.factor(hyper$target)
# 加载DMwR包
if(!require(DMwR)) install.packages("DMwR")
# 进行类失衡处理
# perc.over=100:表示少数样本数=151+151*100%=302
# perc.under=200:表示多数样本数(新增少数样本数*200%=151*200%=302)
hyper_new <- SMOTE(target~.,hyper,perc.over = 100,perc.under = 200)
# 查看处理后变量target的0、1个数
table(hyper_new$target)
# perc.over=200:表示少数样本数=151+151*200%=453
# perc.under=300:表示多数样本数(新增少数样本数*300%=151*200%*300%=906)
hyper_new1 <- SMOTE(target~.,hyper,perc.over = 200,perc.under = 300)
# 查看处理后变量target的0、1个数
table(hyper_new1$target)


# 对活跃用户是否付费数据进行研究
# 导入数据
user <- read.csv("活跃用户是否付费数据.csv",T)
# 查看变量名
colnames(user)
# 查看是否付费的类别占比(0:非付费，1:付费)
prop.table(table(user$是否付费))
# 将是否付费变量转换成因子型
user$是否付费 <- as.factor(user$是否付费)
library(DMwR)
# 对类失衡数据进行处理
user_new <- SMOTE(是否付费~.,data=user,perc.over=100,perc.under=200)
# 查看处理后的结果
table(user_new$是否付费)

# 5.1.3 数据随机抽样
# sample小例子
set.seed(1234)
# 创建对象x，有1~10组成
x <- seq(1,10);x
# 利用sample函数对x进行无放回抽样
a <- sample(x,8,replace=FALSE);a
# 利用sample函数对x进行有放回抽样
b <- sample(x,8,replace=TRUE);b
# 当size大于x的长度
(c <- sample(x,15,replace = F))
(c <- sample(x,15,replace = T))

# 利用sample对活跃用户数据进行抽样
# 导入数据
user <- read.csv("活跃用户是否付费数据.csv",T)
# 查看数据user的行数
nrow(user)
# 利用sample函数对user数据进行无放回抽样
set.seed(1234)
# 提取下标集
index <- sample(nrow(user),10000,replace=TRUE)
# 将抽样数据赋予对象user_sample
user_sample <- user[index,]
# 查看user_sample的行数
nrow(user_sample)
# 现在我们分别查看user与user_sample变量“是否付费”中0、1占比。
round(prop.table(table(user$是否付费)),3)
round(prop.table(table(user_sample$是否付费)),3)
# 以下代码实现抽样后的“是否付费”的0、1占比不变
# 计算出“是否付费”中0的占比
rate <- sum(user$是否付费==0)/nrow(user)
# 提取未付费用户的下标子集
d <- 1:nrow(user)
index1 <- sample(d[user$是否付费==0],10000*rate)
# 提取付费用户的下标子集
index2 <- sample(d[user$是否付费==1],10000*(1-rate))
# 将抽样数据赋予对象user_sample1
user_sample1 <- user[c(index1,index2),]
# 查看“是否付费”的0、1占比
round(prop.table(table(user_sample1$是否付费)),3)

# 利用createDataPartition函数对数据进行抽样
# 对iris数据进行演示
# 载入caret包，如果本地未安装就进行在线安装caret包
if(!require(caret)) install.packages("caret")
# 提取下标集
splitindex <- createDataPartition(iris$Species,times=1,p=0.1,list=FALSE)
splitindex
# 提取符合子集
sample <- iris[splitindex,]
# 查看Species变量中各类别的个数和占比
table(sample$Species);
prop.table(table(sample$Species))
# 设置list为TRUE
# 提取下标集
splitindex1 <- createDataPartition(iris$Species,times=1,p=0.1,list=TRUE)
# 查看下标集
splitindex1
# 提取子集
iris[splitindex1$Resample1,]
# 设置times=2
splitindex2 <- createDataPartition(iris$Species,times=2,p=0.1,list=TRUE)
splitindex2
# 对12万本周活跃用户的数据按照“是否付费”的比例随机抽取1万的活跃用户进行探索性分析
# 导入数据
user <- read.csv("活跃用户是否付费数据.csv",T)
# 将“是否付费”改为因子型变量
user$是否付费 <- as.factor(user$是否付费)
# 提取下标集
ind <- createDataPartition(user$是否付费,p=10000/nrow(user),
                           times=1,list=FALSE)
# 查看子集中0、1占比
prop.table(table(user[ind,'是否付费']))

# 利用sample函数对数据分区
# 提取训练数据集的下标
ind <- sample(nrow(user),0.7*nrow(user),replace=F)
# 构建训练集数据
traindata <- user[ind,]
# 构建测试集数据
testdata <- user[-ind,]
# 查看“是否付费”的0、1占比
prop.table(table(user$是否付费))
prop.table(table(traindata$是否付费))
prop.table(table(testdata$是否付费))
# 利用createDataPartition函数按照”是否付费“等比例对数据进行分区
library(caret)
# 将”是否付费“变量转换成因子型
user$是否付费 <- as.factor(user$是否付费)
# 构建训练数据下标集
idx <-  createDataPartition(user$是否付费,p=0.7,list=FALSE)
# 构建训练数据集
train <- user[idx,]
# 构建测试数据集
test <- user[-idx,]
# 查看”是否付费“的0、1占比
prop.table(table(user$是否付费))
prop.table(table(train$是否付费))
prop.table(table(test$是否付费))

# 利用sample函数构建五折交叉验证的训练集和测试集
# zz1为所有观测值的下标
n <- nrow(user);zz1 <- 1:n 
# zz2为1:5的随机排列
set.seed(1234)
zz2 <- rep(1:5,ceiling(n/5))[1:n]
zz2 <- sample(zz2,n)
# 构建训练集及测试集
for(i in 1:5){
  m <- zz1[zz2==i]
  train <- user[-m,]
  test <- user[m,]
  # 接下来就可以利用训练集建立模型，测试集验证模型，并计算5次MSE
}

# 利用createFoldsh函数构建五折交叉验证的训练集和测试集
user$是否付费 <- as.factor(user$是否付费)
index <- createFolds(user$是否付费,k=5,list=FALSE)
prop.table(table(user[index==1,'是否付费']))
prop.table(table(user[index==2,'是否付费']))
prop.table(table(user[index==3,'是否付费']))
prop.table(table(user[index==4,'是否付费']))
prop.table(table(user[index==5,'是否付费']))

# 5.2 数据清洗
# 5.2.1 缺失值处理
# 导入玩家的玩牌游戏数据
player <- read.csv("玩家玩牌数据.csv",T,na.strings = "NA")
# 查看前六行
head(player)
# 利用is.na函数判断“玩牌局数”变量各值是否为缺失值
is.na(player$玩牌局数)
# 统计缺失值与非缺失值的个数
table(is.na(player$玩牌局数))
# sum()和mean()函数来统计缺失值的个数和占比
# 计算缺失值个数
sum(is.na(player$玩牌局数))
# 计算缺失值占比
mean(is.na(player$玩牌局数))
# 利用complete.cases函数查看完整实例
sum(complete.cases(player))

# 用md.pattern函数查看player的缺失值模式
if(!require(mice)) install.packages("mice")
md.pattern(player)

# 用aggr函数对player数据的缺失值模式进行可视化
if(!require(VIM)) install.packages("VIM")
aggr(player[,-1],prop=FALSE,numbers=TRUE)

# 删除缺失样本
player_full <- na.omit(player)
# 计算有缺失值的样本个数
sum(!complete.cases(player_full))

# 替换缺失值
iris1 <- iris[,c(1,5)]
# 将40、80、120号样本的Sepal.Length变量值设置为缺失值
iris1[c(40,80,120),1] <- NA
# 利用均值替换缺失值
iris1[c(40,80,120),1] <- round(mean(iris1$Sepal.Length,na.rm = T),1)
# 查看以前的值和现在的值
iris[c(40,80,120),1];iris1[c(40,80,120),1]
# 绘制箱线图
plot(iris$Sepal.Length~iris$Species,col=heat.colors(3))
# 利用同类均值进行赋值的方式来填补缺失值
# 将40、80、120号样本的Sepal.Length设置为缺失值
iris1[c(40,80,120),1] <- NA
iris1[40,1] <- round(mean(iris1[iris1$Species=='setosa','Sepal.Length'],
                          na.rm = T),1)
iris1[80,1] <- round(mean(iris1[iris1$Species=='versicolor','Sepal.Length'],
                          na.rm = T),1)
iris1[120,1] <- round(mean(iris1[iris1$Species=='virginica','Sepal.Length'],
                          na.rm = T),1)
# 查看以前的值和现在的值
iris[c(40,80,120),1];iris1[c(40,80,120),1]

# 对缺失值进行赋值
# 利用决策树对性别变量的缺失值进行赋值
# 导入玩家调研数据
questionnaire <- read.csv("问卷调研数据.csv",T)
# 查看问卷调研数据的行数和变量个数
dim(questionnaire)
# 对缺失值进行可视化展示
library(VIM)
aggr(questionnaire[,-1],prop=FALSE,numbers=TRUE)
# 把变量转换成因子型
str(questionnaire)
for(i in 2:ncol(questionnaire)){
  questionnaire[,i] <- as.factor(questionnaire[,i])
}
str(questionnaire)
# 对数据进行分区
train <- na.omit(questionnaire[,c("性别","职业" ,"学历","玩家游戏情况","游戏进入","游戏偏好")])
test <- questionnaire[is.na(questionnaire$性别),c("职业" ,"学历","玩家游戏情况","游戏进入","游戏偏好")]
# 建立logit回归模型
fit <- glm(性别~.,train,family = "binomial")
# 由于拟合结果是给每个观测值一个概率值，下面以0.5作为分类界限：
result <- predict(fit,test,type = "response")<0.5
# 把预测结果转换成原先的值(1或2)
z=rep(1,nrow(test));z[!result]=2 
# 在test集中增加预测的性别变量值
test_new <- cbind('性别'=z,test)
# 查看前六行数据
head(test_new)
# 利用随机森林迭代弥补缺失值的方法进行赋值
rm(list=ls())
# 导入数据
questionnaire <- read.csv("问卷调研数据.csv",T)
# 把变量转换成因子型
str(questionnaire)
for(i in 2:ncol(questionnaire)){
  questionnaire[,i] <- as.factor(questionnaire[,i])
}
# 取前10000行样本进行演示
test <- questionnaire[1:10000,]
library(mice)
md.pattern(test)
# 利用missForest进行缺失值赋值
if(!require(missForest)) install.packages("missForest")
z <- missForest(test)
test.full <- z$ximp
md.pattern(test.full)

# 5.2.2 异常值判断处理
# 3σ原则
# 绘制质量控制图
set.seed(1234)
data <- rnorm(20)
plot(data,type = "l",lwd=1.5,xlab = NA,ylab = NA,
     ylim = c(-4,4),xlim = c(0,23),main="质量控制图")
lines(rep(mean(data),20),lwd=1.8);text(21,mean(data),"均值线")
lines(rep(mean(data)-3*sd(data),20),lty=2,col="red",lwd=1.8)
text(21,mean(data)-3*sd(data),labels = "控制下限",col="red")
lines(rep(mean(data)+3*sd(data),20),lty=2,col="red",lwd=1.8)
text(21,mean(data)+3*sd(data),labels = "控制上限",col="red")

# P质量控制图
# 导入数据
dailydata <- read.csv("每日付费及留存数据.csv",T)
# 查看前六行
head(dailydata)
# 绘制付费率的单值-均值质量控制图
library(qcc)
attach(dailydata)
qcc(七日留存率,type="xbar.one",labels= 日期,
         title="新增用户第7日留存率的单值-均值质量监控图",
         xlab="date",ylab="第七日留存率")

# 通过boxplot.stat()函数识别异常值
boxplot.stats(七日留存率)
# 查找异常值的下标
idx <- which(七日留存率 %in% boxplot.stats(七日留存率)$out)
# 查看异常值的下标集
idx
# 绘制箱线图
boxplot(七日留存率,col='violet')
# 通过text函数把异常值的日期和数值在图上显示
text(1.1,boxplot.stats(七日留存率)$out,
     labels=paste(dailydata[idx,'日期'],dailydata[idx,'七日留存率']),
     col="darkgreen")

# 通过聚类进行异常检测
# 导入棋牌游戏玩家的样本数据
w <- read.csv("玩家玩牌数据样本.csv",T)
# 查看数据对象w的前六行
head(w)
# w各变量的量纲不是处于同一水平，接下来进行归一化处理
u <- round(apply(w[,-1],2,function(x) (x-min(x))/(max(x)-min(x))),4)
# 将u变成data.frame形式
u <- data.frame(u)
# 将用户ID赋予对象u的行号
row.names(u) <- w$用户id
# 查看u的前六行
head(u)
# 利用K-Means聚类对数据u进行分群，k选择为3
kmeans.result <- kmeans(u,3)
# 查看聚类结果
kmeans.result
# 找出距离最大的5个玩家
centers <- kmeans.result$centers[kmeans.result$cluster,]
distances <- sqrt(rowSums((u-centers)^2))
outliers <- order(distances,decreasing = T)[1:5]
# 打印出距离最大的5个玩家的行号
print(outliers)
# 打印出异常玩家的用户ID
rownames(u[outliers,])
# 对结果进行可视化展示

# 绘制135位玩家的散点图
plot(u$玩牌局数,u$正常牌局,pch=kmeans.result$cluster,
     axes=F,xlab="玩牌局数",ylab="正常牌局")
axis(1,labels = F);axis(2,labels = F)
# 绘制类中心点
points(kmeans.result$centers[,c('玩牌局数','正常牌局')],pch=16,cex=1.5)
# 绘制离群点
points(u[outliers,c('玩牌局数','正常牌局')],pch="*",col=4,cex=1.5)
# 把离群点的用户ID号打印出来
text(u[outliers,c('玩牌局数','正常牌局')],
     labels=rownames(u[outliers,]),
     cex=1,col="black")

###  5.3 数据转换
# 产生衍生变量
# 5.3.1 导入数据
rawdata <- read.csv("数据转换数据.csv",na.strings = NA)
# 查看数据的前六行
head(rawdata)
# 将注册日期变量转换成日期格式
rawdata$registration <- as.Date(paste(substr(rawdata$registration,1,4),
                                      substr(rawdata$registration,5,6),
                                      substr(rawdata$registration,7,8),
                                      sep="/"),
                                "%Y/%m/%d")
# 将首次付费日期转换成日期格式
rawdata$firstpaydate <- as.Date(paste(substr(rawdata$firstpaydate,1,4),
                                      substr(rawdata$firstpaydate,5,6),
                                      substr(rawdata$firstpaydate,7,8),
                                      sep="/"),
                                "%Y/%m/%d")
# 查看数据的前六行
head(rawdata)
# 增加ispay变量：0表示非付费用户，1表示付费用户
rawdata$ispay <- ifelse(!is.na(rawdata$firstpaydate),1,0)
# 增加isnewpay变量：0表示非新增首日付费用户，1表示新增首日付费用户
rawdata$isnewpay <- ifelse(rawdata$registration==rawdata$firstpaydate,
                           1,0)
rawdata[is.na(rawdata$isnewpay),'isnewpay'] <- 0
# 查看数据前10行
head(rawdata)

# 5.3.2 数据分箱
# 利用cut函数对数据进行分箱
# 对days(活跃天数)进行分箱操作
rawdata$days_interval <- cut(rawdata$days,
                             breaks=c(0,30,60,90,Inf),
                             labels=c('一个月内','31~60天','61~90天','三个月以上'))
# 对lifetime(生命周期)进行分箱操作
rawdata$lifetime_interval <- cut(rawdata$lifetime,
                                 breaks=c(0,7,21,30,90,Inf),
                                 labels=c('小于一周','小于两周','小于一个月',
                                          '小于三个月','三个月以上'))
# 查看前六行
head(rawdata)

# 5.3.3 数据标准化变换
#采用(x-mu)/std的标准化方法，与scale()函数效果一样
standard <- preProcess(iris)  
head(predict(standard,iris))
head(scale(iris[,1:4]))
#采用(x-min(x))/(max(x)-min(x))的标准化方法
standard <- preProcess(iris, method = 'range')  
head(predict(standard,iris))
fun <- function(x) (x-min(x))/(max(x)-min(x))
head(sapply(iris[,1:4],fun))


# 5.4 数据哑变量处理
# 构建customers数据集
customers<-data.frame(id=c(10,20,30,40,50),
                      gender=c("male","female","female","male","female"),
                      mood=c("happy","sad","happy","sad","happy"),
                      outcome=c(1,1,0,0,0))
customers
# 对因子型变量进行哑变量处理
# 创建新数据框customers.new
customers.new <- customers[,c('id','outcome')]
# 对gender变量进行哑变量处理
customers.new$gender.male <- ifelse(customers$gender=='male',1,0)
customers.new$gender.female <- ifelse(customers$gender=='female',1,0)
# 对mood变量进行哑变量处理
customers.new$mood.happy <- ifelse(customers$mood=='happy',1,0)
customers.new$mood.sad <- ifelse(customers$mood=='sad',1,0)
customers.new

# 加载caret包到内存
library(caret)
# 查看customers的数据结构
str(customers)
# 利用dummyVars函数对customers数据进行哑变量处理
dmy<-dummyVars(~.,data=customers)
# 对自身变量进行预测，并转换成data.frame格式
trsf<-data.frame(predict(dmy,newdata=customers))
# 查看转换结果
trsf
# 将outcome变量转换成因子型变量
customers$outcome <- as.factor(customers$outcome)
# 利用dummyVars函数对customers数据进行哑变量处理
dmy<-dummyVars(~.,data=customers)
# 对自身变量进行预测，并转换成data.frame格式
trsf<-data.frame(predict(dmy,newdata=customers))
# 查看转换结果
trsf
# 只对gender变量进行哑变量转换
dmy.gender <- dummyVars(~gender,data=customers)
trsf.gender <- data.frame(predict(dmy.gender,newdata=customers))
trsf.gender
# 将levelsOnly和fullRank设置为TRUE
customers<-data.frame(id=c(10,20,30,40,50),
                      gender=c("male","female","female","male","female"),
                      mood=c("happy","sad","happy","sad","happy"),
                      outcome=c(1,1,0,0,0))
dmy<-dummyVars(~.,data=customers,levelsOnly=TRUE,fullRank=TRUE)
trsf<-data.frame(predict(dmy,newdata=customers))
trsf

# 导入用户活跃时间段数据
load("loginhour.RData")
dim(loginhour)
# 查看列名
colnames(loginhour)
# 利用dummyVars函数loginhour数据进行哑变量处理
library(caret)
dmy<-dummyVars(~.,data=loginhour)
# 对自身变量进行预测，并转换成data.frame格式
trsf<-data.frame(predict(dmy,newdata=loginhour))
# 查看转换后的维度
dim(trsf)
# 查看转换后的列名
colnames(trsf)