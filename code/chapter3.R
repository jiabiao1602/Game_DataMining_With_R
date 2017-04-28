## 3.1 常用图形参数
# 3.1.1 颜色
# 对women数据集绘制散点图，并用红色表示散点。
plot(women,col="red") # 通过颜色名称
plot(women,col=554)   # 通过颜色下标
plot(women,col="#FF0000") #通过十六进制的颜色值
mycolor <- rgb(red=255,green=0,blue=0,max=255)
plot(women,col=mycolor) # 通过RGB值
# 对其他图形参数颜色进行设置
plot(women,main="身高 VS 体重 散点图",sub="数据来源：women数据集",
     col="red",col.main="green",col.sub="blue",
     col.axis="grey",col.lab="yellow")
colors()
# 主题颜色
par(mfrow=c(3,2))
barplot(rep(1,7),col=rainbow(7),main="barplot(rep(1,7),col=rainbow(7))")
barplot(rep(1,7),col=heat.colors(7),main="barplot(rep(1,7),col=heat.colors(7))")
barplot(rep(1,7),col=terrain.colors(7),main="barplot(rep(1,7),col=terrain.colors(7))")
barplot(rep(1,7),col=topo.colors(7),main="barplot(rep(1,7),col=topo.colors(7))")
barplot(rep(1,7),col=cm.colors(7),main="barplot(rep(1,7),col=cm.colors(7))")
par(mfrow=c(1,1))

# 3.1.2 文字元素
# 字体
plot(0:4,type="n",axes = F,xlab = NA,ylab = NA)
type <- c("正常字体(默认)","粗体字体","斜体字体","粗斜体字体")
for(i in 1:4){
  text(2,5-i,labels = paste0("font=",i,":",type[i]),font = i)
}
# 大小
plot(0:5,type="n",axes = F,xlab = NA,ylab = NA)
text(2,5,labels="cex=0.5:放大0.5倍",cex=0.5)
text(2,4,labels="cex=0.8:放大0.8倍",cex=0.8)
text(2,3,labels="cex=1(默认):正常大小",cex=1)
text(2,2,labels="cex=1.5:放大1.5倍",cex=1.5)
text(2,1,labels="cex=2:放大2倍",cex=2)

# 3.1.3 点元素
plot(1,col="white",xlim=c(1,7),ylim=c(1,5),
     main = "点样式 pch=",xlab=NA,ylab=NA)
for(i in c(0:25)){
  x<-(i %/% 5)*1+1
  y<-6-(i%%5)-1
  if(length(which(c(21:25)==i)>=1)){
    points(x,y,pch=i,col="blue",bg="yellow",cex=2)
  } else {
    points(x,y,pch=i,cex=2)
  }
  text(x+0.2,y,labels = i)
}
# pch取值可以为"*，？，a，A，0，.，+，-，|"
points(6,4,pch="*",cex=2);text(6+0.2,4,labels="\"*\"")
points(6,3,pch="?",cex=2);text(6+0.2,3,labels="\"?\"")
points(6,2,pch="a",cex=2);text(6+0.2,2,labels="\"a\"")
points(6,1,pch="A",cex=2);text(6+0.2,1,labels="\"A\"")
points(7,5,pch="0",cex=2);text(7+0.2,5,labels="\"0\"")
points(7,4,pch=".",cex=2);text(7+0.2,4,labels="\".\"")
points(7,3,pch="+",cex=2);text(7+0.2,3,labels="\"+\"")
points(7,2,pch="-",cex=2);text(7+0.2,2,labels="\"-\"")
points(7,1,pch="|",cex=2);text(7+0.2,1,labels="\"|\"")

# 3.1.4 线元素
plot(x=1:10,y=rep(1,10),type="l",lty=0,ylim=c(1,8),xlim=c(-1,10),
     axes=F,xlab=NA,ylab=NA)
text(0,1,labels="lty=0")
for(i in 2:7){
  lines(x=1:10,y=rep(i,10),lty=i-1,xlab=NA,ylab=NA)
  text(0,i,labels=paste0("lty=",i-1))
}


## 3.2 低级绘图函数
# 3.2.1 标题
attach(iris)
boxplot(Sepal.Length~Species,col=heat.colors(3),
        main=list("Sepal.Length按照Species分类的箱线图",
                  font=4,col="red",cex=1.5),
        sub=list("数据来源：iris数据集",font=3,
                 col="green",cex=0.8),
        xlab="Species",ylab="Sepal.Length")
# title函数
boxplot(Sepal.Length~Species,col=heat.colors(3))
title(main=list("Sepal.Length按照Species分类的箱线图",
                font=4,col="red",cex=1.5),
      sub=list("数据来源：iris数据集",font=3,
               col="green",cex=0.8),
      xlab="Species",ylab="Sepal.Length")
# title另一种方式
boxplot(Sepal.Length~Species,col=heat.colors(3))
title(main="Sepal.Length按照Species分类的箱线图",
      font.main=4,col.main="red",cex.main=1.5,
      sub="数据来源：iris数据集",font.sub=3,
      col.sub="green",cex.sub=0.8,
      xlab="Species",ylab="Sepal.Length")

# 3.2.2 坐标轴
#加载iris数据到内存
attach(iris)
#绘制箱线图
boxplot(Sepal.Length~Species,col=heat.colors(3),
        axes=FALSE,xlab="Species",ylab="Sepal.Length")
#设置X轴样式
axis(side=1,at=1:3,labels=unique(Species),col.axis="red",tick=FALSE)
#设置Y轴样式
axis(side=2,col.ticks = "gold",font = 3,col = "blue")

# 3.2.3 图例
#绘制分组柱状图
barplot(VADeaths,beside = TRUE,col=cm.colors(5))
# 添加图例
legend("top",legend=rownames(VADeaths),
       ncol=5,fill=cm.colors(5),bty="n")

# 3.2.4 网格线
op <- par(mfcol=1:2)
barplot(VADeaths,beside = TRUE,col=cm.colors(5),
        main="plot VADeaths with grid()")
grid()
barplot(VADeaths,beside = TRUE,col=cm.colors(5),
        main="plot VADeaths with grid(NA,7,lty=2,lwd=1.5,col='green')")
grid(NA,7,lty=2,lwd=1.5,col="green")
par(op)

# 3.2.5 点
set.seed(1234)
data <- c(rnorm(100,mean=0,sd=1),rnorm(3,mean=4,sd=1))
boxplot(data,col="violet",ylim=c(-4,5),outline=F)
points(rep(1,3),data[101:103],pch=21,bg="yellow",cex=1.2)

# 3.2.6 文字
text(rep(1,3),data[101:103],pos=4,label=paste0("异常值",round(data[101:103],3)))

# 3.2.7 线
# lines函数
data(economics, package = "ggplot2") 
attach(economics)                      #将economics加载到内存
plot(date,psavert,type="l",ylab="",ylim=c(0,26))    #绘制psavert随时间变化的时序图
lines(date,uempmed,type="l",col="blue") #绘制uempmed曲线，并设置为蓝色
detach(economics)                       #将economics从内存中移除
# abline函数
# 通常会调用abline画一条线
attach(iris)
# 绘制一幅简单的散点图
plot(Petal.Length~Petal.Width)
# 绘制Petal.Length变量均值的水平线
abline(h=mean(Petal.Length),col="gray60")
# 绘制Petal.Width变量均值的竖直线
abline(v=mean(Petal.Width),col="gray60")
# 绘制拟合直线
abline(lm(Petal.Length~Petal.Width),
       col="red",lwd=2,lty=3)
detach(iris)


## 3.3 高级绘图函数
# 3.3.1 散点图
# 1.普通散点图
par(mfrow=c(1,2))
# 绘制一维数据
plot(x=rnorm(10)) 
# 绘制二维数据
plot(women)
par(mfrow=c(1,1))
# 2.散点图矩阵
# 利用plot函数
plot(iris[,1:4],main="利用plot函数绘制散点图矩阵")
# 利用pairs函数
# 方法一
pairs(iris[,1:4],main="利用pairs函数绘制散点图矩阵")
# 方法二
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
      data=iris,main="利用pairs函数绘制散点图矩阵")
# 3.高密度散点图
# 创建一个大数据集
n <- 10000
x1  <- matrix(rnorm(n), ncol = 2)
x2  <- matrix(rnorm(n, mean = 3, sd = 1.5), ncol = 2)
x   <- rbind(x1, x2)
# 利用plot与smoothScatter函数绘制散点图
par(mfrow=c(1,2))
plot(x,main="利用plot函数绘制普通散点图")
smoothScatter(x,main="利用smoothScatter函数绘制高密度散点图")
par(mfrow=c(1,1))

# 3.3.2 气泡图
data("diamonds",package = "ggplot2")
# 随机抽取500个样本
diamonds1 <- diamonds[sample(1:nrow(diamonds),500),]
attach(diamonds1)
# 计算钻石体积
volumn <- x*y*z
# 把钻石体积进行归一化处理，并赋予对象size
size <- (volumn-min(volumn))/(max(volumn)-min(volumn))
# 利用plot函数绘制气泡图
plot(carat,price,cex=size*2)
# 利用symbols函数绘制气泡图
x<-rnorm(10)
y<-rnorm(10)
r<-abs(rnorm(10))
symbols(x,y,circle = r,
        bg=rainbow(10))

# 3.3.3 线图
type <- c('l','b','c','o','s','S')
par(mfrow=c(2,3))
for(i in 1:6){
  plot(1:10,type=type[i],main=paste0("type=",type[i]))
}
par(mfrow=c(1,1))

# 3.3.4 柱状图
par(mfrow=c(1,2))
for(i in c(FALSE,TRUE)){
  barplot(VADeaths,horiz = i,beside = T,col = rainbow(5))
}
par(mfrow=c(1,1))
# 增加图例
barplot(VADeaths,beside = T,col = rainbow(5),
        legend.text = rownames(VADeaths))

# 3.3.5 饼图
pie(table(mtcars$cyl))

# 3.3.6 直方图和密度图
# 绘制直方图
data(economics, package = "ggplot2") 
attach(economics)                      #将economics加载到内存
par(mfrow=c(2,2))
hist(psavert,8,xlab="个人储蓄率",ylab="频数",col="blue",
     main="个人储蓄率直方图（较少区间）")
hist(psavert,30,xlab="个人储蓄率",ylab="频数",col="blue",
     main="个人储蓄率直方图（较多区间）")
hist(uempmed,8,xlab="一周内平均失业持续时间",ylab="频数",col="green",
     main="一周内平均失业持续时间（较少区间）")
hist(uempmed,30,xlab="一周内平均失业持续时间",ylab="频数",col="green",
     main="一周内平均失业持续时间（较多区间）")
par(mfrow=c(1,1))
detach(economics)                 #将economics从内存中释放
# 绘制核密度图
plot(density(economics$psavert))
rug(economics$psavert)

# 3.3.7 Q-Q图
qqnorm(economics$psavert)

# 3.3.8 箱线图
boxplot(iris$Sepal.Length~iris$Species,col=rainbow(3))

# 3.3.9 茎叶图
stem(mtcars$wt)

# 3.3.10 点图
dotchart(mtcars$mpg,labels = rownames(mtcars))

# 3.3.11 马赛克图
ftable(Titanic)
mosaicplot(Titanic)
library(vcd)
mosaic(Titanic,shade = T,legend = T)