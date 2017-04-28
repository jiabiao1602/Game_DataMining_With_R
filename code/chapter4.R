##### 4.1 lattice包绘图工具
# 4.1.1 绘图特色
# 构建简单数据集
d <- data.frame(x=seq(0,14),y=seq(1,15),z=rep(c("a","b","c"),times=5))
# 绘制简单散点图
if(!require(lattice)) install.packages("lattice")
xyplot(y~x,data=d)
# 图形参数
op <- trellis.par.get()
trellis.par.get("axis.text")
trellis.par.set(list(axis.text=list(cex=1.5,col="blue")))
xyplot(y~x,data=d)
trellis.par.set(op)
show.settings()
xyplot(y~x,groups=z,data=d)
mysettings <- trellis.par.get()
mysettings$superpose.symbol$col 
mysettings$superpose.symbol$pch
mysettings$superpose.symbol$pch <- 1:10
mysettings$superpose.symbol$col <- "black"
trellis.par.set(mysettings)
xyplot(y~x,groups=z,data=d)
trellis.par.set(op)
# 条件变量
# 绘制带有条件变量的散点图
library(lattice)
xyplot(y~x|z,data=d,layout=c(3,1))
# 面板函数
mypanel <- function(...){
  panel.abline(a=1,b=1)
  panel.xyplot(...)
}
xyplot(y~x|z,data=d,layout=c(3,1),
       panel=mypanel)
# 分组变量
densityplot(~mpg,data=mtcars,lty=1:2,col=1:2,lwd=2,
           groups=factor(am),
           main=list("MPG Distrubution by Tranamission Type",cex=1.5),
           xlab="Miles per Gallon",
           key=list(column=2,
                    space="bottom",
                    title="Transmission (0 = automatic, 1 = manual)",
                    text=list(levels(factor(mtcars$am))),
                    lines=list(lty=1:2,col=1:2,lwd=2)))
# 页面摆放
graph1 <- xyplot(mpg~wt,data=mtcars,xlab="Weight",ylab="Miles Per Gallon")
displacement <- equal.count(mtcars$disp,number=3,overlap=0)
graph2 <- xyplot(mpg~wt|displacement,data=mtcars,layout=c(3,1),
                 xlab="Weight",ylab="Miles Per Gallon")
plot(graph1,split=c(1,1,2,1))
plot(graph2,split=c(2,1,2,1),newpage = FALSE)
plot(graph1,position = c(0,0,0.5,1))
plot(graph2,position=c(0.5,0,1,1),newpage = FALSE)

# 高级lattice函数
# 条形图
library(lattice)
str(Titanic) # 查看Titanic数据结构
barchart(Titanic,layout=c(4,1),auto.key=TRUE) # 对table类型画出条形图，data参数不用设置
barchart(Titanic,layout=c(4,1),
         auto.key=TRUE,scales=list(x="free")) #将x轴坐标设置为free
# 将表格数据Tatanic转换成数据框，然后绘制条形图
barchart(Class~Freq|Sex+Age,data=as.data.frame(Titanic),
         groups=Survived,stack=TRUE,layout=c(4,1),
         auto.key=TRUE,scales=list(x="free"))
# 改变图例摆放形式
barchart(Class~Freq|Sex+Age,data=as.data.frame(Titanic),
         groups=Survived,stack=TRUE,layout=c(4,1),
         auto.key=list(title="Survived",columns=2),
         scales=list(x="free"))
# 将lattice的高级绘图函数创建的栅栏图存在mygraph对象中。
mygraph <- barchart(Class~Freq|Sex+Age,data=as.data.frame(Titanic),
                    groups=Survived,stack=TRUE,layout=c(4,1),
                    auto.key=list(title="Survived",columns=2),
                    scales=list(x="free"))
# 通过update函数给mygraph图形增加垂直网格线,并将条形边框设置为透明色
update(mygraph,
       panel=function(...){
         panel.grid(h=0,v=-1)
         panel.barchart(...,border="transparent")
       })

# 点图
library(lattice)
# 绘制分组点图
dotplot(VADeaths,pch=1:4,col=1:4,
        main = list("Death Rates in Virginia - 1940",cex=1.5),
        xlab = "Rate (per 1000)",
        key=list(column=4,
                 text=list(colnames(VADeaths)),
                 points=list(pch=1:4,col=1:4)))
# 绘制面板点图
dotplot(VADeaths, groups = FALSE,
        main = list("Death Rates in Virginia - 1940",cex=1.5),
        xlab = "Rate (per 1000)")
# 调整type参数，美化点图
dotplot(VADeaths, groups = FALSE,
        layout=c(1,4),aspect = 0.5,
        origin = 0, type = c("p", "h"),
        main = list("Death Rates in Virginia - 1940",cex=1.3),
        xlab = "Rate (per 1000)")

# 直方图
library(lattice)
library(nutshell)
data(births2006.smpl)
histogram(~DBWT|DPLURAL,data=births2006.smpl,
          main="Births in the United States, 2006",
          xlab="Birth weight, in grams")
# 调整layout参数
histogram(~DBWT|DPLURAL,data=births2006.smpl,layout=c(1,5),
          main="Births in the United States, 2006",
          xlab="Birth weight, in grams")

# 核密度图
densityplot(~DBWT|DPLURAL,data=births2006.smpl,
            layout=c(1,5),plot.points=FALSE,
            main="Births in the United States, 2006",
            xlab="Birth weight, in grams")
# 绘制叠加密度图
densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl,
            plot.points=FALSE,lty=1:5,col=1:5,lwd=1.5,
            main="Births in the United States, 2006",
            xlab="Birth weight, in grams",
            key=list(column=3,
                     text=list(levels(births2006.smpl$DPLURAL)),
                     lines=list(lty=1:5,col=1:5)))

# 带状图
stripplot(~DBWT,data=births2006.smpl,
          subset=(DPLURAL=="5 Quintuplet or highter" |
                    DPLURAL=="4 Quadruplet"),
          jitter.data=TRUE,
          main="Births in the United States, 2006",
          xlab="Birth weight, in grams")

# Q-Q图
# 单变量Q-Q图
library(lattice)
qqmath(~ height | voice.part, aspect = "xy", data = singer,
       prepanel = prepanel.qqmathline,
       panel = function(x, ...) {
         panel.qqmathline(x, ...)
         panel.qqmath(x, ...)
       })
# 生成比较两个分布的Q-Q图
qq(voice.part ~ height, aspect = 1, data = singer,
   subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"))

# 箱线图
# 栅栏箱线图
bwplot( ~ height|voice.part, data=singer, xlab="Height (inches)")
# 分组箱线图
bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")

# 散点图
xyplot(Sepal.Length~Sepal.Width|Species,data=iris)
# 对Petal.Length变量进行分组再画图
xyplot(Sepal.Length~Sepal.Width|cut(Petal.Length,2),data=iris)

# 散点图矩阵
library(lattice)
# 普通散点图矩阵
splom(mtcars[c(1, 3:7)], groups = mtcars$cyl,
      auto.key=TRUE)
# 修改变量名和图例的散点图矩阵
splom(mtcars[c(1, 3:7)], groups = mtcars$cyl,
      pscales = 0,pch=1:3,col=1:3,
      varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
                   "Gross\nhorsepower", "Rear\naxle\nratio",
                   "Weight", "1/4 mile\ntime"),
      key = list(columns = 3, title = "Number of Cylinders",
                 text=list(levels(factor(mtcars$cyl))),
                 points=list(pch=1:3,col=1:3)))

# 三维水平图
library(lattice)
data(Cars93, package = "MASS")
cor.Cars93 <-cor(Cars93[, !sapply(Cars93, is.factor)], use = "pair")
levelplot(cor.Cars93,scales = list(x = list(rot = 90)))


# 三维等高线图
contourplot(volcano, cuts = 20, label = FALSE)

# 三维散点图
par.set <-list(axis.line = list(col = "transparent"),
               clip = list(panel = "off"))
cloud(Sepal.Length ~ Petal.Length * Petal.Width,data = iris, 
      cex = .8,pch=1:3,col=c("blue","red","green"),
      groups = Species,screen = list(z = 20, x = -70, y =0),
      par.settings = par.set,
      scales = list(col = "black"),
      key=list(title="Species",
               column=3,
               space="bottom",
               text=list(levels(iris$Species)),
               points=list(pch=1:3,col=c("blue","red","green"))))

# 三维曲面图
wireframe(volcano, shade = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10))


### 4.2 ggplot2包绘图工具
library(ggplot2)
# 利用qplot绘制箱线图
qplot(Species,Sepal.Length,data=iris,
      geom="boxplot",fill=Species,
      main="依据种类分组的花萼长度箱线图")
# 利用qplot绘制小提琴图
qplot(Species,Sepal.Length,data=iris,
      geom=c("violin","jitter"),fill=Species,
      main="依据种类分组的花萼长度小提琴图")
# 利用qplot绘制散点图
qplot(Sepal.Length,Sepal.Width,data=iris,
      colour=Species,shape=Species,
      main="绘制花萼长度和花萼宽度的散点图")
# 利用qplot绘制分面板散点图
qplot(Sepal.Length,Sepal.Width,data=iris,
      geom=c("point","smooth"),
      facets=~Species,colour=Species,
      main="绘制分面板的散点图")
# 利用ggplot函数绘制箱线图
library(ggplot2)
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()+
  labs(title="依据种类分组的花萼长度箱线图")
# 利用ggplot函数绘制小提琴图
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_violin()+
  geom_jitter()+
  labs(title="依据种类分组的花萼长度箱线图")
# 利用ggplot的分面函数绘制分面板密度图
data(singer,package = "lattice")
ggplot(data=singer,aes(x=height,fill=voice.part))+
  geom_density()+
  facet_grid(voice.part~.)
ggplot(data=singer,aes(x=height,fill=voice.part))+
  geom_density()+
  facet_wrap(~voice.part,ncol=4)+
  theme(legend.position="none")
# 调整图形填充颜色
library(gridExtra)
# 方式一：使用scale_color_manual函数
g1 <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,colour=Species,shape=Species))+
  scale_color_manual(values=c("orange", "olivedrab", "navy"))+
  geom_point(size=3)
# 方式而:使用scale_color_brewer函数
g2 <- ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,colour=Species,shape=Species))+
  scale_color_brewer(palette="Set1")+
  geom_point(size=3)
grid.arrange(g1,g2,ncol=2)
# 保存ggplot的图形
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+
  geom_point(size=2)
ggsave(file="mygraph.pdf",width=5,height=4)

# ggthemes包
library(ggplot2)
library(ggthemes)
library(gridExtra)
p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size=3) 
# Economist themes
p2 <- p1 + ggtitle("Economist theme") +
  theme_economist() + scale_colour_economist() 
# Solarized theme
p3 <- p1 + ggtitle("Solarized theme") + 
  theme_solarized() + scale_colour_solarized("blue")
grid.arrange(p2,p3,ncol=2)

# Stata theme
p4 <- p1 + ggtitle("Stata them") + 
  theme_stata() + scale_colour_stata()
# Excel 2003 theme
p5 <- p1 + ggtitle("Excel 2003  them") + 
  theme_excel() + scale_colour_excel()
grid.arrange(p4,p5,ncol = 2)
  

# 4.3 交互式绘图工具
# rCharts包
# nPlot函数
# 绘制交互柱状图
library(rCharts)
hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
hair_eye_male[,1] <- paste0("Hair",hair_eye_male[,1])
hair_eye_male[,2] <- paste0("Eye",hair_eye_male[,2])
nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
# 绘制交互条形图
n2 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type="multiBarHorizontalChart")
n2
n2$chart(showControls=F)
n2
# hPlot函数
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = 'scatter', 
           group = 'Sex', radius = 6, group.na = "Not Available")
a$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
a$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
a$chart(zoomType = "xy")
a$exporting(enabled = T)
a
# mPlot函数
data(economics, package = 'ggplot2')
dat <- transform(economics, date = as.character(date))
p1 <- mPlot(x = "date", y = "psavert", data = dat, type = 'Line',
            pointSize = 0, lineWidth = 1)
p1
p1$set(type="Area")
p1

# recharts包
# 散点图
library(recharts)
echartr(iris, Sepal.Length, Sepal.Width)
# 分组散点图
g <- echartr(iris, Sepal.Width, Petal.Width, series =Species)
g %>% setSeries(symbolSize=8)
# 改变散点符号,不显示工具箱
g <- echartr(iris, Sepal.Width, Petal.Width, series =Species) %>%
  setSeries(symbolSize=8) %>%
  setSymbols(c('heart', 'arrow','diamond')) %>%
  setToolbox(show=FALSE)
g
# 增加标记
g %>% addMarkPoint(series=unique(iris$Species),
                   data=data.frame(type="max",name="最大值"))
# 添加标题
g %>% setTitle("依据种类绘制的分组散点图")
# 改变标题和图例摆放位置
g %>% setTitle("依据种类绘制的分组散点图",pos=12) %>%
  setLegend(pos=3)
# 主题美化 
g <- echartr(iris, Sepal.Length, Sepal.Width) %>%
  setSeries(symbolSize=8)
g %>% setTheme('helianthus', calculable=TRUE)

# 条形图
revenue <- read.csv("revenue.csv")
library(reshape2)
revenue <- melt(revenue,id="游戏名称")
colnames(revenue) <- c("游戏名称","时间段","收入")
# 绘制条形图，默认hbar类型
b <- echartr(revenue,"游戏名称","收入","时间段") %>%
  setTitle("游戏收入",pos=12) %>%
  setLegend(pos=6)
b
b %>% setGrid(x=150)

# 增加权重变量
revenue$权重 <- ifelse(revenue$时间段=='本周',2,1)
# 绘制柱状图
echartr(revenue,"游戏名称","收入","时间段",weight = "权重",type = "hbar") %>%
  setTitle("游戏收入",pos=12) %>%
  setLegend(pos=6) %>% 
  setGrid(x=150)

# 绘制龙卷风图
revenue_tc <- revenue
revenue_tc$收入[revenue_tc$时间段=="上周"] <-
  -revenue_tc$收入[revenue_tc$时间段=="上周"] 
g <- echartr(revenue_tc,"游戏名称","收入","时间段",type = "hbar") %>%
  setTitle("游戏收入",pos=12) %>%
  setLegend(pos=6) %>% 
  setGrid(x=150)
g
# 金字塔图
g <- echartr(revenue_tc,"游戏名称","收入","时间段",type = "hbar",subtype='stack') %>%
  setTitle("游戏收入",pos=12) %>%
  setLegend(pos=6) %>% 
  setGrid(x=150) %>%
  setYAxis(axisLine=list(onZero=TRUE)) %>%
  setXAxis(axisLabel=list(
    formatter=JS('function (value) {return Math.abs(value);}')
  ))
g

# rbokeh包
# 绘制散点图
if(!require(rbokeh)) install.packages("rbokeh")
z <- lm(dist ~ speed, data = cars)
p <- figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm")
p
# 绘制直方图
h <- figure(width = 600, height = 400) %>%
  ly_hist(eruptions, data = faithful, breaks = 40, freq = FALSE) %>%
  ly_density(eruptions, data = faithful)
h
# 绘制箱线图
figure(ylab = "Height (inches)", width = 600) %>%
  ly_boxplot(voice.part, height, data = lattice::singer)

# plotly包
revenue <- read.csv("revenue.csv")
revenue
# 绘制柱状图
if(!require(plotly)) install.packages("plotly")
p <- plot_ly(revenue,y = ~本周,x = ~游戏名称,type = "bar",name = "本周")
p
p %>% add_trace(y = ~上周,name = "上周") 
p %>% 
  add_trace(y = ~上周,name = "上周") %>%
  layout(barmode = 'stack',
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         title = "游戏收入数据")
# 绘制箱线图
plot_ly(midwest, x = percollege, color = state, type = "box")

# googleVis包
library(googleViz)
M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(M1)

# leaflet包
# 在地图上标记R语言的诞生地--新西兰奥克兰大学
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=174.768,lat=-36.852,popup="ThebirthplaceofR")
# 在地图上标记深圳市
location <- REmap::get_city_coord("深圳")
leaflet()%>%
  setView(lng=location[1],lat=location[2],zoom=9)%>%
  addTiles() %>%
  addMarkers(lng=location[1],lat=location[2],popup="这里是深圳市")

# dygraphs包
if(!require(dygraphs)) install.packages("dygraphs")
# LTV预测曲线
LTV <- read.csv("LTV.csv")
LTV.ts <- ts(LTV)
dygraph(LTV.ts,main="LTV forecast") %>% 
  dySeries("V1",label="LTV",strokeWidth = 2) %>%
  dyOptions(colors = "black",fillGraph = FALSE,fillAlpha = 0.4) %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("x", drawGrid = FALSE) %>% 
  dyAxis("y", label = "LTV(Life Time Value)") %>%
  dyRangeSelector()

# DT包
library(DT)
datatable(iris)
datatable(iris,rownames = FALSE) # 不输出行号

# networkD3包
# 利用simpleNetwork函数绘制简单网络图
library(networkD3)
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
simpleNetwork(networkData,zoom=T)
# 利用networkD3函数绘制力导向网络图
# 加载数据
data(MisLinks)
data(MisNodes)
# 画图
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

# pairsD3包
if(!require(pairsD3)) install.packages("pairsD3")
pairsD3(iris[,1:4],group = iris[,5],
        labels = c("花萼长度","花萼宽度","花瓣长度","花瓣宽度","种类"))

# scatterD3包
if(!require(scatterD3)) install.packages("scatterD3")
mtcars$names <- rownames(mtcars)
scatterD3(data = mtcars, x = wt, y = mpg, lab = names,
          col_var = cyl, symbol_var = am,
          xlab = "Weight", ylab = "Mpg", col_lab = "Cylinders",
          symbol_lab = "Manual transmission")

# wordcloud2包
if(!require(wordcloud2)) devtools::install_github("lchiffon/wordcloud2")
letterCloud(demoFreqC[1:200,], "R", fontFamily = "微软雅黑",
            color = "random-light")

# timevis包
if(!require(timevis)) install.packages("timevis")
data <- data.frame(
  id      = 1:4,
  content = c("事项一"  , "事项二"  ,"事项三", "事项四"),
  start   = c("2016-11-10", "2016-11-11", "2016-11-20", "2016-12-14 15:00:00"),
  end     = c(NA          ,           NA, "2016-12-04", NA)
)
timevis(data)

# rpivotTable包
# devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
library(rpivotTable)
mtcars$vs <- factor(ifelse(mtcars$vs==0,"自动","手动"))
colnames(mtcars)[8] <- "传输"
rpivotTable(mtcars)

