###  2.1.4 R快速入门  ###
a <- 1:10
a
A
cor(iris[,1:4])
Cor(iris[,1:4])

# 获取帮助
?median # 等价于help("median"),查看中位数函数的帮助文档 
??median # 等价于help.serach("median") 搜索包含median的帮助信息
help("runExample")
help("runExample",package = "shiny")
help("runExample",try.all.packages = TRUE)
apropos("plot")
example("median")
data()
data(package = .packages(all.available = TRUE))

# 工作空间
# 创建数据对象a,b
a <- 1:10
b <- 10:1
# 创建模型对象fit
fit <- lm(Sepal.Length~Sepal.Width,data=iris)
# 创建图形对象q、p
library(ggplot2)
q <- qplot(mpg, wt, data = mtcars)
p <- rPlot(Sepal.Length ~ Sepal.Width | Species, data = iris, 
           type = 'point', color = 'Species')
# 移除对象fit
rm(list="fit")
ls()
# 移除剩下的所有对象
rm(list=ls())
ls()


###  2.2 数据对象 ###
# 2.2.1 向量
# 在大多数情况下，使用长度大于1的向量。可以在R中使用c( )函数和相应的参数来创建一个向量:
(w<-c(1,3,4,5,6,7))           #创建数值型向量
length(w)                     #查看向量的长度
mode(w)                       #查看向量的数据类型
(w1<-c("张三","李四","王五")) #创建字符型向量
length(w1)                    #查看向量的长度
mode(w1)                      #查看向量的数据类型
(w2<-c(T,F,T))                #创建逻辑型向量
length(w2)                    #查看向量的长度
mode(w2)                      #查看向量的数据类型
# 一个向量的所有元素都必须属于相同的模式。如果不是，R将强制执行类型转换。
w3 <- c(w,w2)  # 数值型+逻辑型=数值型
w3
mode(w3)
w4<-c(w,w1)    # 数值型+字符型=字符型
w4
mode(w4)
w5<-c(w1,w2)  # 字符型+逻辑型=字符型
w5
mode(w5)
####### 向量化  #######
rm(list=ls())
(w<-seq(1:10))
(x<-round(sqrt(w),3))
# 也可以利用R的这个特性进行向量的算术运算
rm(list=ls())
(w1<-c(2,3,4))
(w2<-c(3.1,4.2,5.3))
(w<-w1+w2)
# 如果两个向量的长度不同，R将利用循环规则，该规则重复较短的向量元素，直到得到的向量长度与较长的向量的长度相同。
# 例一
rm(list=ls())
(w1<-c(2,4,6,8))
(w2<-c(10,12))
(w<-w1+w2)
# 例二
rm(list=ls())
(w1<-c(2,4,6,8))
(w2<-c(10,12,14))
(w<-w1+w2)
# 等差序列的创建
1:10
10:1
# 只给出首项和尾项数据，by自动匹配为1或-1
seq(1,9)
seq(1,-9)
#给出首项和尾项数据以及长度，自动计算等差
seq(1,-9,length.out=5)
#给出首项和尾项数据以及等差，自动计算长度
seq(1,-9,by=-2)
#给出首项和等差以及长度数据，自动计算尾
seq(1,by=2,length.out=10)
# 重复序列的创建
rep(1:4,times=2)
rep(1:4,each=2)
rep(1:4, c(2,1,2,1))
rep(1:4, times = 2, length.out = 6)
rep(1:4, times = 2, length.out = 10)
letters
LETTERS
# 索引向量
# 以下三种方法返回相同的值：
set.seed(1234)
x <- rnorm(5) 
x[c(1,3,5)]
x[c(-2,-4)]
x[c(TRUE,FALSE,TRUE,FALSE,TRUE)]
# 混合使用正负值是不允许的，会抛出一个错误：
x[c(1,-2)]
# 利用表达式提取元素
x>0
x[x>0]
# which 函数将返回逻辑向量中为TRUE 的位置。如果要将逻辑索引切换到整数索引中，这个函数很有用：
set.seed(123)
v <- sample(1:10,5)
v
which(v>5)
which.min(v) 
which.max(v) 

# 2.2.2 矩阵和数组
# 矩阵创建
(w<-seq(1:10))
(a<-matrix(w,nrow=5,ncol=2))
(a<-matrix(w,nrow=5,ncol=2,byrow=T))  #按行填充
(a<-matrix(w,nrow=5,ncol=2,byrow=T,
           dimnames=list(paste0("r",1:5),paste0("l",1:2))))  #给行列设置名称
# 矩阵的合并
(x1<-rbind(c(1,2),c(3,4)))
(x2<-10+x1)
(x3<-cbind(x1,x2))
(x4<-rbind(x1,x2))
cbind(1,x1)
# 矩阵的拉直
(A<-matrix(1:6,nrow=2))
as.vector(A)
# 矩阵的行或列计算的函数
(A <- matrix(1:16,4,4))
colSums(A)  # 等价于 apply(A,2,sum)
colMeans(A) # 等价于 apply(A,2,mean)
rowSums(A)  # 等价于 apply(A,1,sum)
rowMeans(A) # 等价于 apply(A,1,mean)

# 数组创建
rm(list=ls())
(w<-array(1:30,dim=c(3,5,2)))

#2.2.3 列表和数据框
# 列表创建
user.list<-list(user.id=34453,
                user.name="张三",
                user.games=c('地铁跑酷','神庙逃亡2','水果忍者','苍穹变'))
user.list
length(user.list) #检查列表成分个数
unlist(user.list)  #转换成向量元素
# 数据框创建
my.dataset<-data.frame(userid=c("S001","S002","S003","S004","S005"),
                       gamename=c('地铁跑酷','神庙逃亡2','水果忍者','水果忍者','机战王'),
                       iamount=c(100,50,30,60,70))
my.dataset
# 利用names函数查看变量名称
names(my.dataset)
# 我们也可以通过colnames函数查看变量名称，rownames函数查看行名称。
head(mtcars) #查看前六行数据
colnames(mtcars)  # 查看变量名称
rownames(mtcars)  # 查看记录名称
# 利用names函数修改变量名称
names(my.dataset)
names(my.dataset)[1] <- "vopenid"
names(my.dataset)
# 利用reshape包中rename函数对变量名进行批量修改
library(reshape)
newdata <- rename(my.dataset,c("vopenid" = "用户ID",
                               "gamename" = "游戏名称","iamount" = "付费金额"))
names(newdata)
# 数据框索引
my.dataset$gamename
my.dataset[["gamename"]]
my.dataset[[2]]
my.dataset[,2]
# 如果我们想提取前三行，前两列的数据
my.dataset[1:3,1:2]
my.dataset[1:3,c("vopenid","gamename")]


###  2.3 数据的导入 ###
# 2.3.2 文本文件的导入
import.txt <- read.table("iris.txt",header = TRUE) # 读入iris.txt文件
head(import.txt)
import.csv <- read.table("iris.csv",sep = ",") #读入iris.csv文件
head(import.csv)
import.csv1 <- read.csv("iris.csv") # 利用read.csv将iris.csv文件读入
head(import.csv1)
# 读取非结构化文本文件
unstructuredText <- readLines("unstructuredText.txt")
unstructuredText
# 2.3.3 Excel文件的导入
# 利用RODBC包读入
library(RODBC)
channel <- odbcConnectExcel2007("sample.xlsx") # 建立连接
odbcdf <- sqlFetch(channel,'data')     # 读取工作表data的数据
odbcClose(channel)                     # 关闭连接
odbcdf
# 利用xlsx包读取EXcel数据
library(xlsx)
res <- read.xlsx('sample.xlsx',1,encoding="UTF-8")    # 利用read.xlsx函数读取Excel文件
res
detach(package:xlsx)
# 利用XLConnect包读取Excel数据
library(XLConnect)
wb <- loadWorkbook("sample.xlsx")    # 加工作薄加载到R中
xldf<-readWorksheet(wb,sheet=getSheets(wb)[1])  #读取第一个工作表的数据
xldf
# 利用readxl包读取Excel数据
library(readxl)
readexcel <- read_excel("sample.xlsx",1,col_names = T)
readexcel

# 2.3.5 访问网络数据
# 方法一 利用readLines函数和正则表达式提取网页数据
# 爬取全部网页
web <- NULL
for(i in 1:8){
  url <- paste0("https://edu.hellobi.com/course/explore?page=",i)
  web1 <- readLines(url,encoding = 'UTF-8')
  web <- c(web1,web)
}
# 提取课程名称所在的行
class <- web[grep("class=\"caption\"",web)+3]
# 删除多余的空格
class <- gsub(" ","",class)
# 提取课时所在的行
length <- web[grep("class=\"length\"",web)]
# 利用正则表达式提取课时数
length <- substr(length,regexpr("i>",length)+2,regexpr("课",length)-1)
# 提取学生人数
people <- web[grep("class=\"pull-right people\"",web)]
people <- substr(people,regexpr(">",people)+1,regexpr("人",people)-1)
# 提取授课老师
teacher <- web[grep("class=\"teacher\"",web)]
for(i in 1:length(teacher)){
  teacher[i] <- substr(teacher[i],gregexpr(">",teacher[i])[[1]][2]+1,gregexpr("<",teacher[i])[[1]][3]-1)
}
# 提取课程价格 
price <- web[grep("class=\"teacher\"",web)+1]
price <- substr(price,regexpr(">",price)+1,regexpr("/",price)-2)
# 将结果整理成data.frame形式
result <- data.frame(课程=class,课时数=length,学生人数=people,授课老师=teacher,课程价格=price)
head(result)

#### 利用rvest包爬取网页数据
library(rvest)
library(magrittr)
result <- data.frame(课程=1,课时数=1,学生人数=1,授课老师=1,课程价格=1)
result <- result[-1,]
for(i in 1:7){
  url <- paste0("https://edu.hellobi.com/course/explore?page=",i)
  web <- read_html(url,encoding = 'UTF-8')
  class <- web %>% html_nodes("div.course-box") %>% 
    html_nodes("img")  # 提取课程名称
  class <- substr(class,regexpr("alt=",class)+5,regexpr(">",class)-3)
  length <- web %>% html_nodes("div.meta") %>% html_nodes("span.length") %>%
    html_text() # 提取课时数
  people <- web %>% html_nodes("div.meta")  %>% html_nodes("span.people") %>%
    html_text()  # 提取学习人数
  teacher <- web %>% html_nodes("div.meta") %>% html_nodes("span.teacher") %>% 
    html_text()  # 提取老师
  price <- web %>% html_nodes("div.meta") %>% html_nodes("span.price")  %>%
    html_text()  # 提取价格
  result1 <- data.frame(课程=class,课时数=length,学生人数=people,
                          授课老师=teacher,课程价格=price)
  result <- data.frame(rbind(result,result1))
}
head(result)
