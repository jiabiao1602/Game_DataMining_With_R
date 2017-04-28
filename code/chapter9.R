## 9.3 玩家物品购买关联分析
# 9.3.3 案例：对玩家物品购买关联分析
data <- read.csv("玩家购物数据.csv",T)
# 利用cast函数对数据进行重组
library(reshape)
data_matrix <- cast(data,player_id~product_name,value = "qty")
# 查看前三行五列数据
data_matrix[1:3,1:5]
# 进行替换，将NA转化为0，其他数字为1
data_matrix_new <- apply(data_matrix[,-1],2,function(x) {ifelse(is.na(x),0,1)})
# 对矩阵行名称、列名称进行赋值
data_matrix_new <- matrix(data_matrix_new,nrow=dim(data_matrix_new)[1],
                          ncol=dim(data_matrix_new)[2],
                          dimnames = list(data_matrix[,1],colnames(data_matrix)[-1]))
# 查看前三行五列数据
data_matrix_new[1:3,1:5]
# 利用as函数将矩阵转换成事物型
library(arules)
data_class <- as(data_matrix_new,"transactions")
inspect(data_class[1:6]) # 查看前六条交易记录
summary(data_class) # 查看汇总信息
# itemFrequency函数计算各个项集的出现频率(支持度)
itemFrequency(data_class[,1:3]) # 查看前三件道具的支持度
itemFrequency(data_class[,c('8条钥匙','15000金币','0.1元大礼包')]) # 查看指定道具的支持度
# 使用itemFrequencyPlot函数绘制支持度频率图
par(mfrow=c(2,1))
# 输出支持度support大于0.05的项集的支持度频率图
itemFrequencyPlot(data_class,support=0.05,main="support大于0.05的项集支持度频率图") 
# 输出支持度support最大的前20个项集的支持度频率图
itemFrequencyPlot(data_class,topN=20,main="support最大的前20个项集的支持度频率图")
par(mfrow=c(1,1))
# 建立关联规则rules
rules <- apriori(data_class,
                 parameter=list(support=0.005,confidence=0.1,target="rules",minlen=2))
summary(rules) # 查看规则汇总信息
inspect(rules[1:6]) # 查看前六条规则
quality(rules[1:6]) # 提取前六条规则信息
# 对规则按照提升度排序，并输出提升度最大的前六条规则
inspect(sort(rules,by="lift")[1:6]) 
# 提取规则
# 提取前项包含"超值大礼包"或"新手礼包"的规则
inspect(subset(rules,subset=lhs %in% c("超值大礼包","新手礼包")))
# 提取lhs包含"超值大礼包"或"新手礼包"的规则且lift大于1的规则
inspect(subset(rules,subset=lhs %in% c("超值大礼包","新手礼包") & lift>1))
# 对关联规则进行可视化
rules_lift <- subset(rules,subset=lift>2)
library(arulesViz)
#绘制关联图形
plot(rules_lift,method="graph",
     control = list(nodeCol = grey.colors(10), 
                    edgeCol = grey(.7), alpha = 1))
#绘制分组矩阵
plot(rules_lift,method = "grouped",
     control = list(col = grey.colors(10))) 
# 将规则导出到本地
write(rules,"rules.txt",sep="|",row.names=F)

## 9.4 基于玩家物品的智能推荐
# 9.4.2 案例：对玩家物品购买进行智能推荐
library(recommenderlab)
# 将矩阵转化为binaryRatingMatrix对象
data_class <- as(data_matrix_new,"binaryRatingMatrix")
as(data_class,"matrix")[1:3,1:5] #显示部分物品购买情况
# 建立三种模型，并对模型进行评价
# 创建一个评分方案 (十折交叉验证)
scheme <- evaluationScheme(data_class, method="cross-validation",
                       k=10, given=-1)
## 创建用于评估模型的算法列表
algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  UBCF=list(name="UBCF",param=NULL),
  IBCF=list(name="IBCF",param=NULL)
)
# 对模型进行评价
result <- evaluate(scheme,algorithms,n=1:5)
# 输出ROC曲线和precision-recall曲线
par(mfrow=c(1,2))
plot(result,lty=1:3,annotate=1:3,legend="topleft") # ROC
plot(result,"prec/rec",lty=1:3,annotate=1:3) #precision-recall
par(mfrow=c(1,1))

# 按照评价方案建立推荐模型
model.popular <- Recommender(getData(scheme,"train"),method="POPULAR")
model.ubcf <- Recommender(getData(scheme,"train"),method="UBCF")
model.ibcf <- Recommender(getData(scheme,"train"),method="IBCF")
# 对推荐模型进行预测
predict.popular <- predict(model.popular,getData(scheme,"known"),type="topNList")
predict.ubcf <- predict(model.ubcf,getData(scheme,"known"),type="topNList")
predict.ibcf <- predict(model.ibcf,getData(scheme,"known"),type="topNList")
# calcPredictionAccuracy()的参数"know"和"unknow"表示对测试集的进一步划分：
# "know"表示用户已经评分的，要用来预测的items；
# "unknow"表示用户已经评分，要被预测以便于进行模型评价的item
predict.err <- rbind(calcPredictionAccuracy(predict.popular,getData(scheme,"unknow"),given=3),
                     calcPredictionAccuracy(predict.ubcf,getData(scheme,"unknow"),given=3),
                     calcPredictionAccuracy(predict.ibcf,getData(scheme,"unknow"),given=3))
rownames(predict.err) <- c("POPULAR",'UBCF','IBCF')
round(predict.err,3)

#选择IBCF作为最优模型
model.best <- Recommender(data_class,method="IBCF")
# 使用precict函数，对玩家进行Top3推荐
data.predict <- predict(model.best,newdata=data_class,n=5)
recom3 <- bestN(data.predict,3)
as(recom3,"list")[1:5] #查看前五个玩家的top3推荐


## 9.5 社会网络分析
## 9.5.1 网络图的基本概念及R语言实现
par(mfrow=c(1,3))
library(igraph)
g1 <- graph(c(2,1,3,1,4,1,5,1,6,1,7,1,8,7),directed = F)
plot(g1,vertex.size=30,vertex.label.cex=2);title(xlab=list("无向图g1",cex=3))
g2 <- graph(c(1,2,3,1,2,4,3,5,4,6,5,4,5,6,8,9,9,10),directed = F)
plot(g2,vertex.size=30,vertex.label.cex=2);title(xlab=list("无向图g2",cex=3))
g3 <- graph(c(2,1,3,1,4,1,5,1,6,1,7,1,1,7,8,7))
plot(g3,vertex.size=30,vertex.label.cex=2,edge.arrow.size=0.5);title(xlab=list("有向图g3",cex=3))
par(mfrow=c(1,1))
str(g1)
vcount(g1)
V(g1)
ecount(g1)
E(g1)
E(g3)
degree(g1,v=1)
degree(g3,v=1,mode="in")
degree(g3,v=1,mode="out")
get.shortest.paths(g2,from = 4,to = 5)[[1]]

# 9.5.2 网络图的R语言实现
library(igraph)
g <- graph(c(2,1,3,1,4,1,5,1,6,1,7,1),directed = F) #创建无向图g
par(mfrow=c(2,3))
plot(g,vertex.size=40,layout=layout_on_grid,main="简单的网格布局") # 简单的网格布局
plot(g,vertex.size=40,layout=layout.auto,main="自动布局") #自动布局
plot(g,vertex.size=40,layout=layout_as_star,main="星形布局") #星形布局
plot(g,vertex.size=40,layout=layout.circle,main="环形布局") # 环形布局
plot(g,vertex.size=40,layout=layout_randomly,main="随机布局") # 随机布局
plot(g,vertex.size=40,layout=layout_as_tree(g),main="树状布局") # 树状布局
par(mfrow=c(1,1))
# 对网络图g进行美化
library(RColorBrewer)
weight <- seq(1,3,length.out=ecount(g)) #生成边的权重
# 方法一
V(g)$color <- brewer.pal(9,"Set1")[1:vcount(g)] # 设置点填充颜色
V(g)$frame.color <- NA           # 不显示点边框
V(g)$label.color <-"white"       # 设置点标签颜色为白色
V(g)$label.font <- 2              # 设置点标签的字体为粗体
V(g)$label.cex <- 2              # 设置点标签字体大小
E(g)$width <- weight             # 根据边的权重设置边的粗细
E(g)$color <- "steelblue4"       # 设置边的颜色
E(g)$lty <- 2                    # 设置边为虚线
plot(g,main="对网络图g进行美化")
# 方法二
plot(g,vertex.color=brewer.pal(9,"Set1")[1:vcount(g)],
     vertex.frame.color=NA,vertex.label.color="white",
     vertex.label.font=2,vertex.label.cex=2,
     edge.width=weight,edge.color="steelblue4",edge.lty=2,
     main="对网络图g进行美化")
# 利用tkplot函数制作交互图
tkplot(g)
# 利用rglplot函数制作3D图
rglplot(g)

# 对网络数据进行社群发现
library(igraphdata)
data(karate)
fc1 <- multilevel.community(karate) # 多层次聚类
fc2 <- edge.betweenness.community(karate) # 边中间性聚类
fc3 <- walktrap.community(karate) #随机游走聚类
fc4 <- infomap.community(karate) #infomap算法聚类
fc5 <- fastgreedy.community(karate) # 快速贪婪聚类
fc6 <- label.propagation.community(karate) # 标签传播
fc_list <- list(fc1,fc2,fc3,fc4,fc5,fc6)
algorithm_list <- c("multilevel","edge.betweenness","walktrap",
                    "infomap","fastgreedy","label.propagation")
par(mfrow=c(2,3),mar=c(1,1,2,1))
for(i in 1:6){
  plot(fc_list[[i]],karate,main=algorithm_list[[i]])
}
par(mfrow=c(1,1))

## 9.5.3 R与Gephi的结合
# 导入社交数据
Links <- read.csv("Links.csv")
Nodes <- read.csv("Nodes.csv")
# 转换成graph.object对象
library(igraph)
g <- graph.data.frame(Links,directed = T,vertices = Nodes)
g
# 绘制网络图
# 对顶点进行美化
V(g)$size <- 10
V(g)$color <- Nodes$Group
V(g)$label <- as.character(Nodes$Label)
V(g)$label.cex <- 0.7
# 对边进行美化
E(g)$width <- (Links$Weight-min(Links$Weight))*5/(max(Links$Weight)-min(Links$Weight))
E(g)$arrow.size <- 0.5
plot(g,main="用户社会网络图")
# 移除某个玩家后的网络图
remove.u <- "user12"
remove.v <- which(V(g)$Label!=remove.u)
g.new <- induced.subgraph(g,remove.v)
plot(g.new,vertex.size=10,
     vertex.color=Nodes[-which(Nodes$Label==remove.u),"Group"],
     vertex.label=as.character(setdiff(Nodes$Label,remove.u)),
     vertex.label.cex=0.7,
     main=paste0("移除",remove.u,"用户后的网络图"))

## 9.5.4 案例:玩家物品购买分类分析
# 将data_matrix_new数据转换成from、to的形式
data_new <- c()
for(i in 1:nrow(data_matrix_new)){
  item.i <- colnames(data_matrix_new)[which(data_matrix_new[i,]==1)]
  item.i.num <- length(item.i)
  from <- c();to <- c()
  for(m in 1:(item.i.num-1)){
    from <- c(from,item.i[-c((item.i.num-m+1):item.i.num)])
    to <- c(to,item.i[-c(1:m)])
  }
  data_new <- rbind(data_new,matrix(c(from,to),ncol = 2))
}
data_new <- as.data.frame(data_new)
head(data_new)
# 对转换后的数据进行汇总，并按照频数进行降序排序
library(sqldf)
data_count <- sqldf("select V1,V2,count(*) from data_new group by V1,V2")
colnames(data_count) <- c("from","to","qty")

data_count <- data_count[order(data_count$qty,decreasing = T),]
head(data_count)
# 将数据转换为graph.object，并利用多种聚类算法发现社群结构
library(igraph)
G <- graph.data.frame(data_count[1:100,],directed = F)
E(G)$weight <- data_count[1:100,"qty"]
fc1 <- multilevel.community(G) # 多层次聚类
fc2 <- edge.betweenness.community(G) # 边中间性聚类
fc3 <- walktrap.community(G) #随机游走聚类
fc4 <- infomap.community(G) #infomap算法聚类
fc5 <- spinglass.community(G) # 自旋玻璃社群聚类
fc6 <- label.propagation.community(G) # 标签传播
fc_list <- list(fc1,fc2,fc3,fc4,fc5,fc6)
algorithm_list <- c("多层次聚类","边中间性聚类","随机游走聚类",
                    "infomap算法聚类","自旋玻璃社群聚类","标签传播")
par(mfrow=c(2,3))
for(i in 1:6){
  plot(fc_list[[i]],G,main=algorithm_list[[i]])
}
par(mfrow=c(1,1))

