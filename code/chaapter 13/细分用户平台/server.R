load("userdata.RData")
actionuser<-actionuser[,c("用户id", "最后一周登陆天数", "最后一周登陆次数", "最后一周0.8点登陆次数", 
                          "最后一周8.18点登陆次数", "最后一周18.24点登陆次数", 
                          "注册至今距离天数", "是否付费")] 
n <- dim(actionuser)[1] # 计算活跃用户数
# 数据抽样
# 对数据进行分区，90%的数据作为测试集用来建模，10%的数据作为验证集用来验证模型
library(caret)
set.seed(1234)
ind<-createDataPartition(actionuser$是否付费,times=1,p=.9,list=FALSE)
traindata<-actionuser[ind,]
testdata<-actionuser[-ind,]
# 对类失衡数据进行重新抽样，达到数据平衡。
# 对付费用户(1)进行重复抽样，将付费人数增加一倍
ind1<-sample(which(traindata$是否付费==1),2*length(which(traindata$是否付费==1)),replace=TRUE)
# 对非付费用户(0)进行不放回抽样，将非付费人数减少一半
ind2<-sample(which(traindata$是否付费==0),0.5*length(which(traindata$是否付费==0)),replace=FALSE)
# 组合成新数据集newdata
newdata<-traindata[c(ind1,ind2),]
# 利用条件推理决策树算法建立分类树模型
# 首先将树的最大深度设置为3
newdata$是否付费<-as.factor(newdata$是否付费)
library(party)
ctree.sol<-ctree(是否付费~.,data=newdata[,-1],control=ctree_control(mincriterion=0.99,maxdepth=2))
                          
                          
function(input,output){
  ######   用户分群研究  #######
  # 计算不同的用户群人数
  small <- reactive({user_pay[user_pay$amount <= input$small,]})
  middle <- reactive( {user_pay[user_pay$amount > input$small &
                                  user_pay$amount <= input$middle,]})
  big <- reactive( {user_pay[user_pay$amount > input$middle &
                                  user_pay$amount <= input$big,]})
  super <- reactive( {user_pay[user_pay$amount > input$big,]})
  # 统计不同用户群的占比
  output$vbox0 <- renderValueBox({
    valueBox(
      h4(strong("85.89%",style="color:white;")),
      subtitle = h5(em(paste("非R人数共计",n,"人"))),
      icon = icon("user-md"),
      color="aqua",
      width=2
    )
  })
  output$small <- renderValueBox({
    valueBox(
      h4(strong(paste(round(nrow(small())/n*100,2),"%",sep=""),
                style="color:white;")),
      subtitle = h5(em(paste("小R人数共计",nrow(small()),"人"))),
      icon = icon("user-md"),
      color="aqua",
      width=2
    )
  })
  output$middle <- renderValueBox({
    valueBox(
      h4(strong(paste(round(nrow(middle())/n*100,2),"%",sep=""),
                style="color:white;")),
      subtitle = h5(em(paste("中R人数共计",nrow(middle()),"人"))),
      icon = icon("user-md"),
      color="aqua",
      width=2
    )
  })
  output$big <- renderValueBox({
    valueBox(
      h4(strong(paste(round(nrow(big())/n*100,2),"%",sep=""),
                style="color:white;")),
      subtitle = h5(em(paste("大R人数共计",nrow(big()),"人"))),
      icon = icon("user-md"),
      color="aqua",
      width=2
    )
  })
  output$super <- renderValueBox({
    valueBox(
      h4(strong(paste(round(nrow(super())/n*100,2),"%",sep=""),
                style="color:white;")),
      subtitle = h5(em(paste("超R人数共计",nrow(super()),"人"))),
      icon = icon("user-md"),
      color="aqua",
      width=2
    )
  })
  # 绘制金字塔图
  output$barplot <- renderPlot({
    x<-c(0.8589,
         round(nrow(small())/n,4),
         round(nrow(middle())/n,4),
         round(nrow(big())/n,4),
         round(nrow(super())/n,4)
         )
    y<--x
    barplot(x,horiz=T,space=0,xlim=c(-1,1),axes=F,col="green")
    axis(2,c(0.5,1.5,2.5,3.5,5),labels=c("非R","小R","中R","大R","超R"),
         tick=FALSE,cex.axis=0.8)
    axis(4,c(0.5,1.5,2.5,3.5,5),labels=round(x,3),
         tick=FALSE,cex.axis=0.75)
    barplot(y,horiz=T,space=0,add=T,col="green",axes=F)
  })
  
  output$barplot1 <- renderPlot({
    x<-c(round(nrow(small())/n,4),
         round(nrow(middle())/n,4),
         round(nrow(big())/n,4),
         round(nrow(super())/n,4)
    )
    y<--x
    barplot(x,horiz=T,space=0,xlim=c(-0.15,0.15),axes=F,col="green")
    axis(2,c(0.5,1.5,2.5,3.5),labels=c("小R","中R","大R","超R"),tick=FALSE,cex.axis=0.8)
    axis(4,c(0.5,1.5,2.5,3.5),labels=round(x,3),tick=FALSE,cex.axis=0.75)
    barplot(y,horiz=T,space=0,add=T,col="green",axes=F)
  })
  
  # 查看不同的用户群号码包
  output$table <- DT::renderDataTable(
    switch(input$dataset,
           "1"=small(),
           "2"=middle(),
           "3"=big(),
           "4"=super()),
    options=list(pageLength=10)
  )
  # 下载号码包
  output$downloadCsv <- downloadHandler(
    filename = "userid.csv",
    content = function(file) {
    write.csv(switch(input$dataset,
                         "1"=small(),
                         "2"=middle(),
                         "3"=big(),
                         "4"=super()
                     ), file,row.names=F)
    },
    contentType = "text/csv"
    )
  
  ######   潜在付费用户挖掘  #######
  # 导出明细数据
  output$actionuer <- DT::renderDataTable({
    DT::datatable(actionuser)
  })
  # 画出箱线图
  output$boxplot1 <- renderPlot({
    par(mfrow=c(2,3))
    for(i in 2:7)
      boxplot(actionuser[,i]~actionuser$是否付费,col=c(1,2)*i,outline=FALSE,
              main=paste(colnames(actionuser)[i],"箱线图"))
    par(mfrow=c(1,1))
  })
  
  # 画决策树图
  output$treeplot <- renderPlot({
    plot(ctree.sol)
  })
}