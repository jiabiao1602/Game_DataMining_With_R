library(shiny)
library(shinydashboard)
library(DT)

# 导入数据源
load("rawdata.RData")
# 对字段自然周转化成有序因子变量
levels <- c(paste0("第",1:length(unique(rawdata$自然周)),"周"))
rawdata$自然周 <- factor(rawdata$自然周,
                      levels = levels,
                      ordered = T)

server <- function(input,output){
  # 原始数据展示
  output$rawdata <- DT::renderDataTable({
    datatable(rawdata)
  })
  # 原始数据下载
  output$downloadCsv <- downloadHandler(
    filename = "rawdata.csv",
    content = function(file) {
      write.csv(rawdata, file)
    },
    contentType = "text/csv"
  )
  
  # # 同一款游戏在不同渠道的打分模型
  selectdata <- reactive({
    rawdata[rawdata$自然周 >= input$firstweek &
              rawdata$游戏名称==input$game,]
  })
  # 计算周收入的得分模型
  channel_revenue <- reactive({
    channel_score(selectdata()[,c('渠道名称','自然周','周收入')])
  })

  # 计算周活跃用户的得分模型
  channel_active <- reactive({
    channel_score(selectdata()[,c('渠道名称','自然周','周活跃用户数')])
  })

  # 计算第7日留存率的得分模型
  channel_newretation <- reactive({
    channel_score(selectdata()[,c('渠道名称','自然周','第7日留存率')])
  })

  # 计算周付费率的得分模型
  channel_payrate <- reactive({
    channel_score(selectdata()[,c('渠道名称','自然周','周付费率')])
  })

  # 计算周ARPU的得分模型
  channel_arppu <- reactive({
    channel_score(selectdata()[,c('渠道名称','自然周','周ARPPU')])
  })

  # 计算综合得分
  channel_total <- reactive({
    input$w1*channel_revenue()+input$w2*channel_active()+
      input$w3*channel_newretation()+input$w4*channel_payrate()+
      input$w5*channel_arppu()
  })

  output$channel_data <- DT::renderDataTable({
    channel_data <- switch(input$data,
                           '1'=channel_total(),
                           '2'=channel_revenue(),
                           '3'=channel_active(),
                           '4'=channel_newretation(),
                           '5'=channel_payrate(),
                           '6'=channel_arppu())
    datatable(round(channel_data,2))
  })
  # 绘制生存曲线
  output$plot1 <- renderPlot({
    channel_total <- channel_total()
    channel_total$渠道名称 <- rownames(channel_total) # 增加渠道名称变量
    library(reshape)
    md <- melt(channel_total,id="渠道名称") #对数据进行重塑
    md$week <- ifelse(nchar(as.character(md$variable))==4,
                      substr(md$variable,2,3),substr(md$variable,2,2)) #增加周数变量
    md$week <- as.numeric(md$week)
    library(lattice)
    xyplot(value~week|渠道名称,data=md,type=c("l","g"),
           lwd=2,layout=c(6,2)) #绘制面板曲线图
  })
  # 提取最新的指标打分
  new_score <- reactive({
    data.frame("渠道"=rownames(channel_total()),
               "周收入指标得分"=channel_revenue()[,ncol(channel_revenue())],
               '周活跃指标得分'=channel_active()[,ncol(channel_active())],
               '第7日留存率指标得分'= channel_newretation()[, ncol(channel_newretation())],
               '周付费率指标得分'=channel_payrate()[,ncol(channel_payrate())],
               '周ARPPU指标得分'=channel_arppu()[,ncol(channel_arppu())],
               '指标综合得分'=channel_total()[,ncol(channel_total())])
  })
  output$new_score <- DT::renderDataTable({
    datatable(new_score())
  })
}