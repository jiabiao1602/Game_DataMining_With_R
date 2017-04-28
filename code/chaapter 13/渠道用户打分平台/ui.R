library(shiny)
library(shinydashboard)
library(DT)

# 导入数据源
load("rawdata.RData")
# 提取游戏名称和渠道名称
levels <- c(paste0("第",1:length(unique(rawdata$自然周)),"周"))
game <- levels(rawdata$游戏名称)
channel <- levels(rawdata$渠道名称)

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
      tabItem(tabName = "Data",
              tabBox(width = 12,
                     tabPanel("原始数据查询",
                              DT::dataTableOutput("rawdata"),
                              downloadButton("downloadCsv", "Download as CSV")
                              ),
                     tabPanel("同一款游戏在不同渠道的打分模型",
                              box(
                                title = "参数选择", status = "primary", solidHeader = TRUE, width = 16, collapsible = T,
                                column(4,selectInput("firstweek","请选择起始周",
                                                     levels,selected="第18周")),
                                column(4,selectInput("game","请选择需要查看的游戏",game)),
                                column(4,selectInput("data","请选择需要查看的指标得分",
                                                     c("指标综合得分"=1,
                                                       "周收入指标得分"=2,
                                                       "周活跃用户指标得分"=3,
                                                       "第7日留存率指标得分"=4,
                                                       "周付费率指标得分"=5,
                                                       "周ARPPU指标得分"=6)))),
                              box(
                                title = "权重设置", status = "primary", solidHeader = TRUE, width = 16, collapsible = T,
                                column(2,numericInput("w1","周收入权重",value=0.3,min=0,max=1,step=0.1)),
                                column(2,numericInput("w2","周活跃用户数权重",value=0.2,min=0,max=1,step=0.1)),
                                column(2,numericInput("w3","第7日留存率权重",value=0.2,min=0,max=1,step=0.1)),
                                column(2,numericInput("w4","周付费率权重",value=0.15,min=0,max=1,step=0.1)),
                                column(2,numericInput("w5","周ARPPU权重",value=0.15,min=0,max=1,step=0.1))
                                ),
                              box(
                                title = "指标得分", status = "primary", solidHeader = TRUE, width = 16, collapsible = T,
                                DT::dataTableOutput("channel_data")),                              
                              box(
                                  title = "渠道生存曲线", status = "primary", solidHeader = TRUE, width = 16, collapsible = T,
                                  plotOutput("plot1")),
                              box(
                                title = "最近一周得分统计", status = "primary", solidHeader = TRUE, width = 16, collapsible = T,
                                DT::dataTableOutput("new_score"))
                              ),
                     tabPanel('帮助文档',
                              h4("打分目的：们需要利用渠道运营数据中的关键指标数据，构建通用的渠道用户质量评价体系，对渠道质量进行综合打分评级。",
                                 style = "font-family: 'times'; font-si16pt;color:green"),
                              code("指标说明：以自然周为研究周期，选取周收入、周活跃、周ARPPU、周付费渗透率、7日留存率作为五大基础指标，通过计算各个指标的信度和效度，对渠道用户的基础指标进行打分"),
                              h4("打分规则如下：",style = "color:green"),
                              p("1)	指标初始分都是10分；"),
                              p("2)	波动性得分=5*（本周实际值-上周实际值）/最近四周的最大值"),
                              p("3)	量级得分=5*渠道本周值/所有渠道本周总值(只有周收入、周活跃有量级指标)"),
                              p("4）指标得分=上周得分+波动性得分+量级得分"),
                              p("5）综合得分=0.3*周收入指标得分+0.2*周活跃指标得分+0.2*第7日留存率指标得分+0.15*周付费率指标得分+0.15*周ARPPU指标得分")
                              )
                     ))))