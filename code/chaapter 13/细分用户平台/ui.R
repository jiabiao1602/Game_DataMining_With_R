library(shiny)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("用户分群研究",icon = icon("user-md"),tabName="dashboard",
             menuSubItem("金字塔模型",tabName="pyramid",icon=icon("apple")),
             menuSubItem("用户号码包查询",tabName="userid",icon=icon("table"))
             ),
   menuItem("潜在付费用户挖掘",icon = icon("heart"),tabName="payuser",
            menuSubItem("用户活跃度数据",tabName="actionuer",icon=icon("table")),
            menuSubItem("箱线图",tabName="boxplot",icon = icon("area-chart")),
            menuSubItem("决策树建模",tabName="treemodel",icon= icon("male"))
   )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pyramid",
            fluidRow(
              column(4,sliderInput("small","小R划分值",min=10,max=50,value=30,
                          step=10,animate=animationOptions(loop=T))),
              column(4,sliderInput("middle","中R划分值",min=100,max=150,value=100,
                          step=10,animate=animationOptions(loop=T))),
              column(4,sliderInput("big","大R划分值",min=500,max=600,value=100,
                          step=20,animate=animationOptions(loop=T))),
              valueBoxOutput("vbox0"),
              valueBoxOutput("small"),
              valueBoxOutput("middle"),
              valueBoxOutput("big"),
              valueBoxOutput("super")
            ),
            fluidRow(
              box(title = "用户分群金子塔图",
                width = 6, solidHeader = TRUE,
                background="maroon",
                plotOutput("barplot", width = "100%", height = 300)
              ),
              box(title = "用户分群金子塔图(针对付费用户）",
                  width = 6, solidHeader = TRUE,
                  background="aqua",
                  plotOutput("barplot1", width = "100%", height = 300)
              )
              )
            ),

    tabItem(tabName = "userid",
            selectInput("dataset", "选择需要查看的用户群:",
                        choices = c("小R"=1, "中R"=2, "大R"=3,"超R"=4)),
            box(
              title = "RaW Data", status = "primary", solidHeader = TRUE, width = 12, collapsible = T,
              DT::dataTableOutput("table")
            ),
            downloadButton("downloadCsv", "Download as CSV")
            ),

    tabItem(tabName="actionuer",
            DT::dataTableOutput("actionuer")),

    tabItem(tabName = "boxplot",
            plotOutput("boxplot1"),
            strong(em("从箱线图可以看出，非付费用户和付费用户的登陆次数存在明显的不同，说明利用登陆次数来研究用户是否付费是可行的。"))
            ),

    tabItem(tabName="treemodel",
            p("付费用户只占到所有活跃用户的14%，该份数据属于类失衡数据，在进行下一步建模之前需要进行数据抽样处理，达到类平衡！",
              style="color:green;"),
            code(em("1） 对数据进行分区，90%的数据作为测试集用来建模，10%的数据作为验证集用来验证模型。")),
            br(),
            code(em("2） 对测试集进行重新抽样，使得非付费：付费=6:4，达到类平衡。")),
            box(title="决策树形图",status = "primary", solidHeader = TRUE, width = 8,height=500,collapsible = T,
                plotOutput("treeplot")
                ),
            box(title="决策树规则解读",status = "info", solidHeader = TRUE, width = 4,height=500,collapsible = T,
                p("规则1：最后一周登陆天数 = 1 and 最后一周登陆次数 <= 3:",style="color:maroon;"),
                p("符合规则的玩家有35683个，其中有2316位付费玩家，付费率是6.5%。"),
                br(),
                p("最后一周登陆天数 =1 and 最后一周登陆次数 > 3:",style="color:maroon;"),
                p("符合规则的玩家有4430个，其中付费玩家有2548个，付费率是57.5%,故可以认为该规则是付费玩家节点。"),
                br(),
                p("最后一周登陆天数 >=1 and 最后一周登陆次数 > 6:",style="color:maroon"),
                p("符合规则的玩家有28747个，其中付费玩家有22322个，付费率是77.6%,故可以认为该规则是付费玩家节点。")
            ),
            p(strong(em("总结：最后一周登陆天数 =1 and 最后一周登陆次数 > 3 or 最后一周登陆天数大于1天,且最后一周的登陆次数大于5次的玩家可以认为是高潜在付费用户，运营可以针对这批用户进行活动推送，促使玩家转化！")))
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title="用户细分平台"),
  sidebar,
  body,
  skin="blue"
)