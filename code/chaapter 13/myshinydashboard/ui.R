library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "My First App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Chart", tabName = "chart", icon = icon("line-chart"))
  )),
  dashboardBody(
    tabItems(
      # 第一个标签内容
      tabItem(tabName = "data",
              fluidRow(
                box(title="iris数据集",status="success",
                    solidHeader = TRUE,collapsible=TRUE,
                    tableOutput("mytable")),
                box(title="滑动条",status="primary",solidHeader = TRUE,
                    collapsible=TRUE,background="purple",
                    numericInput("n","请选择要查看的数据行",value = 6))
              )),
      # 第二个标签内容
      tabItem(tabName = "chart",
              column(8,plotOutput("myplot"),
                     checkboxInput("outliers","显示异常点",FALSE))))
    
  )
)
