library(shiny)
library(shinydashboard)
library(lattice)
library(ggplot2)
library(recharts)
library(rCharts)
library(igraph)
library(arules)
library(arulesViz)

dashboardPage(
  dashboardHeader(title="数据可视化平台demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("基础绘图展示",tabName = "basedemo"),
      menuItem("rCharts绘图展示",tabName = "rChartsdemo"),
      menuItem("社会网络图展示",tabName = "SNdemo"),
      menuItem("模型结果可视化展示",tabName = "viz")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "basedemo",
              box(title = "lattice包绘制散点图矩阵",
                  width = 6,solidHeader = TRUE,
                  background = "light-blue", plotOutput("splom")),
              box(title = "ggplot2包绘制的密度图",
                  width = 6,solidHeader = TRUE,
                  background = "light-blue", plotOutput("myggplot"))),
      tabItem(tabName = "rChartsdemo",
              box(title = "nPlot函数demo演示",
                  bsolidHeader = TRUE,collapsible = TRUE,width="100%",
                  showOutput("mychart1","nvd3")),
              box(title = "hPlot函数demo演示",
                  bsolidHeader = TRUE,collapsible = TRUE,width="100%",
                  showOutput("mychart2","highcharts"))),
      tabItem(tabName = "SNdemo",
              fluidRow(
                column(12, wellPanel(
                  plotOutput("graphPlot")
                )),
                column(4, wellPanel(
                  radioButtons(inputId="PlotLayout", label="Plot Layout",
                               choices=c("Auto","Random","Circle","Sphere","Fruchterman Reingold",
                                         "Kamada Kawai","Drl","Spring","Reingold Tilford",
                                         "Fruchterma Reingold Grid",
                                          "Lgl","Graphout","SVD"), selected="Auto")
                )),
                column(4, wellPanel(
                  checkboxInput(inputId = "showNodeName",
                                label = "Show Vertex Label",  value = TRUE),
                  sliderInput(inputId = "vertexSize", 
                              label = "Vertex Size",  value = 15, min=1, max=100)
                )),
                column(4, wellPanel(
                  downloadButton('downloadPlot', 'Download Plot in pdf')
                )))),
      tabItem(tabName = "viz",
              box(title = "关联规则可视化",
                  width = 6,solidHeader = TRUE,collapsible = T,
                  background = "red",plotOutput("groceryrules")),
              box(title = "kmeans结果可视化",
                  width = 6,solidHeader = TRUE,collapsible = T,
                  background = "red",plotOutput("kmeans")),
              column(6,selectInput(inputId="method",label="请选择关联规则可视化的method",
                                   choices=c("graph","scatterplot","two-key plot", "matrix",  
                                             "matrix3D","paracoord"))),
              column(6,numericInput('clusters', '请选择k值', 3,
                                    min = 1, max = 9)),
              column(12,box(title = "评价线性模型拟合情况可视化",status="primary",
                            solidHeader = TRUE,collapsible = T,width="100%",
                            plotOutput("lm.fit"))))
    )))
