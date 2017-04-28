library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme=shinytheme("cerulean"),
  titlePanel("My First App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n","请选择要查看的数据行",value = 6),
      checkboxInput("outliers","显示异常点",FALSE),
      a(img(src="bigorb.png",height=60,width=60),
        href="https://www.rstudio.com/",target="black")),
    mainPanel(
      strong(h3("这是我的第一个app应用",style="color:violetred")),
      h4("如果想学习更多的shiny知识",
        span(a("请点击此处",href="http://shiny.rstudio.com/tutorial/"))),
      br(),
      p("我们将利用鸢尾花数据集进行案例演示"),
      tableOutput("mytable"),
      column(8,plotOutput("myplot"))
      ))
))
