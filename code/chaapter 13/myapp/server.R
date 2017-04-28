shinyServer(function(input,output){
  # 向用户界面输出iris数据表，根据input$n输出行数
  output$mytable <- renderTable({
    head(iris,input$n)
  })
  # 用用户界面输出箱线图，并根据input$outliers是否输出异常点
  output$myplot <- renderPlot({
    boxplot(Sepal.Length~Species,data=iris,
            col=rainbow(3),outline = input$outliers)
  })
})