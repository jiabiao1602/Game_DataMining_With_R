
data(MisLinks)
data(MisNodes)

function(input, output) { 
  # 利用lattice包中的绘图函数
  output$splom <- renderPlot({
    splom(mtcars[c(1, 3:7)], groups = mtcars$cyl,
          pscales = 0,pch=1:3,col=1:3,
          varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
                       "Gross\nhorsepower", "Rear\naxle\nratio",
                       "Weight", "1/4 mile\ntime"),
          key = list(columns = 3, title = "Number of Cylinders",
                     text=list(levels(factor(mtcars$cyl))),
                     points=list(pch=1:3,col=1:3)))
  })

  # 利用ggplot2包中的绘图函数
  output$myggplot <- renderPlot(({
    data(singer,package = "lattice")
    p <- ggplot(data=singer,aes(x=height,fill=voice.part))+
                geom_density()+
                facet_grid(voice.part~.)
  }))
  
  # 利用rCharts包中的绘图函数
  # nPlot函数
  output$mychart1 <- renderChart({
    hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
    hair_eye_male[,1] <- paste0("Hair",hair_eye_male[,1])
    hair_eye_male[,2] <- paste0("Eye",hair_eye_male[,2])
    p1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
    p1$chart(color = c('brown', 'blue', '#594c26', 'green'))
    p1$addParams(dom="mychart1")
    return(p1)
  })
  # hPlot函数
  output$mychart2 <- renderChart({
    a <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "Zoom demo", subtitle = "bubble chart", size = "Age", group = "Exer")
    a$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
    a$chart(zoomType = "xy")
    a$exporting(enabled = T)
    a$addParams(dom="mychart2")
    return(a)
  })
  
  # 社会网路图
  plotGraph <- function(){
    g <- make_graph( ~ A-B-C-D-A, E-A:B:C:D,
                     F-G-H-I-F, J-F:G:H:I,
                     K-L-M-N-K, O-K:L:M:N,
                     P-Q-R-S-P, T-P:Q:R:S,
                     B-F, E-J, C-I, L-T, O-T, M-S,
                     C-P, C-L, I-L, I-P)
    plotlayout <- switch(input$PlotLayout,
                         "Auto"=layout.auto(g),
                         "Random"=layout.random(g),
                         "Circle"=layout.circle(g),
                         "Sphere"=layout.sphere(g),
                         "Fruchterman Reingold"=layout.fruchterman.reingold(g),
                         "Kamada Kawai"=layout.kamada.kawai(g),
                         "Drl"=layout.drl(g),
                         "Spring"=layout.spring(g),
                         "Reingold Tilford"=layout.reingold.tilford(g),
                         "Fruchterma Reingold Grid"=layout.fruchterman.reingold.grid(g),
                         "Lgl"=layout.lgl(g),
                         "Graphopt"=layout.graphopt(g),
                         "SVD"=layout.svd(g)
    )
    if(!input$showNodeName){
      V(g)$label = ""
    }
    V(g)$size = input$vertexSize
    plot(g, layout=plotlayout)
  }
  output$graphPlot <- renderPlot({
    plotGraph()
  })
  # 下载社会网路图
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('NetworkGraph',format(Sys.time(),"%Y%m%d_%H%M%S"),'.pdf', sep='')
    },
    content = function(file) {
      pdf(file)
      print(suppressWarnings(plotGraph()))
      dev.off()
    }
  )
  
  #关联规则可视化
  output$groceryrules <- renderPlot({
    data(Groceries)
    groceryrules <- apriori(Groceries,parameter = 
                              list(support=0.006,confidence=0.25,minlen=2))
    plot(subset(groceryrules,lift > 3),method=input$method)
  })
  # 聚类结果可视化
  clusters <- reactive({
    kmeans(iris[,1:4],input$clusters)
  })
  output$kmeans <- renderPlot({
    plot(iris[,c('Sepal.Length','Sepal.Width')],
         col = clusters()$cluster,
         pch = 20, cex = 2)
    points(clusters()$centers, pch = 4, cex = 2, lwd = 4)
  })
  # 评价线性模型拟合情况可视化
  output$lm.fit <- renderPlot({
    fit <- lm(Sepal.Length~Sepal.Width,data=iris[,1:4])
    par(mfrow=c(2,2),pch="*")
    plot(fit)
  })
  
}