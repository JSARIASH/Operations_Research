library(shiny)

ui <- fluidPage(theme="simplex.min.css",
                tags$h1("Particulas Swarm para solucionar algunas funciones de optimización conocidas."),
                tags$style(type="text/css",
                           "label {font-size: 30px;}",
                           ".recalculating {opacity: 1.0;}"
                ),
hr(),
fluidRow(
    column(6,plotOutput("dis")),
    column(6,plotOutput("gra")
 )  
), 
br(), # se deja un espacio
hr(), # se pone una línea horizontal 
fluidRow(
  column(6,offset = 1,
         sliderInput(inputId = "din",
                     label = "canti",
                     min = 1, max = 1000,value = 1,step = 1,
                     animate = animationOptions(loop = FALSE,interval = 100))),
  column(5,
         sliderInput(inputId =  "num_clas",
                     label = "número",
                     value = 9, min = 1, max = 20)
  ),
  column(5,
         sliderInput(inputId = "num_dat",
                     label = "q datos",
                     value = 100, min = 50, max = 1000)
  )
  )
 )


server <- function(input, output, session) {
  output$gra <- renderPlot({
    hist(rnorm(input$num_dat),nclass = input$num_clas, col = "red",
         border = "darkred")
  })
  output$dis <- renderPlot({
    plot(runif(input$din),runif(input$din),pch = 19, cex = 1, col = "navy",
         axes = FALSE)
  })
  
}

shinyApp(ui, server)
