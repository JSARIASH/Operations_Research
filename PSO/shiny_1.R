library(shiny)
library(rgl)
library(plot3D)

ui <- fluidPage(theme="simplex.min.css",
                tags$h1("Particulas Swarm para solucionar algunas funciones de optimización conocidas."),
                tags$style(type="text/css",
                           "label {font-size: 30px;}",
                           ".recalculating {opacity: 1.0;}"
                ),
hr(),
fluidRow(
    column(5,plotOutput("proyeccion",width = "90%")),
    column(5, plotOutput("funcion",width = "110%")
 )  
), 
br(), # se deja un espacio
hr(), # se pone una línea horizontal 
fluidRow(
  column(6,offset = 1,
         sliderInput(inputId = "din",
                     label = "canti",
                     min = 1, max = 1000,value = 1,step = 1,
                     animate = animationOptions(loop = FALSE,interval = 100)))
  )
 )


server <- function(input, output, session) {
  output$funcion <- renderPlot({
    x <- seq(-5.2,5.2, by = 0.1)
    y <- x 
    a <- mesh(x,y)
    z <- 10*2+(a$x^2 - 10*cos(2*pi*a$x)+a$y^2 - 10*cos(2*pi*a$y))
    surf3D(a$x,a$y,z,theta = 15,phi = 35,bty = "b",shade = 0.1,colvar = z)
  })

  output$proyeccion <- renderPlot({
    x <- seq(-5.2,5.2, by = 0.1)
    y <- x 
    a <- mesh(x,y)
    z <- 10*2+(a$x^2 - 10*cos(2*pi*a$x)+a$y^2 - 10*cos(2*pi*a$y))
    image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
            colkey = list(dist = .0, shift = 0.229,
                          side = 3, length = 0.3, width = 0.8,
                          cex.clab = 1.2, col.clab = "black", line.clab = 2,
                          col.axis = "black", col.ticks = "black", cex.axis = 0.8))
    
    points(runif(input$din,-5.2,5.2),runif(input$din,-5.2,5.2),cex = .8,pch = 19)
  })
  
}

shinyApp(ui, server)
