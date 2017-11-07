library(shiny)
library(rgl)
library(plot3D)


n_pariculas <- 200
d1 <- runif(n_pariculas,-15.2,15.2)
d2 <- runif(n_pariculas,-15.2,15.2)
z1 <- 10*2+(d1^2 - 10*cos(2*pi*d1)+d2^2 - 10*cos(2*pi*d2))
vel1 <- vector(length = n_pariculas)
vel1[vel1 == FALSE] <- 0
vel2 <- vel1
d1A <- vel1
d2A <- vel1
z1A <- vel1
swarm  <- cbind(d1,d2,z1,vel1,vel2,d1A,d2A,z1A) # enjambre y función objetivo

# parámetros para las partículas. 
c1 <- .01
c2 <- .01
r1 <- diag(runif(2),nrow =  2) # cuadrada respecato a la cantidad de variables. 
r2 <- diag(runif(2),nrow =  2) # cuadrada respecato a la cantidad de variables.

# Mejor solución del enjambre. 
g_pos <- which(swarm[,3]==min(swarm[,3]))
G <- swarm[g_pos,1:3]


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
                     animate = animationOptions(loop = FALSE,interval = 50)))
  )
 )


server <- function(input, output, session) {
  output$funcion <- renderPlot({
    x <- seq(-15.2,15.2, by = 0.1)
    y <- x 
    a <- mesh(x,y)
    z <- 10*2+(a$x^2 - 10*cos(2*pi*a$x)+a$y^2 - 10*cos(2*pi*a$y))
    surf3D(a$x,a$y,z,theta = 15,phi = 35,bty = "b",shade = 0.1,colvar = z)
  })

  # se va a deginir la matriz como  un reactiveValues 
  
  particulas <- reactiveValues(data = as.data.frame(swarm))
  observeEvent(input$din,{
      if (input$din == 1){
        particulas$data <- as.data.frame(swarm)
      }
    })
  
 
   # cambia las partículas cada vez que se actualiza el slide. 
  particles <-eventReactive(input$din,{
    
    
    # Se actualizan las velocidades y las posiciones. 
    # d1 y d2 representan las mejores personales. 
    swarm <- as.matrix(particulas$data)
      if (input$din == 1){
        # Se actualiza la velocidad tentiendo en cuenta el óptimo. 
        swarm[,4:5] <- c2*((matrix(rep(G[1:2],n_pariculas),nrow = n_pariculas,byrow = TRUE) - swarm[,1:2])%*% r2)
        swarm[,6:7] <- swarm[,1:2] + swarm[,4:5]
      }else{
        swarm[,4:5] <- swarm[,4:5] + runif(1)*c1*(swarm[,1:2] - swarm[,4:5]) %*% r1 +
          c2*((matrix(rep(G[1:2],n_pariculas),nrow = n_pariculas,byrow = TRUE) - swarm[,4:5]) %*% r2)
        swarm[,6:7] <- swarm[,4:5] + swarm[,6:7] 
      }
      swarm[,8] <- 10*2 + (swarm[,6]^2 - 10*cos(2*pi*swarm[,6]) + swarm[,7]^2 - 10*cos(2*pi*swarm[,7]))
      menores <- which(swarm[,8] < swarm[,3])
      # si en la columna 8 todos son mayores la longitud es cero
      # se acualizan las posiciones a una mejor. 
      if (length(menores) != 0){
        swarm[menores,1:3] <- swarm[menores,6:8]
      }
      if (G[3] > min(swarm[,8])){
        p_update <- which(swarm[,8] == min(swarm[,8]))
        G <- swarm[p_update,6:8]
      }
    
    mat <- swarm[,1:2]
    particulas$data <- as.data.frame(swarm)
    print(swarm)
    print(G)
    return(mat)
  })

  output$proyeccion <- renderPlot({
    x <- seq(-15.2,15.2, by = 0.1)
    y <- x 
    a <- mesh(x,y)
    z <- 10*2+(a$x^2 - 10*cos(2*pi*a$x)+a$y^2 - 10*cos(2*pi*a$y))
    image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
            colkey = list(dist = .0, shift = 0.229,
                          side = 3, length = 0.3, width = 0.8,
                          cex.clab = 1.2, col.clab = "black", line.clab = 2,
                          col.axis = "black", col.ticks = "black", cex.axis = 0.8))
    
    points(particles()[,1],particles()[,2],cex = 1,pch = 19)
  })
  
  # observe({
  # 
  # })
  
}

shinyApp(ui, server)










# runApp(shinyApp(
#   ui=(fluidPage(
#     titlePanel("amend data frame"),
# 
#     mainPanel(
#       fileInput("file", "Upload file"),
# 
#       numericInput("Delete", "Delete row:", 1, step = 1),
#       actionButton("Go", "Delete!"),
# 
#       tableOutput("df_data_out")
#     )
#   )),
#   server = (function(input, output) {
#     values <- reactiveValues(df_data = NULL)
# 
#     observeEvent(i {
#       values$df_data <- read.csv(input$file$datapath)
#     })
# 
#     observeEvent(input$Go, {
#       temp <- values$df_data[-input$Delete, ]
#       values$df_data <- temp
# 
#     })
# 
#     output$df_data_out <- renderTable(values$df_data)
#   })))









