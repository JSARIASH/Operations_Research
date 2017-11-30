# Algunas funciones para probar la convergencia de los algoritmos se pueden encotrar en wikipedia. 
# https://en.wikipedia.org/wiki/Test_functions_for_optimization
rm(list = ls())
library(shiny)
library(rgl)
library(plot3D)
# comentario para subir el master

##### Variable requeridas para realizar el gráfico de la función a optimizar. #####

#### Función Rastering ####
x <- seq(-20.2, 20.2, by = 0.1)
y <- x
a <- mesh(x, y)
z <- 10*2 + (a$x^2 - 10*cos(2*pi*a$x) + a$y^2 - 10*cos(2*pi*a$y))


####Cross in tray Function. ####
# el dominio es -10 <= x,y <= 10. para los valores d1 y d2. 
# Tener encuenta que cuando se cambia la función objetivo también se cambia la manera en como esta es evaluada. (los z1)

# x <- seq(-10,10, by = 0.05)
# y <- x
# a <- mesh(x,y)
# z <- -0.0001*(abs(sin(a$x)*sin(a$y)*exp(abs(100 - sqrt(a$x^2 + a$y^2)/pi ))) + 1)^0.1

#### Parámetros y valores iniciales del enjambre. #### 
n_pariculas <- 10 # cantidad de partículas
d1 <- runif(n_pariculas, -10, 10) # Coordenadas para la primera dimensión. 
d2 <- runif(n_pariculas, -10, 10) # Coordenadas para la segunda demensión. 

#### z inciales de las funciones####
z1 <- 10*2+(d1^2 - 10*cos(2*pi*d1)+d2^2 - 10*cos(2*pi*d2)) # Función objetivo del enjambre. Rastering
#z1 <- -0.0001 * (abs(sin(d1) * sin(d2) * exp(abs(100 - sqrt(d1^2 + d2^2)/pi ))) + 1)^0.1 # Cross in Tray

vel1 <- vector(length = n_pariculas) # Vector de las velocidades. 
vel1[vel1 == FALSE] <- 0
vel2 <- vel1
d1A <- vel1
d2A <- vel1
z1A <- vel1
swarm  <- cbind(d1, d2, z1, vel1, vel2, d1A, d2A, z1A) # enjambre y función objetivo

# parámetros del algoritmo. 

c1 <- 1.2
c2 <- 1.2
r1 <- diag(runif(2), nrow =  2) # cuadrada respecato a la cantidad de variables. 
r2 <- diag(runif(2), nrow =  2) # cuadrada respecato a la cantidad de variables.

w_min <- 0.4 # Valores máximo y minímo para controlar la velocidad. 
w_max <- 0.99 # La velocidad va  decrecer de manera líneal. 

#### Mejor solución del enjambre ####
g_pos <- which(swarm[,3] == min(swarm[,3]))
G <- swarm[g_pos, 1:3]


ui <- fluidPage(theme="simplex.min.css",
                tags$h1("Particulas Swarm para solucionar algunas funciones de optimización conocidas."),
                tags$style(type="text/css",
                           "label {font-size: 30px;}",
                           ".recalculating {opacity: 1.0;}"
                ),
                hr(),
                fluidRow(
                  column(5, plotOutput("proyeccion", width = "90%")),
                  column(5, plotOutput("funcion", width = "110%")
                  )  
                ), 
                br(), # se deja un espacio
                hr(), # se pone una línea horizontal 
                fluidRow(
                  column(6,offset = 1,
                         sliderInput(inputId = "din",
                                     label = "canti",
                                     min = 1, max = 500,value = 1,step = 1,
                                     animate = animationOptions(loop = FALSE,interval = 200)))
                )
)


server <- function(input, output, session) {
  output$funcion <- renderPlot({
    surf3D(a$x,a$y,z,theta = 15,phi = 35,bty = "b",shade = 0.1,colvar = z)
  })
  

  #### se va a definir la matriz como  un reactiveValues ####

  
  particulas <- reactiveValues(data = as.data.frame(swarm))
  
  #### Mejor solución encontrada como reactivo para poder actualizarlo ####
  G_Opt <- reactiveValues(data = as.data.frame(G))  
  
  observeEvent(input$din,{
    if (input$din == 1) {
      particulas$data <- as.data.frame(swarm)
      G_Opt$data <- as.data.frame(G)
    }
  })
  
  #### cambia las partículas cada vez que se actualiza el slide. ####
  particles <-eventReactive(input$din,{
    # Se actualizan las velocidades y las posiciones. 
    # d1 y d2 representan las mejores personales. 
    G <- as.matrix(G_Opt$data)
    swarm <- as.matrix(particulas$data)
    
    ##### Se gráfica la posición inicial del las particulas.   ####
    if (input$din == 1){
      mat <- swarm[,1:2]
      particulas$data <- as.data.frame(swarm)
      G_Opt$data <- as.data.frame(G)
      
      # print(particulas$data)
      print(input$din)
      print(swarm)
      print(G)
      return(mat)
    } else if (input$din == 2){
      # Se actualiza la velocidad tentiendo en cuenta el óptimo, Diferencia entre la mejor posición del enjambre 
      # y cada una de las partículas. 
      
      # Las columnas 4 y 5 son las velocidades.   
      # factor de inercia se define por W. 
      W <-  w_max - ((w_max - w_min) / 500) * input$din # 500 máximo número de iteraciones (Definido en el slice). 
      swarm[,4:5] <- c2*((matrix(rep(G[1:2], n_pariculas), nrow = n_pariculas, byrow = TRUE) - swarm[,1:2]) %*% r2)
      swarm[,4:5] <- swarm[,4:5] * W
      swarm[,6:7] <- (swarm[,1:2] + swarm[,4:5])
      
      #### Se evalúa la función objetivo. ####
      
      # Rastering
      swarm[,8] <- 10*2 + (swarm[,6]^2 - 10*cos(2*pi*swarm[,6]) + swarm[,7]^2 - 10*cos(2*pi*swarm[,7])) # Rasterin
      
      # Cross in Tray
      #swarm[,8] <- -0.0001 * (abs(sin(swarm[, 6]) * sin(swarm[, 7]) * exp(abs(100 - sqrt(swarm[, 6] ^ 2 + swarm[, 7] ^ 2) / pi ))) + 1) ^ 0.1  # Cross in Tray
      
      # se identifica si hay menores
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
      
      print(input$din)
      print(swarm)
      print(G)
      # estas son las nuevas posiciones
      mat <- swarm[,6:7]
      # se actualiza el enjambre y la el óptimo. 
      particulas$data <- as.data.frame(swarm)
      G_Opt$data <- as.data.frame(G)
      return(mat)
    }else{
      swarm[,4:5] <-  swarm[, 4:5] + runif(1) * c1 * (swarm[,1:2] - swarm[, 6:7]) %*% r1 +
        c2*((matrix(rep(G[1:2],n_pariculas),nrow = n_pariculas,byrow = TRUE) - swarm[,6:7]) %*% r2)
      
      # factor de inercia. 
      W <-  w_max - ((w_max - w_min) / 500) * input$din
      swarm[,4:5] <- swarm[,4:5] * W
      
      swarm[,6:7] <- (swarm[,4:5] + swarm[,6:7]) %*% (diag(runif(2),nrow =  2)*1.8)
      
      #### Se evalúa la función objetivo. ####
      #Rastering
      swarm[,8] <- 10*2 + (swarm[,6]^2 - 10*cos(2*pi*swarm[,6]) + swarm[,7]^2 - 10*cos(2*pi*swarm[,7]))
      
      # Cross in Tray
      #swarm[,8] <- -0.0001 * (abs(sin(swarm[, 6]) * sin(swarm[, 7]) * exp(abs(100 - sqrt(swarm[, 6] ^ 2 + swarm[, 7] ^ 2) / pi ))) + 1) ^ 0.1  # Cross in Tray
      
      # se identifica si hay menores
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
      
      print(input$din)
      print(swarm)
      print(G)
      # estas son las nuevas posiciones
      mat <- swarm[,6:7]
      # se actualiza el enjambre y la el óptimo. 
      particulas$data <- as.data.frame(swarm)
      G_Opt$data <- as.data.frame(G)
      return(mat)
      
    }
    
  })
  
  output$proyeccion <- renderPlot({
    image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
            colkey = list(dist = .0, shift = 0.229,
                          side = 3, length = 0.3, width = 0.8,
                          cex.clab = 1.2, col.clab = "black", line.clab = 2,
                          col.axis = "black", col.ticks = "black", cex.axis = 0.8))
    
    points(particles()[,1],particles()[,2],cex = 1,pch = 19)
  })
}

shinyApp(ui, server)