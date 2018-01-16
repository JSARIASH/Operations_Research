# SPSA algorithm. 
library(rgl)
library(plot3D)
library(shiny)
x <- seq(-5.2, 5.2, by = 0.1)
y <- x 
red <- mesh(x, y)
z <- (red$x ^ 2 + red$y ^ 2)
# Se suma un ruido. 
z1 <- z + matrix(runif(nrow(z) * ncol(z),  -1, 1 ), nrow = nrow(z))

# parámetros para calcular las ganancias (los parámetros del SPSA)
alpha <- 0.602 # da casi la respuesta con 0.2. el valor original es 0.602
GammaVari <- 0.101
A <- 100
a <- 0.16
# el valo de c debe ser analizado con más detenimiento. 
c <- sd(z) * 1000 # debe ser desviación estandar (http://www.jhuapl.edu/SPSA/PDF-SPSA/Spall_Implementation_of_the_Simultaneous.PDF) página 4. 
valu_ini <- runif(2, min(x), max(x))

ui <- fluidPage(theme="simplex.min.css",
                tags$h1("SPSA"),
                tags$style(type="text/css",
                           "label {font-size: 30px;}",
                           ".recalculating {opacity: 1.0;}"
                ),
                hr(),
                fluidRow(height = 800,
                  column(width = 5,  plotOutput("funcion")),
                  column(width = 5,  plotOutput("CurvasNivel")
                      )
                ),
                br(), # se deja un espacio
                hr(), # se pone una línea horizontal
                fluidRow(
                  column(6,offset = 1,
                         sliderInput(inputId = "k",
                                     label = "Iteraciones",
                                     min = 1, max = 500, value = 1, step = 1,
                                     animate = animationOptions(loop = FALSE, interval = 80)))
                )
)

server <- function(input, output, session){
  posicion <- reactiveValues(
    data = as.data.frame(valu_ini)
  )
  
 pos_actu <- eventReactive(input$k,{
   if (input$k == 1) {
     pos_actu <- posicion$data
   } else {
     ak <- a / (A + input$k) ^ (alpha)
     ck <- c / (input$k) ^ GammaVari
     pos_ante <- as.matrix(posicion$data)
     delta <- ifelse(runif(2) < 0.5, 1, -1)
     coordenadas_p <- pos_ante + ck * delta # coordenadas positivas
     coordenadas_n <- pos_ante - ck * delta # coordenadas negativas
     gradiente <- (coordenadas_p[1] ^ 2 + coordenadas_p[2] ^ 2) -
                  (coordenadas_n[1] ^ 2 + coordenadas_n[2] ^ 2)
     gradiente <- gradiente / (2 * ck * delta)
     pos_actu <- pos_ante - ak * gradiente
     posicion$data <- pos_actu
   }
   return(pos_actu)
 })
  
  # Se grafica la función 
  output$funcion <- renderPlot({
    surf3D(red$x, red$y, z, theta = 15, phi = 35, bty = "b",
           shade = 0.1,colvar = z, contour = TRUE, box = FALSE, colkey = TRUE)
    
  })
  # estas son las curvas de nivel. 
  output$CurvasNivel <- renderPlot({
    contour2D(z, x, y, levels = seq(0.08, 54, by = 1.5), colkey = FALSE, lw = 3)
    points(pos_actu()[1, 1], pos_actu()[2, 1], pch = 19,  cex = 1)
  })
  
}
shinyApp(ui, server)

   



