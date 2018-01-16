# SPSA algorithm. 
# Stochastic optimization. 
par(bg = "gray19")
library(rgl)
library(plot3D)
x <- seq(-5.2,5.2, by = 0.1)
y <- x 
a <- mesh(x,y)
z <- (a$x ^ 2 + a$y ^ 2)
# Se suma un ruido. 
z1 <- z + matrix(runif(nrow(z) * ncol(z),  -1, 1 ), nrow = nrow(z))
surf3D(a$x, a$y, z, theta = 15, phi = 35, bty = "b",
       shade = 0.1,colvar = z, contour = TRUE, box = FALSE, colkey = FALSE)
image2D(z,x,y,clab = "f(xy)",rasterImage = TRUE,
        colkey = list(dist = .0, shift = 0.229,
                      side = 3, length = 0.3, width = 1,
                      cex.clab = 1.2, col.clab = "black", line.clab = 2,
                      col.axis = "black", col.ticks = "black", cex.axis = 0.8), 
        contour = TRUE)
contour2D(z, x, y, levels = seq(0.08, 54, by = 1.5), colkey = FALSE)
contour2D(z1, x, y, levels = seq(0.08, 54, by = 1.5), colkey = FALSE)
points(0,0, pch = 19, col = "white", cex = 1)        

# Se inicia con un valor de los parámetros. Recordar este métod es una aproximación 
# del gradiente
contour2D(z, x, y, levels = seq(0.08, 54, by = 1.5), colkey = FALSE)
valu_ini <- runif(2, min(x), max(x))
points(valu_ini[1],valu_ini[2], pch = 19, col = "white", cex = .5)
# parámetros para calcular las ganancias (los parámetros del SPSA)
alpha <- 0.602 # da casi la respuesta con 0.2
GammaVari <- 0.101
A <- 100
a <- 0.16
# el valo de c debe ser analizado con más detenimiento. 
c <- sd(z) * 1000 # debe ser desviación estandar (http://www.jhuapl.edu/SPSA/PDF-SPSA/Spall_Implementation_of_the_Simultaneous.PDF) página 4. 

for(k in 1:300){
  ak <- a / (A + k) ^ (alpha)
  ck <- c / (k) ^ GammaVari
  delta <- ifelse(runif(2) < 0.5, 1, -1)
  coordenadas_p <- valu_ini + ck * delta # coordenadas positivas
  coordenadas_n <- valu_ini - ck * delta # coordenadas negativas
  gradiente <- (coordenadas_p[1] ^ 2 + coordenadas_p[2] ^ 2) -
               (coordenadas_n[1] ^ 2 + coordenadas_n[2] ^ 2)
  gradiente <- gradiente / (2 * ck * delta)
  valu_ini <- valu_ini - ak * gradiente
  points(valu_ini[1],valu_ini[2], pch = 19, col = "salmon", cex = .2)
  #points(valu_ini[1], valu_ini[2], type = "l", col = "red")
  }
valu_ini




        