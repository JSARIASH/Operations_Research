# Sirve para mira la cantida de digitos. 
# options(digits = )
# 
# m <- matrix(nrow = 4, ncol = 3)
# m[1,1] <- 0
# m[1,2] <- 2
# m[1,3] <- 1
# m[2,1] <- 5
# m[2,2] <- 1
# m[2,3] <- 1
# m[3,1] <- 0
# m[3,2] <- -1
# m[3,3] <- 1
# m[4,1] <- 21
# m[4,2] <- 6
# m[4,3] <- 2

## Ejerciciso gommory.
# m <- matrix(nrow = 3, ncol = 3)
#   m[1,1] <- 0
#   m[1,2] <- 1
#   m[1,3] <- 5
#   m[2,1] <- 20
#   m[2,2] <- 1
#   m[2,3] <- 10
#   m[3,1] <- 20
#   m[3,2] <- 10
#   m[3,3] <- 1
  
## ejercicio gommory 
  # m <- matrix(nrow = 3, ncol = 3)
  # m[1,1] <- 0
  # m[1,2] <- -2
  # m[1,3] <- -2
  # m[2,1] <- -3
  # m[2,2] <- -1
  # m[2,3] <- -2
  # m[3,1] <- -6
  # m[3,2] <- -3
  # m[3,3] <- -2
    
# m <- matrix(nrow = 4, ncol = 3)
# m[1,1] <- 0
# m[1,2] <- 3
# m[1,3] <- 5
# m[2,1] <- 4
# m[2,2] <- 1
# m[2,3] <- 0
# m[3,1] <- 6
# m[3,2] <- 0
# m[3,3] <- 1
# m[4,1] <- 18
# m[4,2] <- 3
# m[4,3] <- 2

rm(list = ls())
source("~/Documents/Directorio_R/IO/Gommory/cuadro_optimo.R")
source("~/Documents/Directorio_R/IO/Gommory/parte_entera.R")

m <- matrix(c(0,120,80,6,2,1,28,7,8),nrow = 3,ncol = 3, byrow = T)

m <- matrix(c(0,1,1,5,3,2,2,0,1),nrow = 3,ncol = 3, byrow = T)
opt <- cuadro_optimo(m)
opt
cortes <- parte_entera(opt)
#cortes <- fractions(cortes)
colnames(cortes) <- colnames(opt)
rownames(cortes) <- rownames(opt)

# se ingresa el primer corte. 
m2 <- rbind(opt,c(-1/3,-1/3,-1/3))
rownames(m2)[4] <- "X5"

# se resuleve 
opt2 <- cuadro_optimo(m2)
cortes2 <- parte_entera(opt2)
#fractions(cortes2)

# Se ingresa el segundo corte
m3 <- rbind(opt2,c(-1/2,0,-1/2))
rownames(m3)[5] <- "X6"
#fractions(m3)

opt3 <- cuadro_optimo(m3)
cortes3 <- parte_entera(opt3) # hAY ERROR DE PRECISION. 
fractions(cortes3)

m4 <- rbind(opt3,c(-5/6,-11/108,-107/108))
rownames(m4)[6] <- "X7"
opt4 <- cuadro_optimo(m4)

cortes4 <- parte_entera(opt4)
m5 <- rbind(opt4,c(-9/11,-9/11,-3/11))
rownames(m5)[7] <- "X8"

opt5 <- cuadro_optimo(m5)



# Ejemplo tomado del Wayne

m <- matrix(c(0,8,5,6,1,1,45,9,5),nrow = 3, byrow = T)
opt <- cuadro_optimo(m)
cortes <- parte_entera(opt)

m2 <- rbind(opt,-cortes[3,])
opt2 <- cuadro_optimo(m2)
