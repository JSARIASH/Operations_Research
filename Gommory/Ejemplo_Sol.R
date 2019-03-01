m <- matrix(c(0,2,1,5,1,1,0,-1,1,21,6,2),nrow = 4,byrow = TRUE)
opt <- cuadro_optimo(m)
fractions(opt)
cortes <- parte_entera(opt)
fractions(cortes)

updt_opt <- rbind(opt,-cortes[4,])
rownames(updt_opt)[5] <- "X6"
fractions(updt_opt)


mat <- matrix(c(0, 8, 5, 6, 1, 1, 45, 9, 5),nrow = 3, byrow = TRUE)
opt <- cuadro_optimo(mat)
c1<- parte_entera(opt)
fractions(c1)
opt2 <- rbind(opt, c(-3/4,-1/4,-3/4))
fractions(opt2)
rownames(opt2)[4] <- "X5"
fractions(opt2)
opt2 <- cuadro_optimo(opt2)
fractions(opt2)

mat <- matrix(c(0, 8, 5, 6, 1, 1, 45, 9, 5),nrow = 3, byrow = TRUE)
opt <- cuadro_optimo(mat)
c1<- parte_entera(opt)
fractions(c1)
opt2 <- rbind(opt, c(-3/4,-1/4,-3/4))
fractions(opt2)
rownames(opt2)[4] <- "X5"
fractions(opt2)
opt2 <- cuadro_optimo(opt2)
fractions(opt2)



mat <- matrix(c(0,3,4,3,2/5,1,1,2/5,-2/5),nrow = 3, byrow = TRUE)
mat
opt <- cuadro_optimo(mat)
fractions(opt)
opt
fractions(opt)
c <- parte_entera(opt)
fractions(c)
opt2 <- rbind(opt,  c(-13/14,-5/7, -11/14))
fractions(opt2)
rownames(opt2)[4] <- "X5"
fractions(opt2)
opt2<- cuadro_optimo(opt2)
opt2
fractions(opt2)
c <- parte_entera(opt2)
fractions(c)
opt3 <- rbind(opt2,  c(-3/10, -3/5, -1/10))
fractions(opt3)
rownames(opt3)[5] <- "X6"
fractions(opt3)
opt3 <- cuadro_optimo(opt3)
opt3
fractions(opt3)
c <- parte_entera(opt3)
fractions(c)
opt4 <- rbind(opt3,  c(-6/7, -1/3, -2/3))
fractions(opt4)
rownames(opt4)[6] <- "X7"
fractions(opt4)
opt4 <- fractions(cuadro_optimo(opt4))


opt5 <- rbind(opt4,  c(-5/7, -1/2, -1/2))
rownames(opt5)[7] <- "X8"
fractions(opt5)

opt6 <- fractions(cuadro_optimo(opt5))
opt6
c <- parte_entera(opt6)

opt7 <- rbind(opt6, c(-6/7, -1/5, 0))
opt7
fractions(opt7)
rownames(opt7)[8] <- "X9"
fractions(opt7)

opt8 <-cuadro_optimo(opt7)
