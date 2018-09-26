m <- matrix(c(0,2,1,5,1,1,0,-1,1,21,6,2),nrow = 4,byrow = TRUE)
opt <- cuadro_optimo(m)
fractions(opt)
cortes <- parte_entera(opt)
fractions(cortes)

updt_opt <- rbind(opt,-cortes[4,])
rownames(updt_opt)[5] <- "X6"
fractions(updt_opt)


