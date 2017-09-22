m <- matrix(c(0,2,1,5,1,1,0,-1,1,21,6,2),nrow = 4,byrow = TRUE)
opt <- cuadro_optimo(m)
fractions(opt)
cortes <- parte_entera(opt)
fractions(cortes)

updt_opt <- rbind(opt,-cortes[4,])
rownames(updt_opt)[5] <- "X6"
fractions(updt_opt)


opt2 <- cuadro_optimo(updt_opt)
fractions(opt2)
cortes2 <- parte_entera(opt2)
fractions(cortes2)
updt_opt2 <- rbind(opt2,-cortes2[3,])
rownames(updt_opt2)[6] <- "X7"
fractions(updt_opt2)


opt3 <- cuadro_optimo(updt_opt2)
fractions(opt3)
