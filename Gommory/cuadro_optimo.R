# esto es una nueva línea que se puso para poner a prueba git. 

#####################################################################
######## ESTE FUE EL CHECKCAUTO QUE REALICE EN EL PRIMER COMMMIT####
#####################################################################

#####################################################################
######## EN ETA RAMA SOLO HAYE ESTO#################################
###################################################################

# m es la matriz con los datos
# m <- matrix(nrow = 5, ncol = 3)
# m[1,1] <- 0
# m[1,2] <- 60
# m[1,3] <- 50
# m[2,1] <- 80
# m[2,2] <- 2
# m[2,3] <- 4
# m[3,1] <- 55
# m[3,2] <- 3
# m[3,3] <- 2
# m[4,1] <- 16
# m[4,2] <- 1
# m[4,3] <- 0
# m[5,1] <- 18
# m[5,2] <- 0
# m[5,3] <- 1

# m <- matrix(nrow = 4, ncol = 4)
# m[1,1] <- 0
# m[1,2] <- -315
# m[1,3] <- -110
# m[1,4] <- -50
# m[2,1] <- -200
# m[2,2] <- -15
# m[2,3] <- -2
# m[2,4] <- -1
# m[3,1] <- -150
# m[3,2] <- -7.5
# m[3,3] <- -3
# m[3,4] <- -1
# m[4,1] <- -120
# m[4,2] <- -5
# m[4,3] <- -2
# m[4,4] <- -1

# m <- matrix(nrow = 4, ncol = 5)
# m[1,1] <- 0
# m[1,2] <- -2
# m[1,3] <- -3
# m[1,4] <- -4
# m[1,5] <- -5
# m[2,1] <- -10
# m[2,2] <- -1
# m[2,3] <-  1
# m[2,4] <- -1
# m[2,5] <-  1
# m[3,1] <- -6
# m[3,2] <- -1
# m[3,3] <-  2
# m[3,4] <- -3
# m[3,5] <-  4
# m[4,1] <- -15
# m[4,2] <- -3
# m[4,3] <-  4
# m[4,4] <- -5
# m[4,5] <-  6

# m <- matrix(nrow = 3,ncol = 4)
# m[1,1] <- 0
# m[1,2] <- -4
# m[1,3] <- -12
# m[1,4] <- -18
# m[2,1] <- -3
# m[2,2] <- -1
# m[2,3] <- 0
# m[2,4] <- -3
# m[3,1] <- -5
# m[3,2] <- 0
# m[3,3] <- -2
# m[3,4] <- -2

# m <- matrix(nrow = 4,ncol = 3)
# m[1,1] <- 0
# m[1,2] <- -2
# m[1,3] <- -1
# m[2,1] <- -3
# m[2,2] <- -3
# m[2,3] <- -1
# m[3,1] <- -6
# m[3,2] <- -4
# m[3,3] <- -3
# m[4,1] <- 3
# m[4,2] <- 1
# m[4,3] <- 2

# m <- matrix(nrow = 3,ncol = 4)
# m[1,1] <- 0
# m[1,2] <- -160
# m[1,3] <- -120
# m[1,4] <- -280
# m[2,1] <-  -1
# m[2,2] <-  -2
# m[2,3] <-  -1
# m[2,4] <-  -4
# m[3,1] <-  -3/2
# m[3,2] <-  -2
# m[3,3] <-  -2
# m[3,4] <-  -2
  
# m <- matrix(nrow = 3,ncol = 4)
# m[1,1] <- 0
# m[1,2] <- -2
# m[1,3] <- -1
# m[1,4] <-  0
# m[2,1] <-  -4
# m[2,2] <-   -2
# m[2,3] <-  2
# m[2,4] <-  1
# m[3,1] <-  -6
# m[3,2] <-   1
# m[3,3] <-   2
# m[3,4] <-  -1

# m <- matrix(nrow = 3,ncol = 4)
#   m[1,1] <-  0
#   m[1,2] <- -5 
#   m[1,3] <- -35
#   m[1,4] <- -20
#   m[2,1] <- -2
#   m[2,2] <-  1
#   m[2,3] <-  -1
#   m[2,4] <-  -1
#   m[3,1] <-  -3
#   m[3,2] <-  -1
#   m[3,3] <-  1
#   m[3,4] <-  0

cuadro_optimo <- function(m){
library(MASS)
dimen <- dim(m) 

 

if (is.null(colnames(m))){
  colnames(m) <-c("RHS",paste("X",1:(dimen[2]-1),sep=""))
  rownames(m)<- c("X0",paste("X",(dimen[2]):(dimen[1]+dimen[2]-2),sep=""))
}
 while (any(m[1,2:dimen[2]] > 0) == TRUE | any(m[2:dimen[1],1] < 0) == TRUE){
    
    # Todos los RHS deben ser positivos. 
   if (all(m[2:dimen[1],1] >= 0)) {
       # se toma la variable más positiva para entrear a la base. 
       # Se toma el indice de la columna. 
       entra <- which(m[1,2:dimen[2]]==max(m[1,2:dimen[2]])) + 1
       
       # si hay más de un valor se escoge el primero de manera arbitraria. 
       if (length(entra) > 1){
         entra <- entra[1]
       }
       
       # Se busca la variable a salir de la base. 
       # RHS / columna entrante, Se escoge el minimo mayor a cero. 
       sale <- m[2:dimen[1],1] / m[2:dimen[1],entra]
       
       # Se rompe el ciclo si hay solución infactible. 
       if (all(sale <= 0)){
         break
       }
       sale <- which(sale == min(sale[sale > 0])) + 1
       # si hay más de un valor se escoge el primero de manera arbitraria. 
       if (length(sale) > 1){
         sale <- sale[1]
       }
       
       # se van a acutaliza los elementos diferentes a la columna y fila pivote.
       
       for(i in 1:dimen[1]){
         for(j in 1:dimen[2]){
           if (i != sale & j != entra){
               m[i,j] <- m[i,j] - (m[i,entra]*m[sale,j])/m[sale,entra]
           }
         }
       }
       # se actualiza la fila y la columna pivote. 
       # Columna pivote se divide entre el elemto pivote y se le cambia el signo. 
       ele_pivote <- m[sale,entra]
       m[,entra] <- m[,entra]/ele_pivote * -1
       
       # La fila pivote se divide entre el elemento pivote. 
       m[sale,] <- m[sale,]/ele_pivote
       
       # Se actualiza el elemento pivote
       m[sale,entra] <- 1/ele_pivote
       
       # se actualiza los nombres de la variable que entra y la que sale. 
       sale_tmp <- rownames(m)[sale]
       rownames(m)[sale] <- colnames(m)[entra]
       colnames(m)[entra] <- sale_tmp
 }else{ 
   # Si hay un RHS negativo a parte de z se reaiza dual simplex. 
   # En el dual simplex sale la mas negativa. 
   sale <- which(m[2:dimen[1],1]==min(m[2:dimen[1],1])) + 1
   
   # Si hay más de dos se escoge la primera de manera arbitraria. 
   if (length(sale) > 1){
     sale <- sale[1]
   }
   
   # Entra el mínimo cociente mayor o igual a cero entre el z y la fila pivote.
   # Solo se tienen en cuenta los valores negativos de la fila pivote. 
   
   entra <- m[1,2:dimen[2]]/m[sale,2:dimen[2]] 
   # se buscan los valores menores a cero de la fila pivote. Puede dar negativo 
   # incluso si la fila pivote es positiva. 
   negativos <- which(m[sale,2:dimen[2]] < 0)
   # Se rompe el ciclo si hay solución infactible. 
   # if (all(entra[negativos] < 0)){
   #   break
   # }
   ind_neg <- which(entra[negativos] == min(entra[negativos]))
   
   # Indice de la variable en la columna que entra. 
   entra <- negativos[ind_neg] + 1
   # si hay más de un valor se escoge el primero de manera arbitraria. 
   if (length(entra) > 1){
     entra <- entra[1]
   }
   
   for(i in 1:dimen[1]){
     for(j in 1:dimen[2]){
       if (i != sale & j != entra){
         m[i,j] <- m[i,j] - (m[i,entra]*m[sale,j])/m[sale,entra]
       }
     }
   }
   
   # se actualiza la fila y la columna pivote. 
   # Columna pivote se divide entre el elemto pivote y se le cambia el signo. 
   ele_pivote <- m[sale,entra]
   m[,entra] <- m[,entra]/ele_pivote * -1
   
   # La fila pivote se divide entre el elemento pivote. 
   m[sale,] <- m[sale,]/ele_pivote
   
   # Se actualiza el elemento pivote
   m[sale,entra] <- 1/ele_pivote
   
   # se actualiza los nombres de la variable que entra y la que sale. 
   sale_tmp <- rownames(m)[sale]
   rownames(m)[sale] <- colnames(m)[entra]
   colnames(m)[entra] <- sale_tmp

   print(m)
  }
}
 #m <- fractions(m)
 # if(all(m[1,2:dimen[2]]<0)){
 #   m[1,] <- -m[1,]
 # }
 return(m)
}

