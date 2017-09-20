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
  
     #print(m)
    }
  }
   #m <- fractions(m)
   # if(all(m[1,2:dimen[2]]<0)){
   #   m[1,] <- -m[1,]
   # }
   return(m)
}

