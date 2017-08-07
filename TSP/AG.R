## Este programa no va a correr porque dañe el archivo de distancias. 
# Este programa debe de estar en el directorio de trabajo. 
rm(list=ls())
library(leaflet)
ciuadades <- read.csv("~/C_Col.csv",header = F)
leaflet(ciuadades)%>%addTiles()%>%addCircles(lng = ~V2, lat = ~V1)
#
source("~/Fun_AG.R")
dis <- read.csv("~/C_Col.csv",header = FALSE,sep = ",")
c_ciu <- dim(dis)
Q_IND <- 20
matriz<-matrix(nrow = c_ciu[1],ncol= c_ciu[1]) # se guardan las distancias
pobini<-matrix(ncol = c_ciu[1],nrow = Q_IND) #

# se calcula la distancia euclidiana
for(j in 1:c_ciu[1]){
  for(i in 1:c_ciu[1]){
    matriz[j,i]<-sqrt(sum((dis[j,]-dis[i,])^2))
  }
}

# se determina la población incial y las funciones objetivos
Fun_Objetivos<-vector(length = Q_IND)
for(i in 1:Q_IND){
  #pobini[i,]<-sample(c_ciu[1])
  pobini[i,]<-greedy(matriz) # se genera la pob con un greedy
  Fun_Objetivos[i]<-fun_obj(pobini[i,],matriz) 
}

solini<-min(Fun_Objetivos)

# Parámetros del algoritmo génetico
maxiter<-1000
tc<-0.9
tm<-0.1
muestra<-4 # cantidad de individuos para el torneo. 

for (k in 1:maxiter){
  Padre1 <- pobini[torneo(Fun_Objetivos,muestra),]
  Padre2 <- pobini[torneo(Fun_Objetivos,muestra),]
  
  #se cruzan y solo se selecciona uno
  selec<-sample(2,1) # aleatorio, toma el valor de 1 o 2.
  if (runif(1)<tc){
    hijo<-pmx(Padre1,Padre2)[selec,]
  }else{
    hijo<-rbind(Padre1,Padre2)[selec,]
  }

  # se raliza la mutación. 
  if (runif(1)<tm){
   hijo<-opt2(hijo)
  }
  
  # Función objetivo del hijo. 
  FOhijo<-fun_obj(hijo,matriz)
  
  # Individuo malo. 
  FOPeor<-max(Fun_Objetivos) # Función objetivo más grande
  PosFOPeor<-which(Fun_Objetivos==max(Fun_Objetivos)) # posición del individuo más malo. 

  if(length(PosFOPeor)>1){ # Cuantos individuos malos hay. Solo se toma uno
     mu<-sample(length(PosFOPeor),1)
     PosFOPeor<-PosFOPeor[mu]
  }
  
  if (FOhijo < FOPeor){
    Fun_Objetivos[PosFOPeor]<-FOhijo
    pobini[PosFOPeor,]<-hijo
  }
}

solfin<-c(solini,min(Fun_Objetivos)) # La primera es la solución incial y la segundo la encontrada por el genético 
solfin
 
### Se va a realizar la visualización el leaflet. 
library(leaflet)

leaflet(dis)%>%addTiles()%>% addPolylines(lng=~V2,lat=~V1,color = "green")%>%
  addMarkers(c(dis[1,2],dis[nrow(dis),2]),c(dis[1,1],dis[nrow(dis),1]),
             popup =c("Inicio","Fin")) %>% addCircleMarkers(lng=~V2,lat=~V1,radius = .1,color = "darkgreen")


#se escoge un individuo de la población final. 
AG_Solu <- dis[pobini[1,],]
leaflet(AG_Solu)%>%addTiles()%>% 
  addPolylines(lng=~V2,lat=~V1,color = "salmon",opacity = 0.4) %>%
    addMarkers(c(AG_Solu[1,2],AG_Solu[nrow(AG_Solu),2]),c(AG_Solu[1,1],AG_Solu[nrow(AG_Solu),1]),
                 popup =c("Inicio","Fin")) %>% addCircleMarkers(lng=~V2,lat=~V1,radius = .1,color ="darksalmon")
                                    
solfin
