setwd("~/NGSA_II/")
source("FunNSGA.R")
dev<-dev.cur()
if (dev >1){
  dev.off(dev.list()["RStudioGD"])
}
rm(list=ls())
aaa<-proc.time()

##### Configuración del Algoritmo #####
MaxMin<-c(1,1)
genera<-800 # numbers of generations
tm<-1 # mutation rate
tc<-0.8 # crosover rate
sizepob<-15 # poblation size
source("FunNSGA.R")
##### Matriz de distancias y emisiones #####
corde<-matrix(c(37,52,49,49,52,64,20,26,40,30,21,47,17,63,31,62,52,33,51,21,42,41,31,32,5,25,
     12,42,36,16,52,41,27,23,17,33,13,13,57,58),ncol=2,byrow = T)
numrow<-dim(corde)[1]
dist<-matrix(nrow=20,ncol=20)
for(i in 1:(numrow-1)){
  for(j in (i+1):numrow){
    dist[i,j]<-{{corde[j,]-corde[i,]}%*%{corde[j,]-corde[i,]}}^{1/2}
    dist[j,i]<-dist[i,j]
  }
}

dist[is.na(dist)==TRUE]<-0
poluci<-matrix(nrow=20,ncol=20) # población.
poluci<-apply(poluci,1,runif)*100 # matriz con polución. 
pob<-matrix(nrow=sizepob,ncol=numrow) # 

###### Se determina la población inicial y sus funciones objetivo ######

pcity<-1:numrow
funObje<-matrix(nrow = sizepob,ncol = 2)
for (i in 1:sizepob){
   pcity<-sample(pcity) # orden en que se van a visitar las ciudades
   pob[i,]<-pcity
   funObje[i,1]<-disre(pcity,dist)
   funObje[i,2]<-disre(pcity,poluci)
  }
#funObje<-cbind(funObje,1:nrow(funObje)) # Función objetivo del indiviudo
funObje<-funObje[order(funObje[,1],funObje[,2]),]

##### Mediante Operadores Géneticos se genera una población de hijos #####
pobnw<-pob
nwfunobj<-funObje
for(k in 1:genera){
  pobhij<-matrix(nrow=sizepob,ncol=numrow)
  funObjehij<-matrix(nrow = sizepob,ncol = 2)
  for(h in 1:sizepob){
    
    if(tc >= runif(1,0,1)){
      ale<-sample(1:sizepob,2)
      pad1<-ale[1]
      pad2<-ale[2]
      pad1<-pobnw[pad1,]
      pad2<-pobnw[pad2,]
      pobhij[h,]<-pmx(pad1,pad2)
    }else{
      pad1<-sample(1:sizepob,1)
      pobhij[h,]<-pobnw[pad1,]
    }
    if(tm >= runif(1,0,1)){
      pobhij[h,]<-opt(pobhij[h,])
    }
    funObjehij[h,1]<-disre(pobhij[h,],dist)
    funObjehij[h,2]<-disre(pobhij[h,],poluci)
  }
  nwfunobj<-rbind(nwfunobj,funObjehij)
  nwfunobj<-cbind(nwfunobj,1:(2*sizepob))
  nwfunobj<-nwfunobj[order(nwfunobj[,1],nwfunobj[,2]),]
  frentes<-ParetoFront(nwfunobj,MaxMin)
  rmna<-which(is.na(frentes[,1]))
  frentes<-frentes[-rmna,]
  orfre<-apply(frentes,1,dife<-function(a){a[!is.na(a)]})
  posnwpob<-nwpob(orfre,nwfunobj,sizepob)
  pobnw<-rbind(pobnw,pobhij)
  pobnw<-cbind(pobnw,1:(2*sizepob))
  pobnw<-pobnw[pobnw[,(numrow+1)]%in%posnwpob,]
  pobnw<-pobnw[,-(numrow+1)]
  nwfunobj<-nwfunobj[nwfunobj[,3]%in%posnwpob,]
  nwfunobj<-nwfunobj[,-3]
  }
## solución inicial
funObje<-cbind(funObje,1:nrow(funObje))
frentes<-ParetoFront(funObje,MaxMin)
rmna<-which(is.na(frentes[,1]))
frentes<-frentes[-rmna,]
orfre<-apply(frentes,1,dife<-function(a){a[!is.na(a)]})

# solución luego de aplicar NSGA II
nwfunobj<-cbind(nwfunobj,1:nrow(nwfunobj))
nwfunobj<-nwfunobj[order(nwfunobj[,1],nwfunobj[,2]),]
frentesnw<-ParetoFront(nwfunobj,MaxMin)
rmna<-which(is.na(frentesnw[,1]))
frentesnw<-frentesnw[-rmna,]
if (is.matrix(frentesnw)==TRUE){
  orfrenw<-apply(frentesnw,1,dife<-function(a){a[!is.na(a)]})
}else{
  orfrenw<-list(frentesnw)
}
nwfunobj[,3]<-nwfunobj[,3]+sizepob
funObjetot<-rbind(funObje,nwfunobj)

for(i in 1:(length(orfrenw)+length(orfre))){
  if(i <= length(orfre)){
      gra<-funObjetot[funObjetot[,3] %in% orfre[[i]],]
        if(is.matrix(gra)==TRUE){
          plot(gra[,1],gra[,2],type="b",col="blue",xlim = c(min(funObjetot[,1]),max(funObjetot[,1])),ylim =c(min(funObjetot[,2]),max(funObjetot[,2])),xlab = "F1",ylab = "F2",main = "NSGAII") 
        }else{
          plot(gra[1],gra[2],type="b",col="blue",xlim = c(min(funObjetot[,1]),max(funObjetot[,1])),ylim =c(min(funObjetot[,2]),max(funObjetot[,2])),xlab = "F1",ylab = "F2",main = "NSGAII") 
        }
      par(new=TRUE)
  }else{
    gra<-funObjetot[funObjetot[,3] %in% (orfrenw[[(i-length(orfre))]]+15),]
      if(is.matrix(gra)==TRUE){
        plot(gra[,1],gra[,2],type="b",col="red",xlim = c(min(funObjetot[,1]),max(funObjetot[,1])),ylim =c(min(funObjetot[,2]),max(funObjetot[,2])),xlab = "F1",ylab = "F2",main = "NSGAII") 
      }else{
        plot(gra[1],gra[2],type="b",col="red",xlim = c(min(funObjetot[,1]),max(funObjetot[,1])),ylim =c(min(funObjetot[,2]),max(funObjetot[,2])),xlab = "F1",ylab = "F2",main = "NSGAII") 
      }
    par(new=TRUE)
  }
}
proc.time()-aaa

