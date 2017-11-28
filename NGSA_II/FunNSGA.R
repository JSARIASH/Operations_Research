#### Distancias recorridas y emisiones ####
disre<-function(pcity,dis){
   n<-length(pcity)
      disre <- 0
    for (i in 1:(n-1)){
      disre <- disre + dis[pcity[i],pcity[i+1]]
    }
   disre<-disre+dis[pcity[n],pcity[1]]
   return(disre)
}

#### Frente de Pareto ####
ParetoFront<-function(funObje,MaxMin){
  numrow<-nrow(funObje)
  repet<-vector(length = numrow)
  cont<-0
  for(i in 1:(numrow-1)){
    if(all(funObje[i,1:2]==funObje[(i+1),1:2])==TRUE){
      cont<-cont+1
      repet[cont]<-i
    }
  }
  # se elieminan las funciones objetivos iguales. 
  if(any(repet!=0)==TRUE){
    repet<-repet[repet > 0]
    funObje<-funObje[-repet,]
  }
  if(all(MaxMin==c(1,1)) || all(MaxMin==c(1,0))){
    funObje<-funObje[order(funObje[,1],funObje[,2],decreasing=TRUE),]
  }
  front<-matrix(nrow = nrow(funObje),ncol=nrow(funObje))
  contcol<-0
  contfil<-0
  breakd<-0
  maxfil<-nrow(funObje)
  while(breakd < (maxfil-1)){
    contfil<-contfil+1
    contcol<-0
    for (i in 1:nrow(funObje)){
      unic<-0
      for (j in 1:nrow(funObje)){
        if (all(MaxMin==c(0,1))==TRUE && i!=j){
          if(funObje[j,1] <= funObje[i,1] && funObje[j,2] >= funObje[i,2]){
            unic<-1
            break
          }    
        }else if(all(MaxMin==c(1,0))==TRUE && i!=j){
          if(funObje[j,1] >= funObje[i,1] && funObje[j,2] <= funObje[i,2]){
            unic<-1
            break
          }
        }else if(all(MaxMin==c(1,1))==TRUE && i!=j){
          if(funObje[j,1] >= funObje[i,1] && funObje[j,2] >= funObje[i,2]){
            unic<-1
            break
          } 
        }else{
          if(funObje[j,1] <= funObje[i,1] && funObje[j,2] <= funObje[i,2] && i!=j){
            unic<-1
          break
          }
        }
      }# j
      if(unic==0){
        contcol<-contcol+1
        front[contfil,contcol]<-funObje[i,3]
      }
    } # i
    breakd<-sum(!is.na(front))
    a<-front[!is.na(front)]
    p<-which(funObje[,3] %in% a)
    funObje<-funObje[-p,]
  } #while
  if(breakd != maxfil){
    front[(contfil+1),1]<-funObje[3]
  }
  return(front)
  }

#### apilamiento ####
apilamiento<-function(mat){
  numfil<-nrow(mat)
  apilamiento<-vector(length = (numfil-2))
  f1max<-max(mat[,1])
  f1min<-min(mat[,1])
  f2max<-max(mat[,2])
  f2min<-min(mat[,2])
  for (i in 1:(numfil-2)){
    f1<-abs({mat[i,1]-mat[(i+2),1]}/{f1max-f1min})
    f2<-abs({mat[i,2]-mat[(i+2),2]}/{f2max-f2min})
    apilamiento[i]<-f1+f2
  }
  return(apilamiento)
}

#### Nueva Población ####
nwpob<-function(orfre,funObje,sizepob){
  summ<-0
  ind<-0
  nwpobc<-NULL
  while(summ < sizepob){
    ind<-ind+1
    summ<-summ+length(orfre[[ind]])
  
      if(summ==sizepob){
        for(i in 1:ind){
          nwpobc<-c(nwpobc,orfre[[i]])
        }
      }else if (summ > sizepob){ 
        if(ind==1){
            #frente mayor a la pob se toman los extremos y mayor apilamiento
            punfalt<-sizepob-2
        if(punfalt==0){
          lorf<-length(orfre[[ind]])
          nwpobc<-c(nwpobc,orfre[[ind]][1],orfre[[ind]][lorf])
        }else if (punfalt==-1){
             ale<-runif(1,0,1)
             if(ale<0.5){
               nwpobc<-c(nwpobc,orfre[[ind]][1])
             }else{
                 l<-length(orfre[[ind]])
                 nwpobc<-c(nwpobc,orfre[[ind]][l])
               }
          }else{
            mat<-funObje[funObje[,3] %in% orfre[[ind]],]
            apil<-apilamiento(mat)
            lorf<-length(orfre[[ind]])
            nwpobc<-c(nwpobc,orfre[[ind]][1],orfre[[ind]][lorf]) # se ponen los extremos del frente
          for(i in 1:punfalt){
            maxi<-which.max(apil)
            nwpobc<-c(nwpobc,orfre[[ind]][maxi+1])
            apil[maxi]<-0
          }
          }
        }else{
          selpunt<-length(orfre[[ind]])-(summ-sizepob) # Cantidad de Puntos a selecionas de pareto ind
           if(selpunt==1){
             ale<-runif(1)
               for(i in 1:(ind-1)){nwpobc<-c(nwpobc,orfre[[i]])}
                 if(ale < 0.5){
                   nwpobc<-c(nwpobc,orfre[[ind]][1])
                 }else{
                   lorf<-length(orfre[[ind]])
                   nwpobc<-c(nwpobc,orfre[[ind]][lorf])
                   }
           }else if(selpunt==2){
               for(i in 1:(ind-1)){nwpobc<-c(nwpobc,orfre[[i]])}
               lorf<-length(orfre[[ind]])
               nwpobc<-c(nwpobc,orfre[[ind]][1],orfre[[ind]][lorf])
           }else {
               for(i in 1:(ind-1)){nwpobc<-c(nwpobc,orfre[[i]])}
                 lorf<-length(orfre[[ind]])
                 nwpobc<-c(nwpobc,orfre[[ind]][1],orfre[[ind]][lorf])
                 lorf<-length(nwpobc)
                 mat<-funObje[funObje[,3] %in% orfre[[ind]],]
                 apil<-apilamiento(mat)
                 punfalt<-sizepob-lorf
               for(i in 1:punfalt){
                 maxi<-which.max(apil)
                 nwpobc<-c(nwpobc,orfre[[ind]][maxi+1])
                 apil[maxi]<-0
               }
           }
        }
      }
  }
  return(nwpobc)
}

###Cruzamiento####
pmx<-function(pad,mad){
  # padre, madre, numero de ciudades. 
  l<-length(pad)
  ale1<-floor(runif(1,0,l))+1
  ale2<-floor(runif(1,0,l))+1
  while(ale1==ale2){ale1<-floor(runif(1,0,l))+1}
  if(ale1>ale2){
    tm<-ale1
    ale1<-ale2
    ale2<-tm
  }
  hij<-pad
  hij[c(ale1:ale2)]<-0
  cont<-ale1
  for(i in 1:l){
    rep<-0
    for(j in 1:l){
      if(mad[i]==hij[j]){
        rep<-1
        break
      }
    }
    if(rep==0){
      hij[cont]<-mad[i]
      cont<-cont+1
    }
  }
  return(hij)
}

#### Mutación ####
opt<-function(ind){
  himut<-ind
  l<-length(ind)
  ale1<-floor(runif(1,0,l))+1
  ale2<-floor(runif(1,0,l))+1
  while(ale1==ale2){ale1<-floor(runif(1,0,l))+1}
  if(ale1>ale2){
    tm<-ale1
    ale1<-ale2
    ale2<-tm
  }
  for(i in ale1:ale2){
    himut[i]<-ind[ale2]
    ale2<-ale2-1
  }
  return(himut)
}















