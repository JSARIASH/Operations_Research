parte_entera <- function(m){
dimen <- dim(m)
for (i in 1:dimen[1]){
  for(j in 1:dimen[2]){
    if(m[i,j] < 0){
      m[i,j] <- abs(floor(m[i,j])) + m[i,j]
      m[i,j] <- fractions(m[i,j])
      }else if(m[i,j] > 0){
        m[i,j] <- m[i,j] - floor(m[i,j])
        m[i,j] <- fractions(m[i,j])
      }else{
        m[i,j] <- 0
      }
  }
}
  return(m)
}
