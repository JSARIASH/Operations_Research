reset;

# Parámetros
param ciudades;
param coordenadas{1..ciudades, 1..2};
param distancias{1..ciudades, 1..ciudades};
param cont;

var x{1..ciudades, 1..ciudades} binary;

maximize z: sum{i in 1..ciudades, j in 1..ciudades: i != j} distancias[i,j]*x[i,j]; 
subject to llegada{j in 1..ciudades}: sum{i in 1..ciudades: i!= j} x[i, j] = 1;
subject to salidas{i in 1..ciudades}: sum{j in 1..ciudades: i != j} x[i, j] = 1;

# subtours de longitud 2.
subject to long2{i in 1..ciudades, j in 1..ciudades: i < j}: x[i, j] + x[j, i] <= 1; 

# subtours de longitud 3.
subject to long3{i in 1..ciudades, j in 1..ciudades, k in 1..ciudades: i < j && j < k}: x[i, j] + x[j, k] + x[k, i] <= 2; 

# subtours de longiutd 4.
subject to long4{i in 1..ciudades, j in 1..ciudades, k in 1..ciudades, l in 1..ciudades: i < j && j < k && k <l}: 
                       x[i, j] + x[j, k] + x[k, l] + x[l ,i] <= 3; 
/*
subject to long5{i in 1..ciudades, j in 1..ciudades, k in 1..ciudades, l in 1..ciudades, p in 1..ciudades: 
              i < j && j < k && k < l && l < p}:  x[i, j] + x[j, k] + x[k, l] + x[l ,p] + x[p ,i]<= 4; 
*/                

data tsp_no_smart.dat;
display ciudades, coordenadas;
display distancias;

expand long3;

option solver cplex;
solve z;
display x;

let cont:= 1;
for{i in 1..ciudades}
 {
   for {j in 1..ciudades}
      {
        if (x[cont, j] != 0) then {
          print cont, j;
          let cont := j;
          break;          
        }
      }
 }









