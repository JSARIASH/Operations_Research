reset;
param ciudades;
param corde{1..ciudades,1..2};
param dista{1..ciudades,1..ciudades};

var x{1..ciudades,1..ciudades} binary;
var u{1..ciudades} >= 0;

minimize costos: sum{i in 1..ciudades, j in 1..ciudades} dista[i,j]*x[i,j];
subject to r1{i in 1..ciudades}: sum{j in 1..ciudades: j !=i } x[i,j] = 1; # salida
subject to r2{j in 1..ciudades}: sum{i in 1..ciudades: i != j} x[i,j] = 1; # llegada
subject to r3{i in 1..ciudades, j in 2..ciudades: j != i && j > 1}: u[i] - u[j] + ciudades*x[i,j] <= ciudades -1; # sub tours

data tsp.dat;
#display x, corde;
#expand costos,r1,r2,r3;

option solver cplex;
solve costos;
display x;
#display dista;

for {i in 1..ciudades}{
  for{j in 1..ciudades}{
     if (x[i,j] != 0) then {
     print j;
     }
   }
  }
  display _ampl_time, _total_solve_time;