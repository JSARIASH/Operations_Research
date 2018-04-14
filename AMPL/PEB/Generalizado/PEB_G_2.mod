reset;
set proyecto;
set alternativa;
set años;

param VPN{proyecto,alternativa};
param FW{proyecto,alternativa};
param disponible_años{años,proyecto,alternativa};
param C_Disponible{años};
var X{proyecto,alternativa} binary;


maximize utili: sum{i in proyecto, j in alternativa} X[i,j]*VPN[i,j];
subject to nada3: X['pro3','alt3'] = 0; # variable tresn o existe debe ser cero
subject to solouna{i in proyecto}: sum{j in alternativa} X[i,j] = 1; # se escoje una alternativa de cada proyecto. 
subject to inversion_anual{k in años}:sum{i in proyecto,j in alternativa} X[i,j]*disponible_años[k,i,j] <= C_Disponible[k];
subject to fw:sum{i in proyecto, j in alternativa}  X[i,j]*FW[i,j] <= 10000;
  
data PEB_G_2.dat;
option solver cplex;
solve utili;
display utili, X;
