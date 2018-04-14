reset;
var x{1..9,1..9,1..9} binary;
#set datos within{1..9 cross 1..9 cross 1..9};
/*param set:=
   (1,1,8) (1,4,6) (1,7,9) (1,9,5) (3,5,2) 
   (3,7,3) (3,8,1) (4,3,7) (4,4,3) (4,5,1)
   (4,6,8) (4,8,6) (5,1,2) (5,2,4) (5,8,7)
   (5,9,3) (7,3,2) (7,4,7) (7,5,9) (7,7,1) 
   (8,1,5) (8,5,8) (8,8,3) (8,9,6) (9,3,3);
display datos;
*/
#display x;
#maximize z: sum{i in 1..9, j in 1..9, k in 1..9} x[i,j,k];
maximize z:x[1,1,1];
subject to r1{i in 1..9, j in 1..9}: sum{k in 1..9} x[k,i,j] = 1;
subject to r2{i in 1..9, k in 1..9}: sum{j in 1..9} x[k,i,j] = 1;
subject to r3{j in 1..9, k in 1..9}: sum{i in 1..9} x[k,i,j] = 1;
subject to r4{i in 1..3,j in 1..3,k in 1..9}: sum{a in 1..3, b in 1..3} x[k,a+(3*(i-1)),b+(3*(j-1))] = 1;

# Valores que ofrece el problema

fix x[8,1,1] := 1;
fix x[6,1,4] := 1;
fix x[9,1,7] := 1;
fix x[5,1,9] := 1;

fix x[2,3,5] := 1;
fix x[3,3,7] := 1;
fix x[1,3,8] := 1;

fix x[7,4,3] := 1;
fix x[3,4,4] := 1;
fix x[1,4,5] := 1;
fix x[8,4,6] := 1;
fix x[6,4,8] := 1;

fix x[2,5,1] := 1;
fix x[4,5,2] := 1;
fix x[7,5,8] := 1;
fix x[3,5,9] := 1;

fix x[2,7,3] := 1;
fix x[7,7,4] := 1;
fix x[9,7,5] := 1;
fix x[1,7,7] := 1;

fix x[5,8,1] := 1;
fix x[8,8,5] := 1;
fix x[3,8,8] := 1;
fix x[6,8,9] := 1;

fix x[3,9,3] := 1;

display x;
#expand r4;




option solver cplex;
solve z;
   
param respuesta{1..9,1..9};
for {k1 in 1..9}{
  for {k2 in 1..9}{
     for {k3 in 1..9}{
          if (x[k1,k2,k3] == 1) then {
              let respuesta[k2,k3] := k1; 
          }    
       }
     }  
   }  
   
   display respuesta;
   
   # Guardar la respuesta como .csv. 
 option show_stats 1;
 option csvdisplay_header 0;
 
 #option log_file respuesta.txt;
 
 #estas líneas funcionan para exportar el .csv
 /*
   for {i in 1..9}
      {
       for {j in 1..9}
          {
             printf  "%s\t%s\t%s\t%.3f\t%.3f\t%.3f\n",i,j,
              respuesta[i,j] > ./output.csv;          
          }
      }
   
   
   
   
   