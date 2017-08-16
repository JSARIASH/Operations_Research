# Ejemplo tomado del Hillier y Lieberman, Introducción a la Investigación de Operaciones,
# novena edición página 284.
reset;

set enlatadora;
set almacenes;

param costos_envio{enlatadora, almacenes};
param produccion{enlatadora};
param capacidad{almacenes};

# varible de decisión 
var cantidad{enlatadora,almacenes} >=0; # se define la variable como mayor o igula a cero. 

minimize costo: sum{i in enlatadora, j in almacenes} cantidad[i,j]*costos_envio[i,j] -123; # costo es el nombre de la Obj. 

subject to capacidad_enlatadoras{i in enlatadora}: sum{j in almacenes} cantidad[i,j] = produccion[i];
subject to capacidad_almacenes{j in almacenes}: sum{i in enlatadora} cantidad[i,j] = capacidad[j];

data modelo_hillier_datos.dat;
option solver './cplex';
solve costo;
display cantidad, costo;
