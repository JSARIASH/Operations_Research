# La almohadilla (#) permite generar comentarios. 

/*
Lo primero que se debe hacer es eliminar variables que se encuentren en memoria
ya que pueden llevar a errores. Esto lo hacemos con el comando reset. 
*/

reset; 

# Luego se deben declarar las variables. Para nuestro caso son binarias.  
var x1 binary;
var x2 binary;
var x3 binary;
var x4 binary;
var x5 binary;
var x6 binary;

# Ya que se tienen las varialbles se procede a escribir la función objetivo. 

maximize satisfaccion: 2000*x1 + 300*x2 + 100*x3 + 400*x4 + 1000*x5 + 2000*x6;

# Ahora debemos escribir las restricciones. 

subject to capacidad: 500*x1 + 200*x2 + 100*x3 + 300*x4 + 300*x5 + 400*x6 <= 800;
subject to max_un_tv: x1 + x5 <= 1;
subject to bafles_radio: x2 + x4 >= 1;
subject to radio_luces: x2 - x3 = 0;
subject to mesa_si_bafles: x6 <= x4; 

# Ya que se tiene el modelo le vamos a decir a ampl que lo resuelva.

option solver cplex; # cplex es el sover que se va a utilizar. 
solve satisfaccion; # se resuelve la función objetivo. 

# Por último le pedimos que muestre los resultados. 

display satisfaccion, x1, x2, x3, x4, x5, x6;



/*
Notas:

1. Las variables fueron definidas con la palabra "var". Evitar el uso de esta palabra para nombrar variables. 
2. La función objetivo se declara con la palabra "maximize" o "minimize" de acuerdo a si se va a maximiar o minimizar. Seguido por el nombre ":" y la expresión. 
3. Las restricciones se declaran con las palabras "subject to" seguidas por el nombre de la restricción y ":" para luego escribir la relación.    
4. Se debe indicar la opción a utilizar para resolver el problema. "option solver cplex" 
5. Se ordena que resuelva la función objetivo. 
6. con el comando "display" se muestran los valores obtenidos. 
7. Observe que cada línea termina con ";"
*/




