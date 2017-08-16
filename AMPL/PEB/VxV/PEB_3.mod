reset; 

var x1 binary;
var x2 binary;
var x3 binary;
var x4 binary;
var x5 binary;
var x6 binary;

minimize estaciones: x1 + x2 + x3 + x4 + x5 + x6;

subject to c1: x1 + x3 + x5 >= 1;
subject to c2: x2 + x5 >= 1;
subject to c3: x3 + x1 + x4 +x6 >= 1;
subject to c4: x4 + x3  >= 1;
subject to c5: x5 + x2 + x1 >= 1;
subject to c6: x6 + x3  >= 1;

option solver cplex;
solve estaciones; 
display x1, x2, x3, x4, x5, x6;