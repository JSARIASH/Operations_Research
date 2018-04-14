reset;

var x11 binary;
var x12 binary;
var x13 binary;
var x21 binary;
var x22 binary;
var x23 binary;
var x31 binary;
var x32 binary;

maximize utilidad: 757*x11 + 825*x12 + 987*x13 + 350*x21 + 596*x22 + 650*x23 + 1420*x31 + 1425*x32;

subject to una_alternativa_p1: x11 + x12 + x13 = 1;
subject to una_alternativa_p2: x21 + x22 + x23 = 1;
subject to una_alternativa_p3: x31 + x32  = 1;

subject to empleos:  700*x11 + 3500*x12 + 2000*x13 + 1200*x21 + 100*x22 + 6000*x23 + 2000*x31 + 500*x32 <= 10000;

subject to FluCap_a1:  50*x11 + 150*x12 + 300*x13 + 100*x21 +  0*x22 + 150*x23 + 500*x31 + 70*x32 <= 700;
subject to FluCap_a2:  50*x11 + 120*x12 +  20*x13 + 100*x21 + 40*x22 +  20*x23 + 100*x31 + 70*x32 <= 1300;
subject to FluCap_a3:  50*x11 +  40*x12 +   0*x13 + 100*x21 + 40*x22 +  20*x23 + 100*x31 + 70*x32 <= 150;
subject to FluCap_a4:  50*x11 +  40*x12 +   0*x13 +  60*x21 + 40*x22 +  20*x23 +   0*x31 + 70*x32 <= 150;
subject to FluCap_a5:  50*x11 +  40*x12 +  80*x13 +  30*x21 + 40*x22 +  20*x23 +   0*x31 + 70*x32 <= 150;

option solver cplex;
solve utilidad;
display utilidad, x11, x12, x13, x21, x22, x23, x31, x32;