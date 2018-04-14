reset;
param n = 8;
var x{1..n,1..n} binary;
maximize z: sum{i in 1..n, j in 1..n} x[i,j];
subject to r1{i in 1..n}: sum{j in 1..n} x[i,j] =1;
subject to r2{j in 1..n}: sum{i in 1..n} x[i,j] =1;
subject to r3{j in 1..n}: x[1,j] + sum{i in 1..(n-j)} x[1+i,i+j] <= 1;
subject to r4{i in 1..n}: x[i,1] + sum{k in 1..(n-i)} x[i+k,1+k] <= 1;
subject to r5{j in 1..n}: x[1,j] + sum{k in 1..(j -1)} x[k+1,j-k] <=1;
subject to r6{i in 1..n}: x[i,n] + sum{k in 1..(n-i)} x[i+k,n-k] <= 1;

option solver cplex;
solve z;
display x;
