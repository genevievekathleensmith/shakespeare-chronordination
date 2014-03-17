function [x,error]=LDP(G,h)
% Computes a solution to the Least Distance Programming problem
%
% minimize ||x|| over x subject to Gx>=h 
%
cv=1e-12;


[m,n]=size(G);
E=[G';h'];
f=[zeros(n,1);1];
%u = nnls(E,f);
u = lsqnonneg(E,f);
r=E*u-f;
if sum(r.^2)> cv
   error = 0;
   x = -r(1:n)/r(n+1);
else
   error = 1;
  
end