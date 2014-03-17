function x=LSI(f,E,G,h)

% Sets up the Inequality constrained 
% Least-Squares regression problem, i.e.,
%
% minimize ||Ex-f|| over x subject to Gx>=h 
%
m=length(f);
[m2,n]=size(E);
[P,Fi,Q]=svd(E);
t=sum(Fi,1)>0;
i=1:n;
k=max(i(t));
i=1:k;
K1=Q(i,:);
Q1=P(:,i);
f1=Q1'*f;
RInv=diag(1./diag(Fi(i,i)));
T=G*K1*RInv;
z=LDP(T,h-T*f1);
y=RInv*(z+f1);
x=K1*y;


