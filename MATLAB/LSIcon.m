function x=LSIcon(f,E,G,h,C,d)

% Uses: LSI

% Sets up the Inequality and equality constrained 
% Least-Squares regression problem, i.e.,
%
% minimize ||Ex-f|| over x subject to Gx>=h and Cx=d
%

[m1,n]=size(C);
[P,Fi,Q]=svd(C);
K=Q;
t=sum(Fi,1)>0;
i=1:n;
k=max(i(t));
nullIndex=(k+1):n;
Index=1:k;
E1=E*K;
E2=E1(:,nullIndex);
E1=E1(:,Index);
G1=G*K;
G2=G1(:,nullIndex);
G1=G1(:,Index);
C1=C*K;
C1=C1(:,Index);
y1=C1\d;
y2=LSI(f-E1*y1,E2,G2,h-G1*y1);
x=K*[y1;y2];
