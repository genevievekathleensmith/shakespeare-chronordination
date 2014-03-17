function [Coord,Xval,Years]=ExpandSpline(z,Base,KnotSequence,b,R,Order)

% Interpolates coordinates for a spline transformation in Coord
% and gives their values in Xval (using variable z)

npoints=100;
eps=1e-10;
Years=zeros(size(R,1),1);
Coord=zeros(npoints,1);
if Order>=1
   %Reconstruct for Splines
   Intervals=size(KnotSequence,1)-1;
   PolynomialWeights=zeros(Order+1,Intervals);
   Poly=(0:Order)';
   for j=1:Intervals
      % Compute polynomial
      t=(z>=KnotSequence(j))&(z<=KnotSequence(j+1));
      i=1:length(z);i=i(t);
      x=reshape(z(i),length(z(i)),1);
      X=(x*ones(1,Order+1)).^(ones(length(x),1)*Poly');
      PolynomialWeights(:,j)=X\(Base(i,:)*b);
      % Reconstruct Years from R
      Knotj=KnotSequence(j);
      RKnotj =PolynomialWeights(:,j)'*KnotSequence(j).^Poly;
      if j==1;RKnotj=min(R);end
      RKnotj1=PolynomialWeights(:,j)'*KnotSequence(j+1).^Poly;
      if j==Intervals;RKnotj1=max(R);end
      t2=(R>=RKnotj-eps)&(R<=RKnotj1+eps);
      i=1:length(R);i=i(t2);
      Years(i)=solveEquation(PolynomialWeights(:,j),R(i));
   end
else
   % Reconstruct for fixed nonlinear transformation
   Poly=(0:1)';
   x=reshape(z,length(z),1);
   X=(x*ones(1,2)).^(ones(length(x),1)*Poly');
   Weights==X\(Base*b);
   Years=((R-Weights(1))/Weights(2)).^2;
end


% create new points
x=((1:npoints)-1)'/(npoints-1)*(max(z)-min(z))+min(z);
X=(x*ones(1,Order+1)).^(ones(length(x),1)*Poly');
if Order>=1
   for j=1:Intervals
      t=(x>=KnotSequence(j))&(x<=KnotSequence(j+1));
      i=1:length(x);i=i(t);
      Coord(i)=X(i,:)*PolynomialWeights(:,j);
   end
else
   Coord(i)=X(i,:)*Weights(:,j);
end

Xval=x;

function x=solveEquation(PolynomialWeights,y)
% Solve the equation ax^2 +bx + c=y
%
b=PolynomialWeights(2);
c=PolynomialWeights(1);
if length(PolynomialWeights)==3 
   % Solve the equation ax^2 +bx + c=y
   a=PolynomialWeights(3);
   x=(-b+(b^2-4*a*(c-y)).^.5)/(2*a);
elseif length(PolynomialWeights)==2 
   % Solve the equation bx + c = y
   x = (y-c)/b;
end

