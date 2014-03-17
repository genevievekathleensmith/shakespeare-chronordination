function [Coord,Xval,Years]=ExpandFixedFunction(z,Weights,R,Order)

% Interpolates coordinates for a spline transformation in Coord
% and gives their values in Xval (using variable z)

npoints=100;
eps=1e-10;
Years=zeros(size(R,1),1);
Coord=zeros(npoints,1);
% Order=2;
% Reconstruct for fixed nonlinear transformation
Years=((R-Weights(2))/Weights(1)).^(1/Order);

% create new points
z=z.^Order;
x=((1:npoints)-1)'/(npoints-1)*(max(z)-min(z))+min(z);
X=(x*[1 1]).^(ones(length(x),1)*[1 0]);
%Coord=R;
Coord=X*Weights;
Xval=x.^(1/Order);

