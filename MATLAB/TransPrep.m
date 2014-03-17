function [DatInit,iord,ties,Base,KnotSequence]=TransPrep(Dat,trans,AllKnots,Order,KnotSequence)

% Uses: 

% Prepares for transformation 
% and gives an initial form of the data
%    trans=none     no transformation 
%          linear   linear transformation
%          nominal  nominal transformation
%          ordinalp ordinal primary approach to ties (untie ties)
%          ordinals secondary approach to ties (keep ties tied)
%          spline   I-spline transformation
%          mspline  monotone I-spline transformation
%
n=length(Dat);iord=[];ties=[];Base=[];
if nargin~=5;
   KnotSequence=[];
end
[y,iord]=sort(Dat);
% find tieblocks
ties = 1; tieblock=1;
for i=2:n
   if y(i)==y(i-1) 
      ties(tieblock) = ties(tieblock)+1; 
   else
      tieblock = tieblock + 1;
      ties = [ties 1];
   end
end
switch (trans)
case {'none','linear'}
   DatInit = Dat;
case {'ordinalp','ordinals','ordinal','nominal','nominals','nominalp'}
   i=1:length(ties);DatInit=Dat;
   DatInit(iord)=expand(i,ties)';
case {'spline','mspline'}    
   [Base,KnotSequence]=SplineSetUp(Dat,AllKnots,Order,KnotSequence);
   DatInit = sum(Base,2);
end

function a=expand(x,e)
% performs the apl expand function
n=length(e);
a=ones(1,e(1))*x(1);
for i=2:n
  a=[a ones(1,e(i))*x(i)];
end

function [Base,KnotSequence]=SplineSetUp(Dat,AllKnots,Order,KnotSequence)
% SplineSetUp.m
% 
% Sets up a monotone spline base.
%   AllKnots: the number of interior and exterior knots
%   Order:    order of the spline
%   
%
n=size(Dat,1);
if size(KnotSequence,1)==0
   KnotSequence = CreateKnots(Dat,AllKnots,Order);
else
   KnotSequence = reshape(KnotSequence,1,length(KnotSequence));   
   KnotSequence = [KnotSequence(ones(1:(Order-1))) KnotSequence KnotSequence(ones(1:(Order-1))*length(KnotSequence))];
   KnotSequence = reshape(KnotSequence,length(KnotSequence),1);   
end
Base = JSpline(Dat,KnotSequence,Order,AllKnots);

% Remove knot duplications at extrema
KnotSequence=KnotSequence((Order-1)+(1:AllKnots));

function KnotSequence = CreateKnots(Dat,AllKnots,Order)
% CreateKnots.m
% 
% Creates the knot sequence.
%   AllKnots: the number of interior and exterior knots
%   Order:    order of the spline
%   
%
n=max(size(Dat));
Temp=sort(Dat);
m0=Order;
m1=m0+1;
m2=Order+AllKnots-2;
m3=m2+1;
m4=m3+Order-1;
t=zeros(m4,1);
t(1:m0)=Temp(1);
t((m3-1)+(1:(m4-m3+1)))=Temp(n);
xorder=m3-m0;
prop=((m1-m0-1)+(1:(m2-m1+1))')/xorder;
xpos=prop*(n+1);
npos=floor(xpos);
xval=Temp(npos)+(xpos-npos).*(Temp(npos+1)-Temp(npos));
t((m1-1)+(1:(m2-m1+1)))=xval;

KnotSequence = t;

function spli=ImSpline(KnotSequence,datum,Order,left) 
% ImSpline.m
% 
isw = 0;ncoef=size(KnotSequence,1)-Order;
wk=ones(1,ncoef);dr=wk;dl=wk;spli=wk;
j=1;
while isw==0
   isw=Order==1;
   if (j>=Order) isw=1; end
   jpl = j+1;
   dr(j) = KnotSequence(left+j)-datum;
   dl(j) = datum - KnotSequence(left-j+1);
   s=0;
   for i=1:j
      z     = wk(i)/(dr(i)+dl(jpl-i));
      wk(i) = s+dr(i)*z;
      s     = dl(jpl-i)*z;
   end
   wk(jpl)=s;
   if (isw==1) break; end
   j=jpl;
end
s = 0;
for i=1:Order
   n = Order-i+1;
   s = s + wk(n+1);
   spli(n) = s;
end

function Base = JSpline(Dat,KnotSequence,Order,AllKnots)
% JSpline.m
%
% Compute I-spline basis (Base) for variable Dat
% 
n=max(size(Dat));ncoef=Order+AllKnots-2;
Base=zeros(n,Order+AllKnots-2);
for i=1:n
   datum=Dat(i);
   left=Order;
   if left<=ncoef-1
      while(left<=ncoef-1)
         if KnotSequence(left+1)>datum break; end
         left=left+1;
      end
   end
   %sum((Order:(ncoef-1))<datum);
   spli=ImSpline(KnotSequence,datum,Order,left);  
   ind=1:left-Order;      
   Base(i,ind)= Base(i,ind) + 1;
   ind=left-Order+1:left; 
   Base(i,ind)= Base(i,ind) + spli(1:size(ind,2));
end

   