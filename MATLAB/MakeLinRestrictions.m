function [H,Base,KnotSequence]=MakeLinRestrictions(IndYear,Year,m,IndEqual,AllKnots,Order,KnotSequence)

% Uses: Transprep, RemoveSeries

% Makes a matrix H that contains the linear restrictions  
% by Year for the rows indicated by IndYear
% IndEqual gives indexes of the equality constraints per row

HEqual=[];
I=eye(m,m);
if Order>=1  
   % Make Spline basis for Years
   trans='mspline';
   [DataInit,iord,ties,Base,KnotSequence]=TransPrep(Year,trans,AllKnots,Order,KnotSequence);
else
   % Make fixed nonlinear transformation
   % Order=2;
   Base = reshape((Year).^Order,length(Year),1);
end
RankSplineBasis=size(Base,2);


H=zeros(m,RankSplineBasis+1);
% Add to H the intercept and the year constraint
H(IndYear,1:(RankSplineBasis+1))=[Base ones(length(Year),1)];
% Add to H the equality constraints
for i=1:size(IndEqual,1)
   HEqual=[HEqual sum(I(:,IndEqual(i,:)),2)];
end

HEqual=RemoveSeries(HEqual);
H=[H HEqual];

% Find the indexes of the unconstrained columns
Ind=IndYear(:);
Ind=[Ind; IndEqual(:)];
Ind=sort(Ind(:));
nInd=length(Ind);
NextInd=1;
IndRemain=[];
for i=1:m
   if (i==Ind(NextInd))
      NextInd=min(NextInd+1,nInd);
      while NextInd<nInd & Ind(NextInd)==Ind(NextInd-1), 
            NextInd=NextInd+1; 
        end;
   else 
      IndRemain=[IndRemain i];
   end
end
% Add the unconstrained columns to H
H=[H I(:,IndRemain)];
