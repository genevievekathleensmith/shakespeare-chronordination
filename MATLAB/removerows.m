function [Y] = removerows(X,r)

% remove certain rows of X. r is vector with to be removed row numbers.

[n,p]=size(X);
K=[];
r=[r;n+1];
[d]=size(r,1);

Xi=X(1:(r(1)-1),:);

for i=1:(d-1),
    for j=(r(i)+1):(r(i+1)-1),
        Xi=[Xi; X(j,:)];
    end
end

Y=Xi;