function H=RemoveSeries(H)


Hsum=sum(H,2);
[n,p]=size(H);
ToRemove=[];
T=(Hsum>1);
[T,Ind]=sort(T);
T=Ind(T);

Doubles=not(isempty(T));

while Doubles,
    
    Row=H(T(1),:);
    [TH,IndCol]=sort(Row);
    Col1=IndCol(p-1);
    Col2=IndCol(p);
    H(:,Col1)=H(:,Col2)+H(:,Col1);
    H(T(1),Col1)=1;
    ToRemove=[ToRemove;Col2];
    
    
    H=removerows(H',ToRemove);
    H=H';
    
    Hsum=sum(H,2);
    [n,p]=size(H);
    T=(Hsum>1);
    [T,Ind]=sort(T);
    T=Ind(T);
    
    Doubles=not(isempty(T));
end
