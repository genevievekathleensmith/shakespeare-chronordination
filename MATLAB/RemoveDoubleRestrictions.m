function [IndEqual,IndYears,Years]=RemoveDoubleRestrictions(IndEqual,IndYears,Years)

% Uses: removerows

IndexToAdd=[];
YearsToAdd=[];
IndDouble=[];
IndEqualVec=IndEqual';
IndEqualVec=IndEqualVec(:);
IndYears=IndYears';
p=size(IndYears,1);





for i=1:size(IndEqualVec,1),
    
       
    for j=1:p,
        
        if IndEqualVec(i)==IndYears(j), 
            IndDouble=[IndDouble;i];
            if mod(i,2)==1,
                if j~=p,
                    if IndEqualVec(i+1)~=IndYears(j+1),
                        IndexToAdd=[IndexToAdd;IndEqualVec(i+1)]; 
                        YearsToAdd=[YearsToAdd; Years(j)];
                    end
                else %j=p
                    IndexToAdd=[IndexToAdd;IndEqualVec(i+1)]; 
                    YearsToAdd=[YearsToAdd; Years(j)];
                   
                end
            else
                if j>1,
                    if IndEqualVec(i-1)~=IndYears(j-1),
                        IndexToAdd=[IndexToAdd;IndEqualVec(i-1)];
                        YearsToAdd=[YearsToAdd; Years(j)];
                    end
                else
                    IndexToAdd=[IndexToAdd;IndEqualVec(i-1)];
                    YearsToAdd=[YearsToAdd; Years(j)];
                end
            end
            
        end
        
    end
    
end

IndEqual=removerows(IndEqual,round(IndDouble/2));
IndYears=sort([IndYears ; IndexToAdd]);
Years=sort([Years; YearsToAdd]);

IndYears=IndYears';


