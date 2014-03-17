function [IndEqual,IndYear,Year]=CheckForOverlap(IndEqual,IndYear,Year);

% Uses: RemoveDoubleRestrictions

% Double restrictions (year and equality) should be removed. For example:
% if Year 6 is known, and year 5 = year 6, both should be incorporated as
% year constraints. 

check=[];

overlap=logical(1);

while overlap,

IndEqualVec=IndEqual(:);
    
if not(isempty(IndEqual)) & not(isempty(IndYear)),
                
        for i=1:size(IndEqualVec,1),
            
            for j=1:size(IndYear,2),
                    
                check=[check;(IndEqualVec(i)==IndYear(j))];
            end
            
        end
        
        overlap=(sum(check)>0);
        
        if overlap,
            
             [IndEqual,IndYear,Year]=RemoveDoubleRestrictions(IndEqual,IndYear,Year);      
             
         end
            
    else  
        overlap=logical(0);
      
    end

check=[];

end
