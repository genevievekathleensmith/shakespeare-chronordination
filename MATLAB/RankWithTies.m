function [RankedSeries]=RankWithTies(Serie)

[RcOrdered,RanksRc]=sort(Serie);
index=1;
ranks=[];
RankedSeries=[];

[DatInit,iord,ties,Base,KnotSequence]=TransPrep(Serie,'ordinals',0,0,0);
ties=ties';

for i=1:size(ties),
    
    rank=0;
    
    for j=1:ties(i);
        
        rank=rank+RanksRc(index);
        index=index+1;
        
    end
    
    
    ranks=[ranks; rank/ties(i)];
    
    
end

index=0;

for i=1:size(ties),
    
    for j=1:ties(i),
        index=index+1;
        RankedSeries=[RankedSeries ; ranks(i)];
    end
    
end