
%a = {'a'}
%b = {'b'}


load('variable_a.mat')
load('variable_b.mat')

x1 = nominal(a)
x2 = nominal(b)

tab = crosstab(x1,x2)
[tab,CHI2,P,LABELS] = crosstab(x1,x2)

playtitles = LABELS(:,1)
pausepositions = LABELS(:,2)

load('ShDataEmpty.mat')
Data.Labels = playtitles
Data.F = tab

rawpredictions = CAalsInequal_SH_YEARS(Data,1,1)
csvwrite('CCA_rawpredictions.csv',rawpredictions,0,0)


newcounts = []
res = []
for i = 1:1000
    for j = 1:length(playtitles)
        newcounts(j,:) = crosstab(datasample(x2(x1==playtitles(j)),length(x2(x1==playtitles(j)))));
    end

    Data.F = newcounts
    res(:,i) = CAalsInequal_SH_YEARS(Data,1,1);
end

csvwrite('CCA_bootpredictions.csv',res,0,0)
