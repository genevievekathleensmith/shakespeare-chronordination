
%a = {'a'}
%b = {'b'}


load('variable_a_april.mat')
load('variable_b_april.mat')
% open up 'fulldata.csv', copy first column

% double click on a object, it will open in viewer
% right mouse click on column heading and paste excel data into viewer
% play names (or pause names) SHOULD paste in WITHOUT QUOTATIONS

% delete the first entry by deleting the row (to remove 'Title')

% repeat for b to enter pause names

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
