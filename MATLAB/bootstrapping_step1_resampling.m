
tab = crosstab(x1,x2)

samp1 = datasample(x2(x1=='Titus Andronicus'),length(x2(x1=='Titus Andronicus')))
samp2 = datasample(x2(x1=='Titus Andronicus'),length(x2(x1=='Titus Andronicus')))

plays = unique(x1)

permed = []
for i = 1:length(unique(x1))
    temp = datasample(y1(x1==plays(i)),sum(x1==plays(i)))
    permed = cat(1,permed, temp)
end



Data.F = permed

CAalsInequal_SH_YEARS(Data,1,1)



res = []
for i = 1:10
     res(:,i) = CAalsInequal_SH(Data,1,1);
end