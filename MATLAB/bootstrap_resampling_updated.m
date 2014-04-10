
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load('variable_a_april.mat')
load('variable_b_april.mat')

x1 = nominal(a)
x2 = nominal(b)

tab = crosstab(x1,x2)

%load('table_data.mat')
load('ShCountData_april2014.mat')

res = []
for i = 1:10
    henry4part1 = crosstab(datasample(x2(x1=='1H4'),length(x2(x1=='1H4'))));
    henry6part1 = crosstab(datasample(x2(x1=='1H6'),length(x2(x1=='1H6'))));
    henry4part2 = crosstab(datasample(x2(x1=='2H4'),length(x2(x1=='2H4'))));
    henry6part2 = crosstab(datasample(x2(x1=='2H6'),length(x2(x1=='2H6'))));
    henry6part3 = crosstab(datasample(x2(x1=='3H6'),length(x2(x1=='3H6'))));
    alls = crosstab(datasample(x2(x1=='AWW'),length(x2(x1=='AWW'))));
    antony = crosstab(datasample(x2(x1=='ANT'),length(x2(x1=='ANT'))));
    asyou = crosstab(datasample(x2(x1=='AYL'),length(x2(x1=='AYL'))));
    errors = crosstab(datasample(x2(x1=='ERR'),length(x2(x1=='ERR'))));
    coriolanus = crosstab(datasample(x2(x1=='COR'),length(x2(x1=='COR'))));
    cymbeline = crosstab(datasample(x2(x1=='CYM'),length(x2(x1=='CYM'))));
    ed3 = crosstab(datasample(x2(x1=='E3'),length(x2(x1=='E3'))));
    hamlet = crosstab(datasample(x2(x1=='HAM'),length(x2(x1=='HAM'))));
    henry5 = crosstab(datasample(x2(x1=='H5'),length(x2(x1=='H5'))));
    henry8 = crosstab(datasample(x2(x1=='H8'),length(x2(x1=='H8'))));
    julius = crosstab(datasample(x2(x1=='JC'),length(x2(x1=='JC'))));
    john = crosstab(datasample(x2(x1=='JN'),length(x2(x1=='JN'))));
    lear = crosstab(datasample(x2(x1=='King Lear'),length(x2(x1=='King Lear'))));
    loves = crosstab(datasample(x2(x1=='LLL'),length(x2(x1=='LLL'))));
    macbeth = crosstab(datasample(x2(x1=='MAC'),length(x2(x1=='MAC'))));
    measure = crosstab(datasample(x2(x1=='MM'),length(x2(x1=='MM'))));
    merchant = crosstab(datasample(x2(x1=='MV'),length(x2(x1=='MV'))));
    merry = crosstab(datasample(x2(x1=='WIV'),length(x2(x1=='WIV'))));
    midsummer = crosstab(datasample(x2(x1=='MND'),length(x2(x1=='MND'))));
    muchado = crosstab(datasample(x2(x1=='ADO'),length(x2(x1=='ADO'))));
    othello = crosstab(datasample(x2(x1=='OTH'),length(x2(x1=='OTH'))));
    pericles = crosstab(datasample(x2(x1=='PER'),length(x2(x1=='PER'))));
    richard2 = crosstab(datasample(x2(x1=='R2'),length(x2(x1=='R2'))));
    richard3 = crosstab(datasample(x2(x1=='R3'),length(x2(x1=='R3'))));
    romeo = crosstab(datasample(x2(x1=='ROM'),length(x2(x1=='ROM'))));
    sirthomas = crosstab(datasample(x2(x1=='STM'),length(x2(x1=='STM'))));
    spanish = crosstab(datasample(x2(x1=='SPT'),length(x2(x1=='SPT'))));
    taming = crosstab(datasample(x2(x1=='SHR'),length(x2(x1=='SHR'))));
    tempest = crosstab(datasample(x2(x1=='TMP'),length(x2(x1=='TMP'))));
    timon = crosstab(datasample(x2(x1=='TIM'),length(x2(x1=='TIM'))));
    titus = crosstab(datasample(x2(x1=='TA'),length(x2(x1=='TA'))));
    trolius = crosstab(datasample(x2(x1=='TRO'),length(x2(x1=='TRO'))));
    twelfth = crosstab(datasample(x2(x1=='TN'),length(x2(x1=='TN'))));
    twogent = crosstab(datasample(x2(x1=='TGV'),length(x2(x1=='TGV'))));
    twonoble = crosstab(datasample(x2(x1=='TNK'),length(x2(x1=='TNK'))));
    winters = crosstab(datasample(x2(x1=='WT'),length(x2(x1=='WT'))));
    permed = transpose(horzcat(henry4part1,henry6part1,henry4part2,henry6part2,henry6part3,alls, antony, asyou, errors, coriolanus, cymbeline, ed3, hamlet, henry5, henry8, julius, john, lear, loves, macbeth, measure, merchant, merry, midsummer, muchado, othello, pericles, richard2, richard3, romeo, sirthomas, spanish, taming, tempest, timon, titus, trolius, twelfth, twogent, twonoble, winters ));
    Data.F = permed;
    res(:,i) = CAalsInequal_SH_YEARS(Data,1,1);
end