
a = {'a'}
b = {'b'}

% open up 'fulldata.csv', copy first column

% double click on a object, it will open in viewer
% right mouse click on column heading and paste excel data into viewer
% play names (or pause names) SHOULD paste in WITHOUT QUOTATIONS

% delete the first entry by deleting the row (to remove 'Title')

% repeat for b to enter pause names

x1 = nominal(a)
x2 = nominal(b)

tab = crosstab(x1,x2)

load('table_data_march2014.mat')

res = []
for i = 1:1000
    henry4part1 = crosstab(datasample(x2(x1=='1 Henry 4'),length(x2(x1=='1 Henry 4'))));
    henry6part1 = crosstab(datasample(x2(x1=='1 Henry 6'),length(x2(x1=='1 Henry 6'))));
    henry4part2 = crosstab(datasample(x2(x1=='2 Henry 4'),length(x2(x1=='2 Henry 4'))));
    henry6part2 = crosstab(datasample(x2(x1=='2 Henry 6'),length(x2(x1=='2 Henry 6'))));
    henry6part3 = crosstab(datasample(x2(x1=='3 Henry 6'),length(x2(x1=='3 Henry 6'))));
    alls = crosstab(datasample(x2(x1=='Alls Well'),length(x2(x1=='Alls Well'))));
    antony = crosstab(datasample(x2(x1=='Antony and Cleopatra'),length(x2(x1=='Antony and Cleopatra'))));
    asyou = crosstab(datasample(x2(x1=='As You Like It'),length(x2(x1=='As You Like It'))));
    errors = crosstab(datasample(x2(x1=='Comedy of Errors'),length(x2(x1=='Comedy of Errors'))));
    coriolanus = crosstab(datasample(x2(x1=='Coriolanus'),length(x2(x1=='Coriolanus'))));
    cymbeline = crosstab(datasample(x2(x1=='Cymbeline'),length(x2(x1=='Cymbeline'))));
    ed3 = crosstab(datasample(x2(x1=='Edward 3'),length(x2(x1=='Edward 3'))));
    hamlet = crosstab(datasample(x2(x1=='Hamlet'),length(x2(x1=='Hamlet'))));
    henry5 = crosstab(datasample(x2(x1=='Henry 5'),length(x2(x1=='Henry 5'))));
    henry8 = crosstab(datasample(x2(x1=='Henry 8'),length(x2(x1=='Henry 8'))));
    julius = crosstab(datasample(x2(x1=='Julius Caesar'),length(x2(x1=='Julius Caesar'))));
    john = crosstab(datasample(x2(x1=='King John'),length(x2(x1=='King John'))));
    lear = crosstab(datasample(x2(x1=='King Lear'),length(x2(x1=='King Lear'))));
    loves = crosstab(datasample(x2(x1=='Loves Labours Lost'),length(x2(x1=='Loves Labours Lost'))));
    macbeth = crosstab(datasample(x2(x1=='Macbeth'),length(x2(x1=='Macbeth'))));
    measure = crosstab(datasample(x2(x1=='Measure for Measure'),length(x2(x1=='Measure for Measure'))));
    merchant = crosstab(datasample(x2(x1=='Merchant of Venice'),length(x2(x1=='Merchant of Venice'))));
    merry = crosstab(datasample(x2(x1=='Merry Wives'),length(x2(x1=='Merry Wives'))));
    midsummer = crosstab(datasample(x2(x1=='Midsummer Nights Dream'),length(x2(x1=='Midsummer Nights Dream'))));
    muchado = crosstab(datasample(x2(x1=='Much Ado'),length(x2(x1=='Much Ado'))));
    othello = crosstab(datasample(x2(x1=='Othello'),length(x2(x1=='Othello'))));
    pericles = crosstab(datasample(x2(x1=='Pericles'),length(x2(x1=='Pericles'))));
    richard2 = crosstab(datasample(x2(x1=='Richard 2'),length(x2(x1=='Richard 2'))));
    richard3 = crosstab(datasample(x2(x1=='Richard 3'),length(x2(x1=='Richard 3'))));
    romeo = crosstab(datasample(x2(x1=='Romeo and Juliet'),length(x2(x1=='Romeo and Juliet'))));
    sirthomas = crosstab(datasample(x2(x1=='Sir Thomas More Iid'),length(x2(x1=='Sir Thomas More Iid'))));
    spanish = crosstab(datasample(x2(x1=='Spanish Tragedy Add.'),length(x2(x1=='Spanish Tragedy Add.'))));
    taming = crosstab(datasample(x2(x1=='Taming of the Shrew'),length(x2(x1=='Taming of the Shrew'))));
    tempest = crosstab(datasample(x2(x1=='Tempest'),length(x2(x1=='Tempest'))));
    timon = crosstab(datasample(x2(x1=='Timon'),length(x2(x1=='Timon'))));
    titus = crosstab(datasample(x2(x1=='Titus Andronicus'),length(x2(x1=='Titus Andronicus'))));
    trolius = crosstab(datasample(x2(x1=='Troilus and Cressida'),length(x2(x1=='Troilus and Cressida'))));
    twelfth = crosstab(datasample(x2(x1=='Twelfth Night'),length(x2(x1=='Twelfth Night'))));
    twogent = crosstab(datasample(x2(x1=='Two Gentlemen Verona'),length(x2(x1=='Two Gentlemen Verona'))));
    twonoble = crosstab(datasample(x2(x1=='Two Noble Kinsmen'),length(x2(x1=='Two Noble Kinsmen'))));
    winters = crosstab(datasample(x2(x1=='Winters Tale'),length(x2(x1=='Winters Tale'))));
    permed = transpose(horzcat(henry4part1,henry6part1,henry4part2,henry6part2,henry6part3,alls, antony, asyou, errors, coriolanus, cymbeline, ed3, hamlet, henry5, henry8, julius, john, lear, loves, macbeth, measure, merchant, merry, midsummer, muchado, othello, pericles, richard2, richard3, romeo, sirthomas, spanish, taming, tempest, timon, titus, trolius, twelfth, twogent, twonoble, winters ));
    Data.F = permed;
    res(:,i) = CAalsInequal_SH_YEARS(Data,1,1);
end