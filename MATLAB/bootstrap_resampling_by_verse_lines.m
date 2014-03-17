
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

load('table_data.mat')

% use the 'IMPORT DATA' button to import the file verse_line_counts.csv
% import two items: title and verse_lines

title = nominal(title)

res = []
for i = 1:1000
    henry4part1 = crosstab(datasample(x2(x1=='1 Henry 4'),verse_lines(title=='1 Henry 4')));
    henry6part1 = crosstab(datasample(x2(x1=='1 Henry 6'),verse_lines(title=='1 Henry 6')));
    henry4part2 = crosstab(datasample(x2(x1=='2 Henry 4'),verse_lines(title=='2 Henry 4')));
    henry6part2 = crosstab(datasample(x2(x1=='2 Henry 6'),verse_lines(title=='2 Henry 6')));
    henry6part3 = crosstab(datasample(x2(x1=='3 Henry 6'),verse_lines(title=='3 Henry 6')));
    alls = crosstab(datasample(x2(x1=='Alls Well'),verse_lines(title=='Alls Well')));
    antony = crosstab(datasample(x2(x1=='Antony and Cleopatra'),verse_lines(title=='Antony and Cleopatra')));
    asyou = crosstab(datasample(x2(x1=='As You Like It'),verse_lines(title=='As You Like It')));
    errors = crosstab(datasample(x2(x1=='Comedy of Errors'),verse_lines(title=='Comedy of Errors')));
    coriolanus = crosstab(datasample(x2(x1=='Coriolanus'),verse_lines(title=='Coriolanus')));
    cymbeline = crosstab(datasample(x2(x1=='Cymbeline'),verse_lines(title=='Cymbeline')));
    ed3 = crosstab(datasample(x2(x1=='Edward 3'),verse_lines(title=='Edward 3')));
    hamlet = crosstab(datasample(x2(x1=='Hamlet'),verse_lines(title=='Hamlet')));
    henry5 = crosstab(datasample(x2(x1=='Henry 5'),verse_lines(title=='Henry 5')));
    henry8 = crosstab(datasample(x2(x1=='Henry 8'),verse_lines(title=='Henry 8')));
    julius = crosstab(datasample(x2(x1=='Julius Caesar'),verse_lines(title=='Julius Caesar')));
    john = crosstab(datasample(x2(x1=='King John'),verse_lines(title=='King John')));
    lear = crosstab(datasample(x2(x1=='King Lear'),verse_lines(title=='King Lear')));
    loves = crosstab(datasample(x2(x1=='Loves Labours Lost'),verse_lines(title=='Loves Labours Lost')));
    macbeth = crosstab(datasample(x2(x1=='Macbeth'),verse_lines(title=='Macbeth')));
    measure = crosstab(datasample(x2(x1=='Measure for Measure'),verse_lines(title=='Measure for Measure')));
    merchant = crosstab(datasample(x2(x1=='Merchant of Venice'),verse_lines(title=='Merchant of Venice')));
    merry = crosstab(datasample(x2(x1=='Merry Wives'),verse_lines(title=='Merry Wives')));
    midsummer = crosstab(datasample(x2(x1=='Midsummer Nights Dream'),verse_lines(title=='Midsummer Nights Dream')));
    muchado = crosstab(datasample(x2(x1=='Much Ado'),verse_lines(title=='Much Ado')));
    othello = crosstab(datasample(x2(x1=='Othello'),verse_lines(title=='Othello')));
    pericles = crosstab(datasample(x2(x1=='Pericles'),verse_lines(title=='Pericles')));
    richard2 = crosstab(datasample(x2(x1=='Richard 2'),verse_lines(title=='Richard 2')));
    richard3 = crosstab(datasample(x2(x1=='Richard 3'),verse_lines(title=='Richard 3')));
    romeo = crosstab(datasample(x2(x1=='Romeo and Juliet'),verse_lines(title=='Romeo and Juliet')));
    sirthomas = crosstab(datasample(x2(x1=='Sir Thomas More Iid'),verse_lines(title=='Sir Thomas More Iid')));
    spanish = crosstab(datasample(x2(x1=='Spanish Tragedy Add.'),verse_lines(title=='Spanish Tragedy Add.')));
    taming = crosstab(datasample(x2(x1=='Taming of the Shrew'),verse_lines(title=='Taming of the Shrew')));
    tempest = crosstab(datasample(x2(x1=='Tempest'),verse_lines(title=='Tempest')));
    timon = crosstab(datasample(x2(x1=='Timon'),verse_lines(title=='Timon')));
    titus = crosstab(datasample(x2(x1=='Titus Andronicus'),verse_lines(title=='Titus Andronicus')));
    trolius = crosstab(datasample(x2(x1=='Troilus and Cressida'),verse_lines(title=='Troilus and Cressida')));
    twelfth = crosstab(datasample(x2(x1=='Twelfth Night'),verse_lines(title=='Twelfth Night')));
    twogent = crosstab(datasample(x2(x1=='Two Gentlemen Verona'),verse_lines(title=='Two Gentlemen Verona')));
    twonoble = crosstab(datasample(x2(x1=='Two Noble Kinsmen'),verse_lines(title=='Two Noble Kinsmen')));
    winters = crosstab(datasample(x2(x1=='Winters Tale'),verse_lines(title=='Winters Tale')));
    permed = transpose(horzcat(henry4part1,henry6part1,henry4part2,henry6part2,henry6part3,alls, antony, asyou, errors, coriolanus, cymbeline, ed3, hamlet, henry5, henry8, julius, john, lear, loves, macbeth, measure, merchant, merry, midsummer, muchado, othello, pericles, richard2, richard3, romeo, sirthomas, spanish, taming, tempest, timon, titus, trolius, twelfth, twogent, twonoble, winters ));
    Data.F = permed;
    res(:,i) = CAalsInequal_SH_YEARS(Data,1,1);
end