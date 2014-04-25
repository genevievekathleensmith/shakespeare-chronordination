
% Load in the table of play abbreviations and pauses built using the R
% script "building_long-form_tables.R"
% This will produce two objects: A and P

load('abbrvs_and_pauses.mat')

% Convert A and P to nominal variables, called x1 and x2
x1 = nominal(A)
x2 = nominal(P)

% Create a table of counts for each pause type for each play
% Also store the LABELS (the play titles and pause types)
[tab,CHI2,p,LABELS] = crosstab(x1,x2)

% Create new variables from the LABELS object
playtitles = LABELS(:,1)
pausepositions = LABELS(:,2)

% Load in the Shakespeare data format, which includes the anchor
% specifications

load('ShData.mat')

% Put the playtitles into the Data object
Data.Labels = playtitles

% Put the count table into the Data object
Data.F = tab

% Run the modified Constrained Correspondence Analysis Function and write
% the predictions to a .csv file
rawpredictions = CAalsInequal_SH_YEARS(Data,1,1)
csvwrite('CCA_rawpredictions.csv',rawpredictions,0,0)

% Load in the verse line counts for each play (also built using the R
% script mentioned above), this will create a new object, V
load('verse_lines.mat')

% Create a nominal version of the abbreviations in the object V
abbrv = nominal(V.abbrv)

% Create two new variables
% newcounts will store the new counts for each play after each iteration, i, of
% the bootstrapping and each time through it will be used to overwrite
% Data.F

% res will hold the result of running the CCA function on each new version
% of newcounts
newcounts = []
res = []

% Run the bootstrap procedure 1000 times
for i = 1:1000
    % Iterate through each play
    for j = 1:length(playtitles)
        % build a table by sampling the data from our list of pauses from
        % each play, each sample will be equal in size to the number of
        % verse lines recorded for that play
        newcounts(j,:) = crosstab(datasample(x2(x1==playtitles(j)),V.verse_lines(abbrv==playtitles(j))));
    end

    Data.F = newcounts
    res(:,i) = CAalsInequal_SH_YEARS(Data,1,1);
end

% Write the bootstrap results to a .csv file
csvwrite('CCA_bootpredictions.csv',res,0,0)

