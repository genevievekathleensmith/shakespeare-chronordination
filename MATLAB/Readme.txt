Contrained Correspondence Analysis Matlab Toolbox:

Version 1
Date: 26/09/2008


The program "CAalsInequal.m" performs constrained CA analysis as described in the paper: 
"Seriation by Constrained Correspondence Analysis: A Simulation Study". 

Some directions concerning the input can be found in "CAalsInequal.m". 

At this moment very little checks are performed concerning input. Make sure that the input
is correctly specified. 

At this moment, CAalsInequal takes three elements as input: 

Data: 		A structure containing the data and all constraints.
Interval: 	A 0,1 variable indicating whether a global interval constraint is to be 		included (1) or not (0). 
		The limits for such an interval must be specified at the top (lines 66, 67)
		of the program.
Random Starts: 	A 0,1 variable indicating whether random starts are to be considered.

One data set (including labels and restrictions) can be found in the file Data1.mat

This version is work in progress. Suggestions and comments are welcome. 

Michel van de Velden

