function [PredictedYears,R,C,Chi2]=CAalsInequal(Data)

%%%%%%%%%%%%%%%%%%%%%%%%%% Main Program %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This version by: Patrick Groenen & Michel van de Velden 
%                  Econometric Institute, Erasmus University, Rotterdam
% 
% For comments/questions/suggestions mail Michel van de Velden
%                                         vandevelden@few.eur.nl
%
% 
%
% Uses: CheckForOverlap, MakeLinRestrictions, CAalsLin, LSIcon, TransPrep,
%       ExpandSpline, ExpandFixedFunction, Battleship

% Perform correspondence analysis by alternating least squares
% Incorporating linear equality and inequality restrictions on the rows
% Row principal
%
% Input: It is important that all fields exist. If no constraints exist use
% an empty matrix. The program does not check for bad input. 

%   Data                Structure containing all information:

%   Data.F              Contingency table. Rows are assemblages, columns are artefacts
%   Data.Labels         Cell array with labels for each row.
%   Data.IndYears       Used for year constraints. Column vector with index for assemblages with known year.
%   Data.Years          Used for year constraints. Column vector with known year.
%   Data.IndEqual       Used for equality constraints. Each row gives pairs indices of
%                       equal rows
%   Data.IndInequal     Used for inequality constraints. Each row gives a pairs of row indices:
%                       the first index is smaller than the second index (column 1 is
%                       older than column 2)
%   Data.InequalYears:  Inequality constraint based on years. Size: Number of constraints x 3: 
%                       column 1: Index of the constrained year
%                       column 2: -1 or +1 : -1 indicates site dates from
%                       before specified date, +1 indicates it is from after specified date.
%                       column 3: specified date.
%                       Note: These can only be incorporated when we have linear year constraints.
%                       
%   Interval            0 or 1, indicating whether all dates are to be in a
%                       certain interval (1) or not (0). The actual dates for the Interval
%                       must be defined at the beginning of the program in "LowerBound" and "UpperBound".
%   RS                  0 or 1, indicating whether several Randomstarts
%                       will be used. The number of randomstarts is set as
%                       a constant rs at the beginning of the program.
%
%
%
% Output:
%   R               (constrained) row coordinates
%   C               (unconstrained) column coordinates
%   Chi2            Explained Inertia
%   PredictedYears  predicted years by the current model
%
% Parameter settings and some initialisations:
%   conv:            stopping criterion
%   p:               CA dimensionality. Equal to 1.
%   LowerBound:      The lowerbound for the interval contraint (if applicable, else ignored)
%   UpperBound:      The upperbound for the interval contraint (if applicable, else ignored)
%   PrintOutput:     0 no printed output, 1 printed output


conv=1e-6;
p=1;
LowerBound=  1589.5;
UpperBound= 1614;

randomstart=logical(1);
Chi2=0;
rs=10;
AllKnots=2;
Order=1;
Check=[];
Interval=logical(1);

% Print options (set to 1 if printed, 0 do not print)
PrintOutput        = 1; % Print the reconstructed years.
PrintIterations    = 0; % Print the iterations.
MakeBattleshipPlot = 0; % Make battleship plot.

F          = Data.F;
Labels     = Data.Labels;
IndYear    = Data.IndYears;
Year       = Data.Years;
IndEqual   = Data.IndEqual;
IndInequal = Data.IndInequal;
InequalYears=Data.InequalYears;
Fase       = Data.Labels;

[nr,nc]      = size(F);
KnotSequence = [];

% Make linear restriction matrix

% Check to see if there are overlapping Year and Equality constraints.
% Incorporate the overlapping constraints in the Year constraints
[IndEqual,IndYear,Year] = CheckForOverlap(IndEqual,IndYear',Year);
[H,Base,KnotSequence]   = MakeLinRestrictions(IndYear,Year,nr,IndEqual,AllKnots,Order,KnotSequence);

% Add an inequality constraint based on the Year constraint. That is,
% min(IndYear) < max(IndYear)
IndInequal=[IndInequal; IndYear(1) IndYear(length(IndYear))];

% Test for valid H. 

if isempty(H)
    error('Empty H');
end

NumbEqual   = size(IndEqual,1);
NumbYears   = size(Year,1);

% Make inequality matrix
nInequals = size(IndInequal,1);
G = zeros(nInequals,nr);
for i=1:nInequals
    G(i,IndInequal(i,:)) = [-1 1];
end

% Add inequality constraints for montone spline
RankSplineBasis = size(Base,2);
A = H(:,1:RankSplineBasis);
A = inv(A'*A)*A';
G = [G;A];
h = zeros(size(G,1),1);

MinYearInd = IndYear(1);
MaxYearInd = IndYear(length(Year));
MinYear    = Year(1);
MaxYear    = Year(length(Year));

% Incorporate Inequality Constraints with Years: Type 4. Only possible with
% linear time transformations.

if not(isempty(InequalYears)),
    Z=[];
    for i=1:size(InequalYears,1),
        gamma = (InequalYears(i,3)-MinYear)/(MaxYear-MinYear);
        z     = zeros(1,nr);
        z(1,[InequalYears(i,1) MinYearInd MaxYearInd])=[InequalYears(i,2) InequalYears(i,2)*(gamma-1) -InequalYears(i,2)*gamma];
        Z     = [Z;z];
    end
    G = [G;Z];
    h = [h; zeros(size(InequalYears,1),1)];
end

if Interval
    
    IndInequal3     = (1:nr);
    IndInequal3Sign = [1 -1];

    YearLow         = ones(nr,1)*LowerBound;
    YearHigh        = ones(nr,1)*UpperBound;
    RelYearLow      = (YearLow-MinYear)/(MaxYear-MinYear);
    RelYearHigh     = (YearHigh-MinYear)/(MaxYear-MinYear);
    Gl              = zeros(nr,nr);
    Gu              = zeros(nr,nr);
    for i=1:nr
        Gl(i,[IndInequal3(i) MinYearInd MaxYearInd])=...
            [IndInequal3Sign(1) IndInequal3Sign(1)*(RelYearLow(i)-1) -IndInequal3Sign(1)*RelYearLow(i)];
        Gu(i,[IndInequal3(i) MinYearInd MaxYearInd])=...
            [IndInequal3Sign(2) IndInequal3Sign(2)*(RelYearHigh(i)-1) -IndInequal3Sign(2)*RelYearHigh(i)];
    end
    G = [G;Gl;Gu];
    h = [h;zeros(2*nr,1)];
end

% Compute CA weights Dr and Dc and some powers
Dr   = sum(F,2);                  % Compute row sum of F.
n    = sum(Dr);                   % Compute total sum of F.
T    = (Dr==0);                   % Check for zero row frequencies.
Dr_5 = diag((1-T).*(T+Dr).^-.5);  % Compute Dr^(-.5).
Dr_1 = diag((1-T).*(T+Dr).^-1);   % Compute Dr^(-1).
Dr5  = diag(Dr.^.5);              % Compute Dr^(+.5).
Dc   = sum(F,1);                  % Compute column sum of F.
T    = (Dc==0);                   % Check for zero column frequencies.
Dc_5 = diag((1-T).*(T+Dc).^-.5);  % Compute Dc^(-.5).
Dc_1 = diag((1-T).*(T+Dc).^-1);   % Compute Dc^(-1).
Dc_5 = diag(Dc.^-.5);               % Compute Dc^(-.5).
Dc5  = diag(Dc.^.5);              % Compute Dc^(+.5).
E    = Dr*Dc/n;                   % Compute expected frequencies under the independence model.
FE   = F - E;

% Compute nullspace and construct vector of zeros for restrictions

Hnull = null(H')';
d     = zeros(size(Hnull,1),1);

if not(randomstart)     % No random start
    [R,C,Fi]=CAalsLin(F,H);
    rs=1;
end

for i=1:rs,

    if randomstart,

    C = rand(nc,p);
    C = C - ones(nc,1)*mean(C);
    [P,Fi,Q] = svd(C,0);
    C = Dc_5*P;
    R = Dr_5*FE*C;
    end
    % Check if order of known years in R is reversed
    if R(MinYearInd)>R(MaxYearInd)
        C = -1*C;      % If so, then reflect C.
    end
    [P,Fi,Q] = svd(C,0);
    C = Dc_5*P;
    f = Dr_5*FE*C;
    % Second check if order of known years in R is reversed
    if f(MinYearInd)>f(MaxYearInd)
        C = -1*C;       % If so, then reflect C
        f = -1*f;       % and reflect f.
    end

    % Solve the quadratic program in R subject to G*R>=h and Hnull*R=d
    % with h and d vectors of zeros of appropriate length.
    R = LSIcon(f,Dr_5,G,h,Hnull,d);

    % Compute first loss.
    T    = Dr_5*(FE-(1/n)*diag(Dr)*R*C'*diag(Dc))*Dc_5;
    s    = sum(T(:).^2);       % Loss value.
    iter = 0;                  % Initialize iteration counter
    if PrintIterations==1; % Print iteration history
        fprintf(' Stress after %5.0f iters = %12.8f  %12.8f \n',iter,s,0);
    end
    sold = s+2*conv;
    while abs(sold-s)>conv
        Check = [Check; [f(MinYearInd)>f(MaxYearInd) R(MinYearInd)>R(MaxYearInd)]];

        if R(MinYearInd)>R(MaxYearInd) % Reflect C if R is in the wrong order.
            C = -1*C;
        end

        iter = iter+1;
        sold = s;
        [P,Fi,Q] = svd(Dc_5*FE'*R*(1/n),0); % Compute unconstrained update
        C    = Dc_5*P*Q';                   % Update C.
        f    = Dr_5*FE*C;                   % Unconstrained update of R

        if f(MinYearInd)>f(MaxYearInd) % Check for wrong order.
            C = -1*C;                    % Reflect C
            f = -1*f;                    % and f.
        end

        % Compute constrained update by solving
        % the quadratic program in R subject to G*R>=h and Hnull*R=d
        % with h and d vectors of zeros of appropriate length.
        R = LSIcon(n*f,Dr5,G,h,Hnull,d);

        % Compute loss.
        T = Dr_5*(FE-(1/n)*diag(Dr)*R*C'*diag(Dc))*Dc_5;
        s = sum(T(:).^2);
        if PrintIterations==1; % Print iteration history
            fprintf(' Stress after %5.0f iters = %12.8f  %12.8f \n',iter,s,sold-s);
        end
    

        if R(MinYearInd)>R(MaxYearInd) % Reflect C, R, and f if R is in the wrong order.
            R = -R;
            C = -C;
            f = -f;
        end
    end
    
    % Compute the years by the Spline way
    B = H(:,1:(RankSplineBasis+1));
    Weights = inv(B'*B)*B'*R;
    if Order>=1
        trans='mspline';
        [DataInit,iord,ties,Base2,KnotSequence1] = TransPrep([Year;KnotSequence],trans,AllKnots,Order,KnotSequence);
        Base2=[Base2 ones(size(Base2,1),1)];
        [Coord,Xval,Years2] = ExpandSpline([Year;KnotSequence],Base2,KnotSequence,Weights,R,Order);
    else
        [Coord,Xval,Years2] = ExpandFixedFunction(Year',Weights,R,Order);
    end

    % Compute the years the old fashioned way
    Years = MinYear+((R-R(MinYearInd))/(R(MaxYearInd)-R(MinYearInd)))*(MaxYear-MinYear);

    TotChi2 = sum(sum( ((F-E).^2)./E));
    T       = (1/n^.5)*Dr5*R*C'*Dc5;
    RecChi2 = sum(T(:).^2);
    Chi2i    = RecChi2/TotChi2;
    if Chi2i>Chi2,      % Better solution is attained: Update Rsolution, Csolution, PredictedYears and Chi2
                        % with the current results.
        Rsolution=R;
        Csolution=C;
        PredictedYearssolution = Years;
        Chi2=Chi2i;
    end

end   % for loop randomstarts

R=Rsolution;
C=Csolution;
PredictedYears=PredictedYearssolution;

if PrintOutput,
    % Compute row contribution
    Fi2            = R'*diag(Dr/n)*R;
    RowConToDim    = diag(Dr/n)*R.^2/Fi2;
    RowTotChi2Dist = sum((Dr_1*F-ones(nr,1)*Dc/n).^2./(ones(nr,1)*Dc/n),2);
    RowRecChi2Dist = R.^2/n;
    RowPropRec     = RowRecChi2Dist./RowTotChi2Dist;

    fprintf(' Stress after %5.0f iters = %12.8f  %12.8f \n',iter,s,sold-s);



    [y,j] = sort(R); % Find index vector j to obtain permutation such that the solution can
    % be printed from lowest reconstructed year to
    % the highest.
    fprintf('\n')
    fprintf('Year nr Label                  R     Observations  Contr  Contr to \n')
    fprintf('                                                   dim    total dist \n')
    for i=1:nr
        fprintf('%4.0f %2.0f %-19s %6.1f %14.0f %6.3f %6.3f \n',...
            Years(j(i)),...
            j(i),...
            Fase{j(i)},R(j(i)),Dr(j(i)),RowConToDim(j(i)),RowPropRec(j(i)))
    end
    TotChi2 = sum(sum( ((F-E).^2)./E));
    T       = (1/n^.5)*Dr5*R*C'*Dc5;
    RecChi2 = sum(T(:).^2);
    fprintf('\nTotal Chi-square:         %10.3f \n',TotChi2)
    fprintf(  'Reconstructed Chi-square: %10.3f (%5.3f%%)\n',...
        RecChi2,100*RecChi2/TotChi2)

    % Make a battleship type of plot.
    if MakeBattleshipPlot
        s=[];
        for i=1:length(Dr)
            s = [s sprintf('%3.0f',i)];
        end
        Battleship(R,Labels,F,99999,s,C)
        [s,cj] = sort(C);
        Ft = F*Dc_1;
        Ft = Ft(j,cj);ones(nr,1)*max(Ft,[],1);
        Ft = -Ft;
        pcolor(Ft)
        colormap(gray(30))
    end
end



end
