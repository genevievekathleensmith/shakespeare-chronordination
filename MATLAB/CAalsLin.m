function [R,C,Fi]=CAalsLin(F,H)

% Perform correspondence analysis by alternating least squares
% Row principal, linear constraints H on the rows

conv=1e-8;p=1;[nr,nc]=size(F);PrintOutput=0;
%rand('state',0)
Dr=sum(F,2);T=(Dr==0);
Dr_5=diag((1-T).*(T+Dr).^-.5);Dr_1=diag((1-T).*(T+Dr).^-1);
n=sum(Dr);
Dr5=diag((Dr/n).^.5);
Dc=sum(F,1);T=(Dc==0);
Dc_5=diag((1-T).*(T+Dc).^-.5);
Dc5=diag((Dc/n).^.5);
E=Dr*Dc/n;FE=F-E;
HDrH_1=inv(H'*diag(Dr)*H);
C=rand(nc,p);C=C-ones(nc,1)*mean(C);
[P,Fi,Q]=svd(C,0);C=(n^.5)*Dc_5*P;
K=HDrH_1*H'*FE*C;R=H*K;

if isnan(R),
    C=R;
    Fi=R;
else
    [P,Fi,Q]=svd(R'*diag(Dr)*R,0);R=R*Q;C=C*Q;K=K*Q;
    Dr_5FEDc_5=Dr_5*FE*Dc_5;
    T=Dr_5FEDc_5-Dr5*R*C'*Dc5;

    s=sum(T(:).^2);

    iter=0;
    if PrintOutput==1;fprintf(' Stress after %5.0f iters = %12.8f \n',0,s);end
    sold=s+2*conv;

    while (sold-s)>conv
        iter=iter+1; sold=s;
        [P,Fi,Q]=svd(Dc_5*FE'*R*(1/n),0);C=(n^.5)*Dc_5*P*Q';
        K=HDrH_1*H'*FE*C;R=H*K;
        [P,Fi,Q]=svd(R'*diag(Dr)*R,0);R=R*Q;C=C*Q;K=K*Q;
        T=Dr_5FEDc_5-Dr5*R*C'*Dc5;
        s=sum(T(:).^2);
        if PrintOutput==1;fprintf(' Stress after %5.0f iters = %12.8f  %12.8f \n',iter,s,sold-s);end
        if sold<s-conv,disp(' Dysmonotony');end;
    end;
    if PrintOutput==1;fprintf(' Stress after %5.0f iters = %12.8f  %12.8f \n',iter,s,sold-s);end


end

