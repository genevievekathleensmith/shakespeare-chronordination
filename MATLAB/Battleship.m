function Battleship(Coord,Labels,Data,Mis,VarLabels,CoordColumn)

% Makes a battleship plot given coordinates

n=size(Coord,1);m=size(Data,2);OrderCoord=1;
if OrderCoord==1
   [y,i]=sort(Coord);
   i=(1:n);
   i=i(n:-1:1);
   Coord(i)=1:n;
end
scatter((m/20+1.5*m)*ones(n,1),Coord)
hold on
maxcoord=(max(Coord)-min(Coord))*1.1+min(Coord);T=zeros(m,n);
% Sort columns according to CoordColumn
[y,j]=sort(CoordColumn);
Data=Data(:,j);
for j=1:m
   %text (1+1.5*j,maxcoord,deblank(VarLabels{j}),...
   %   'HorizontalAlignment','center','FontSize',7);
   t=Data(:,j)~=Mis;k=1:n;k=k(t);z=zeros(1,n);
   z(k)=Data(k,j)*(.6/max(Data(k,j)));
   zk=(z(k)~=0).*max(.0*ones(size(z(k))),z(k));
   z(k)=zk;
   for i=1:n
      if (Data(i,j)~=Mis)&(z(i)~=0)
         T(i,j)=1;
         plot([1+1.5*j-z(i) 1+1.5*j+z(i)],[Coord(i) Coord(i)],'LineWidth',7)
      end
   end
end
for i=1:n
   printlabel=0;
   if (sum(T(i,:))>0)
      fullLabel=sprintf('%2.0f: %s',i,Labels{i});
      text (m/10+1.5*m,Coord(i),deblank(fullLabel),...
         'HorizontalAlignment','left','FontSize',7);
   end
end
axis off
drawnow
figure(gcf)
hold off