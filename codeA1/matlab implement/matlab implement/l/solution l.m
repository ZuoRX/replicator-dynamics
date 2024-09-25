clear;
clc;
global l;
X=linspace(0,1,100); 
Y=linspace(0,1,100);
[x,y]=meshgrid(X, Y); 
sizex=size(x);
fp=zeros(100);
fx=zeros(100);
fy=zeros(100);
fz=zeros(100);
for l=0.1:0.1:1
for i=1:sizex(1)
    for j=1:sizex(1)-i+1
        [fx(i,j),fy(i,j),fz(i,j)]=my_get_f(x(i,j),y(i,j));
        if((abs(fx(i,j))<0.005)&&(abs(fy(i,j))<0.005)&&(abs(fz(i,j))<0.005))
        
            fp(i,j)=l;
        end
    end
end
figure;
mesh(x,y,fp);
 xlabel('l');
 ylabel('d');
 zlabel('t');