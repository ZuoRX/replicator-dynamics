function [fx,fy,fz]=my_get_f(x,y)
global l;
t=1;
c=t; 
d=4*t;
% l=0.5*t;
betac=1.1;
betad=1.5;
Pd=0;
Pc=0;
Pl=0;
M=100;
N=50;
for Nd=0:(N-1)
   for Nl=0:(N-1-Nd)
    Nc    = N-1-Nd-Nl;
    pai_d =d*betad*M/(betad*(Nd+1)*d +betac*Nc*c+Nl*l)-d;
    pai_c =betac*c*M/(betad*Nd*d+betac*(Nc+1)*c +Nl*l)-c;
    pai_l =l*M/(betad*Nd*d+betac*Nc*c +(Nl+1)*l)-l;
    Pd = Pd + nchoosek(N-1,Nd)*nchoosek(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_d;
    Pc = Pc + nchoosek(N-1,Nd)*nchoosek(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_c;
    Pl = Pl + nchoosek(N-1,Nd)*nchoosek(N-1-Nd,Nl)*y^Nd*(1-x-y)^Nc*x^Nl*pai_l;
%     disp(['Nl=',num2str(Nl)]);
   end
%    disp(['Nd=',num2str(Nd)]);
end
P_=(1-x-y)*Pc+x*Pl+y*Pd;
fx=x*(Pl-P_);
fy=y*(Pd-P_);
fz=(1-x-y)*(Pc-P_);
end