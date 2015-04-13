function res=dcomplex(real,imagin)
%******* ************

if nargin  >2, disp('!!! dcomplex.m has to be completed...');    end;
if nargin ==1, res=complex(real); else res=complex(real,imagin); end;
