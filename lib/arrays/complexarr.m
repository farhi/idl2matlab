function  mat=complexarr(varargin)
%function complexarr(d1, d2, ...)
%******** **********
%**
ln=length(varargin);
if ln == 1; sz=[1 varargin{1}]; else for i=ln:-1:1; sz(i)=varargin{i}; end; end;
tmp=zeros(sz);
mat=complex(tmp,tmp);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

