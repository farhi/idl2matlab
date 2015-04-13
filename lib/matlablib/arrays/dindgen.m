function  mat=dindgen(varargin)
%function dindgen(d1, d2, ...)
%******** *******
%**

for i=1:length(varargin), dim(i)=varargin{i}; end;

mat=make_array('dimension',dim,'type',5,'index',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
