function  mat=sindgen(varargin)
%function sindgen(d1, d2, ...)
%******** *******
%**

for i=1:length(varargin), dim(i)=varargin{i}; end;

mat=make_array('dimension',dim,'type',7,'index',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
