function  mat=findgen(varargin)
%function findgen(d1, d2, ...)
%******** *******
%**

for i=1:length(varargin), dim(i)=varargin{i}; end;

mat=make_array('dimension',dim,'type',4,'index',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
