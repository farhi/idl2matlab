function  mat=ul64indgen(varargin)
%function ul64indgen(d1, d2, ...)
%******** **********
%**

for i=1:length(varargin), dim(i)=varargin{i}; end;

mat=make_array('dimension',dim,'type',5,'index',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
