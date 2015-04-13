function  mat=uindgen(varargin)
%function uindgen(d1, d2, ...)
%******** *******
%**

for i=1:length(varargin), dim(i)=varargin{i}; end;

mat=make_array('dimension',dim,'type',12,'index',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
