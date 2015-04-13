function  n=n_tags(var)
%function n_tags(var)
%******** ******
%**
if nargin ~=1, disp('!!! n_tags.m has to be completed...'); end;

if isstruct(var); n=length(fieldnames(var)); else n=0; end;
