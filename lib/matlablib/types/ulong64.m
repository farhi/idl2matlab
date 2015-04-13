function  num=ulong64(val)
%function ulong64(val)
%******** *******
%**
if nargin ~=1, disp('!!! ulong64.m has to be completed...'); end;

num=double(val);
num=fix   (num);
