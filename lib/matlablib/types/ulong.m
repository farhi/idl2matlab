function  num=ulong(val)
%function ulong(val)
%******** *****
%**
if nargin ~=1, disp('!!! ulong.m has to be completed...'); end;

 num=double(val);
 num=fix   (num);
