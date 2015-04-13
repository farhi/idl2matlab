function res=shift(mat,s1,s2,s3,s4,s5,s6,s7,s8)
%*******     *****
%**
sz=size(mat); if sz(1)==1, sz=sz(2:end); elseif sz(2)==1, sz=sz(1); end;
nd=length(sz); ns=nargin-1;

                                 x1=shiif(sz(1),s1);
if nd > 1, if ns < 2, s2=0; end; x2=shiif(sz(2),s2); end
if nd > 2, if ns < 3, s3=0; end; x3=shiif(sz(3),s3); end
if nd > 3, if ns < 4, s4=0; end; x4=shiif(sz(4),s4); end
if nd > 4, if ns < 5, s5=0; end; x5=shiif(sz(5),s5); end
if nd > 5, if ns < 6, s6=0; end; x6=shiif(sz(6),s6); end
if nd > 6, if ns < 7, s7=0; end; x7=shiif(sz(7),s7); end
if nd > 7, if ns < 8, s8=0; end; x8=shiif(sz(8),s8); end

if     nd== 1, res=mat(x1);
elseif nd== 2, res=mat(x1,x2);
elseif nd== 3, res=mat(x1,x2,x3);
elseif nd== 4, res=mat(x1,x2,x3,x4);
elseif nd== 5, res=mat(x1,x2,x3,x4,x5);
elseif nd== 6, res=mat(x1,x2,x3,x4,x5,x6);
elseif nd== 7, res=mat(x1,x2,x3,x4,x5,x6,x7);
elseif nd== 8, res=mat(x1,x2,x3,x4,x5,x6,x7,x8); end;




function x1=shiif(sz,s1)
%*******    *****
x1=1:sz(1); if     s1>0, idb=x1(end-s1+1:end); x1=x1-s1; x1(1:s1)=idb;
            elseif s1<0, ida=x1(1:-s1);        x1=x1-s1; x1(end+s1+1:end)=ida; end;
