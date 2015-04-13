function s=i2msizz(dm1,dm2)
%******* *********
%**
s=0; % 1=mode horizontal 0=mode normal ...That is the question...

if s==1,
   if     nargin==1, s=[1 dm1];
   elseif nargin==2, s=0; if dm1 > 1 & dm2 ==1, s=1; end; %should transpose
   end;
   
else, %****************************************************

   if     nargin==1, s=[dm1 1];
   elseif nargin==2, s=0;
   end;
end;