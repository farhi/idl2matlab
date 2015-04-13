function  vec6=bin_date(datt)
%*******       ********
%**

month=char('Jan','Fev','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');

vec6(1)=str2num (datt(21:24));
vec6(2)=strmatch(datt( 5:7),month);
vec6(3)=str2num (datt( 9:10));
vec6(4)=str2num (datt(12:13));
vec6(5)=str2num (datt(15:16));
vec6(6)=str2num (datt(18:19));
