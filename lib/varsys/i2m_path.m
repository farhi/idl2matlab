function str=i2m_path(str)
%*******     ********
%**
p=path;
stp=strrep(str,[p ';'],'');
stp=strrep(stp,[p,':'],'');
stp=strrep(stp,[':',p],'');
stp=strrep(stp,[';',p],'');
addpath(stp);
