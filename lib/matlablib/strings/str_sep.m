function  lst=str_sep(str,sep)
%function str_sep(str,sep)
%******** *******
%**
remo=str; j=0; lst={''};
while remo; [token,remo]=strtok(remo,sep); if token; j=j+1; lst{j}=token; end; end;

%if length(lst) > 1, lst=i2mstr(lst); else, lst=lst{1}; end;
lst=i2mstr(lst);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
