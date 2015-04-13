function res=n_params(narg,vararg)
%*******     ********
%**
res=narg; ln=length(vararg); tot=0;
if fix(ln/2)*2 == ln, for i=1:2:ln, tmp=vararg{i}; if ~ischar(tmp); tot=0; break; end;
                                    if strmatch('I2M_',tmp), tot=tot+1; end; end; end;
if tot > 0, res=tot; end;
