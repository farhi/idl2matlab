////////////////////////////////////////////////
//// File generated by IDL2Matlab V2.0.     ////
////////////////////////////////////////////////

function [varargout] = nb_prem(varargin)

////Initialization of parameters
  I2Mkwn=['I2M_a1', 'I2M_pos'];
  I2Mkwv=['lim', 'I2M_pos'];
  lim=[]; I2M_pos=[];
  I2M_lst=[];  I2M_out=[];  lv=length(varargin); if (~(modulo(lv,2)==0)), I2M_ok=0;  else,   I2M_ok=1;
  for I2M=1:2:lv, I2M_tmp=varargin(I2M);	if (typeof(I2M_tmp)~='string'); I2M_ok=0;break;	end;I2Mx=strindex(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1;	I2M_ok=0;break;	end; str=string(I2Mkwv(I2Mx));	str=str+'=';str=str+string(varargin(I2M+1)); str=str+';';execstr([str]);	I2M_lst((I2M+1)/2)=I2Mkwv(I2Mx); end;  end;
  if ~I2M_ok; for I2M=1:lv; str=string(I2Mkwv(I2M)); str=str+'='; str=str+string(varargin(I2M)); str=str+';';  execstr([str]);end; end;
  if ~isempty(I2M_pos);	for I2M=1:length(I2M_pos); str=sprintf("varargout(%d)=I2M_lst(I2M_pos(%d));",I2M,I2M); I2M_out= [I2M_out  str];	end; end;

////End of parameters initialization

  cour_int = 1;
  compt = 1;
  str = 'liste des ' + string(lim) + ' nombres premiers :';
  disp(str);
  while ((compt < lim | compt == lim)),
    if (verif_prem(cour_int))
      disp(cour_int);
      compt = compt + 1;
    end//if

    cour_int = cour_int + 2;
  end//while

if ~(isempty(I2M_out)); eval(I2M_out); else varargout(1)=varargin(1); end 
return;
//// end of function nb_prem
