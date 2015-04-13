function cloze(lun)
%PRO cloze, lun
%*** *****
%***

global I2Mflu0 I2Mflu1 I2Mflu2 I2Mflu3 I2Mflu4

if ~isempty(I2Mflu1),
  idx=where(I2Mflu1 == lun); idx=idx+1;
  if idx(1) >0,     I2Mflu1(idx)=0;   try, fclose(I2Mflu2(idx)); catch, end;
	            if I2Mflu3(idx),  delete(I2Mflu0{idx}); end; end;

  idx=find(I2Mflu1);
  if ~isempty(idx), I2Mflu0=I2Mflu0(idx);   I2Mflu1=I2Mflu1(idx);
                    I2Mflu2=I2Mflu2(idx);   I2Mflu3=I2Mflu3(idx);   I2Mflu4=I2Mflu4(idx);
  else,               I2Mflu0={}; I2Mflu1=[]; I2Mflu2=[]; I2Mflu3=[]; I2Mflu4={}; end;
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
