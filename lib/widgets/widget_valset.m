function [vol,lx,ly]=widget_valset(base,val,but,append)
%*******     *************
%***
global I2Mfig I2Mfcar

if base > 0;
      fii=int16 (base);   fig=I2Mfig(fii,3);  par=I2Mfig(fii,2);
      but=I2Mfig(fii,16); szz=I2Mfig(fii,19:20);
else, fii=0; end;

switch but;

case  0;  %*** pushbutton ***
	  lx=length(val)    *I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case  1;  %*** exclusive ***
	  lx=(length(val)+3)*I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case  2;  %*** nonexcl ***
	  lx=(length(val)+3)*I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case  3;  %*** menu***
	  lx=(length(val)+3)*I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case  4;  %*** bar ***
	  lx=length(val)    *I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case 10;  %*** frame ***
	  lx=length(val)    *I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case 12;  %*** slider ***
	  lx=100; ly=I2Mfcar(2); vol=val;

case 13;  %*** text ***
	  ly=n_elements(val); lx=0; if isa(val,'i2mstr'), vol=val(:); else vol=val; end;
	  if ly > 1, for i=1:ly, lx=max(lx,length(vol{i})); end;
	  else,                  lx=length(vol);  end;
          lx=(lx+2)*I2Mfcar(1);  ly=ly*I2Mfcar(2);

case 14;  %*** draw *** NO VALUE
	  lx=length(val)*I2Mfcar(1); ly=I2Mfcar(2); vol=val; fii=0;

case 15;  %*** label ***
	  lx=length(val)*I2Mfcar(1); ly=I2Mfcar(2); vol=val;

case 16;  %*** list ***
	  sz =sizz(val); ly=sz(1); lx=0; nel=sz(sz(1)+1);
	  if ly > 0, vol=val(:);   ly=nel; for i=1:ly, lx=max(lx,length(vol{i})); end;
	  else,      vol=val;      ly=1; lx =length(vol); end;
          lx=(lx+3)*I2Mfcar(1);    ly=ly*I2Mfcar(2);

case 17;  %*** unknown ***

case 18;  %*** droplist ***
	  sz =sizz(val); ly=sz(1); lx=0; nel=sz(sz(1)+1);
	  if ly > 0, vol=val(:);   ly=nel; for i=1:ly, lx=max(lx,length(vol{i})); end;
	  else,      vol=val;      ly=1; lx =length(vol); end;
          lx=(lx+2)*I2Mfcar(1);    ly=I2Mfcar(2);

case 19;  %*** table *** NOT USED, see function I2Mwidg_tab in widget_control.
	  lx=length(val)*I2Mfcar(1); ly=I2Mfcar(2); vol=val; fii=0;
	  
otherwise;lx=length(val)*I2Mfcar(1); ly=I2Mfcar(2); vol=val; fii=0;
end;

if fii > 0;
   pos=get(base,'position');
   if  szz(1) > 0, lx=round(lx*szz(1)); if append, lx=max(pos(3),lx); end; pos(3)=lx; end;
   if  szz(2) > 0, ly=round(ly*szz(2)); if append, ly=   (pos(4)+ly); end; pos(4)=ly; end;
   I2Mfig(fii,7:10)=pos;
   if but == 12, set(base,'value' ,vol ,'position',pos);
   elseif append,valv=get(base,'string'); if isa(vol,'i2mstr'), valv=[valv(:)' vol(:)]; elseif iscell(vol), valv=[valv(:)' vol]; else, valv=[valv(:)' {vol}]; end;
                 set(base,'string',valv,'position',pos);
   else,         set(base,'string',vol ,'position',pos); end;
end;
