function [pos,szz]=widget_font(wid,fnt,pos,szz)
%******* ***************
%**

%ft_biggest , ft_bigger  , ft_normal , ft_smaller, ft_smallest
%ft_b_bigger, ft_b_normal, ft_propor

fs=8;
%fsize=11;
fsize=9;
weight='normal';
fname ='arial';
if isempty(fnt), fnt='default'; end;

switch fnt,
case 'ft_biggest'; fsize=16; weight='bold';
case 'ft_bigger';  fsize=14; weight='normal';
case 'ft_normal';  fsize=12; weight='normal';
case 'ft_smaller'; fsize=10; weight='normal';
case 'ft_smallest';fsize= 7; weight='normal';
case 'ft_b_bigger';fsize=14; weight='bold';
case 'ft_b_normal';fsize=12; weight='bold';
case 'ft_propor';  fsize=12; weight='bold';
otherwise;
end;

fac=fsize/fs;
if wid==0; pos=fac; return; end;
fii=int16(wid);

typ=get(wid,'type'); if typ == 'uicontrol', styl=get(wid,'style'); else, styl='?'; end;

set(wid,'FontSize',fsize,'FontWeight',weight,'FontName',fname);

if length(pos)==4,
   if szz(1)+szz(2) > 0;
      if szz(1) > 0, szz(1)=fac; pos(3)=round(pos(3)*fac); end;
      if szz(2) > 0, szz(2)=fac; pos(4)=round(pos(4)*fac);
              if styl ~= 'listbox'; ext=get(wid,'Extent'); pos(4)=ext(4); end; end;
      set(wid,'position',pos); end;  end;
