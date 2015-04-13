function [wid,varargout]=widget_button(varargin)
%FUNCTION widget_button, base, ...
%*** ******************
%***
%DEFAULT: no_release
%UNUSED : bitmap

    I2Mkwn=char('I2M_a1' , 'notify_realize' , 'event_pro' , 'event_func', 'func_get_value' , 'pro_set_value' , 'map' , 'value' , 'uvalue' , 'font' , 'menu' , 'xsize' , 'ysize' , 'scr_xsize' , 'scr_ysize' , 'sensitive' , 'uname' , 'no_release' , 'separator' , 'resource_name' , 'I2M_pos');
    I2Mkwv=    {'bose'   , 'not_r'          , 'eventp'    , 'eventf'    , 'f_get'          , 'p_set'         , 'map' , 'vol'   , 'uval'   , 'font' , 'menu' , 'xsize' , 'ysize' , 'scr_xs'    , 'scr_ys'    , 'sens'      , 'uname' , 'norel'      , 'sepa'      , 'resource_name' , 'I2M_pos'};
    bose=[]; not_r=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; map=[]; vol=[]; uval=[]; font=[]; menu=[]; xsize=[]; ysize=[]; scr_xs=[]; scr_ys=[]; sens=[]; uname=[]; norel=[]; sepa=[]; resource_name=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig I2Mfevn I2Mnotr I2Msget I2Mfseq I2Mfcar I2Mfun

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top, 12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc, 19:20=szz
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

base=I2Mfig(int16(bose),1);
if isempty(map);  map=1;    end;  mop=I2Mfig(int16(base),5); if mop, mop=map; end;
if isempty(uval); uval=[];  end;
if isempty(vol);   val=' '; else, val=vol; end;
if isempty(sens); sens=1;   end;
if isempty(menu); menu=0;   end;
ibas  =int16(base);
fig   =I2Mfig(ibas, 3);
but   =I2Mfig(ibas,16);
grp   =I2Mfig(ibas,17);
bgc   =I2Mfig(ibas,18);
szz   =[1 1];
pip   =0;
[val,lx,ly]=widget_valset(0,val,but);

if scr_xs, xsz=scr_xs; szz(1)=0; elseif xsize, xsz=xsize; szz(1)=0; else xsz=lx; end;
if scr_ys, ysz=scr_ys; szz(2)=0; elseif ysize; ysz=ysize; szz(2)=0; else ysz=ly; end;

%MenuBar & CascadeMenu
%*********************
if but ==3;   I2Mfig(ibas,16)=3.5; ctx=widget_button('I2M_a1',base); but=3.6; I2Mfig(ibas,16)=but; I2Mfig(ibas,17)=ctx; end;
if but ==3.5; wid=uicontextmenu('parent',fig,'visible','off'); pip=1; pos=[0  0  0  0]; map=1; end;
if but ==3.6; base=I2Mfig(ibas,17); ibas=int16(base); but=4; end;
if but ==4;
  ui=I2Mfig(ibas,6); if ui ~=2, here=fig; else, here=base; end;
  I2Mfig(ibas,7)=I2Mfig(ibas,7)+1;
  pos=[0  0  0  0]; pos1=I2Mfig(ibas,7);
  if menu>0 pip=1; end;
  wid=uimenu(here,'label',val,'position',pos1,'userdata',uval); if sepa, set(wid,'separator','on'); end;
      if ~mop;  set(wid,'visible','off'); end;
      if ~sens; set(wid,'enable' ,'off'); end;
  end;
if (but < 3) | (but > 4);
%Normal button
%*************
  pos=[0  0  xsz  ysz]; style='pushbutton';
%Exclusive buttons
%*****************
  if     but == 1, style='radiobutton';
%Non-exclusive buttons
%*********************
  elseif but == 2, style='checkbox';
  else   but  = 0; end;
%CascadeButton asked
%*******************
  if menu > 0, style='pushbutton';   pos(3)=pos(3)+3*I2Mfcar(1); but=3; end;

%*********
  wid=uicontrol(fig,'style',style,'position',pos,'string',val,'userdata',uval);
      
 [pos,szz]=widget_font    (wid,font,pos,szz);
  bgc     =widget_resource(wid,bgc,resource_name,mop,sens);
end;
fii=double(int16(wid));
top=I2Mfig(ibas,11);

I2Mfig(fii,1)   =wid; I2Mfig(fii, 2)=base; I2Mfig(fii,3)=fig;
I2Mfig(fii,4)   =3;   I2Mfig(fii, 5)=map;  I2Mfig(fii,6)=2;
I2Mfig(fii,7:10)=pos; I2Mfig(fii,15)=sens; I2Mfig(fii,16)=but;
I2Mfig(fii,17)  =grp; I2Mfig(fii,18)=bgc;  I2Mfig(fii,19:20)=szz;
I2Mfig(fii,11)  =top; I2Mfig(fii,12)=I2Mfseq;
I2Mfun{fii}=char(uname);

if ~isempty(not_r), I2Mnotr{fii}  =not_r; else, I2Mnotr{fii}  =''; end;
if ~isempty(f_get), I2Msget{fii,1}=f_get; else, I2Msget{fii,1}=''; end;
if ~isempty(p_set), I2Msget{fii,2}=p_set; else, I2Msget{fii,2}=''; end;

if     ~isempty(eventf); I2Mfevn{fii}=['___' eventf];
elseif ~isempty(eventp); I2Mfevn{fii}=eventp; else I2Mfevn{fii}=''; end;
ev=['struct(''structure_name'',''WIDGET_BUTTON'',''id'',' num2str(fii) ',''select'',1)'];
if ~pip, set(wid,'Callback',['widget_i2mevent(' ev ');']); end;

I2Mfseq=I2Mfseq+1;

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
