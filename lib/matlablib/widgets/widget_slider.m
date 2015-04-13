function [wid,varargout]=widget_slider(varargin)
%FUCTION widget_slider, base, ...
%*** *****************
%***
%UNUSED : drag, scroll, title?

    I2Mkwn=char('I2M_a1' , 'notify_realize' , 'event_pro' , 'event_func' , 'func_get_value' , 'pro_set_value' , 'map' , 'value' , 'uvalue' , 'font' , 'xsize' , 'ysize' , 'scr_xsize' , 'scr_ysize' , 'sensitive' , 'uname' , 'resource_name' , 'vertical' , 'drag' , 'maximum' , 'minimum' , 'suppress_value' , 'title' , 'I2M_pos' );
    I2Mkwv=    {'bise'   , 'not_r'          , 'eventp'    , 'eventf'     , 'f_get'          , 'p_set'         , 'map' , 'val'   , 'uval'   , 'font' , 'xsize' , 'ysize' , 'scr_xs'    , 'scr_ys'    , 'sens'      , 'uname' , 'resource_name' , 'vert'     , 'drag' , 'maxim'   , 'minim'   , 'suppress'       , 'title' , 'I2M_pos' };
    bise=[]; not_r=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; map=[]; val=[]; uval=[]; font=[]; xsize=[]; ysize=[]; scr_xs=[]; scr_ys=[]; sens=[]; uname=[]; resource_name=[]; vert=[]; drag=[]; maxim=[]; minim=[]; suppress=[]; title=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig I2Mfevn I2Mnotr I2Msget I2Mfseq I2Mfcar I2Mfun

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top, 12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc, 19:20=szz
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

base=I2Mfig(int16(bise),1); 
if isempty(map);     map    =1;    end; mop=I2Mfig(int16(base),5); if mop, mop=map; end;
if isempty(uval);    uval   =[];   end;
if isempty(sens);    sens   =1;    end;
if isempty(drag);    drag   =0;    end;
if isempty(maxim);   maximum=100;  else maximum=maxim; if ischar(maximum), maximum=str2num(maximum); end; end;
if isempty(minim);   minimum=0;    else minimum=minim; if ischar(minimum), minimum=str2num(minimum); end; end;
if isempty(title);   title  =' ';  end;
if isempty(val);     valu=minimum; else valu=val;      if ischar(valu),    valu   =str2num(valu);    end; end;
ibas=int16(base);
fig=I2Mfig(ibas,3);
grp=I2Mfig(ibas,17);
bgc=I2Mfig(ibas,18);
szz=[0 0];
but=12;
[val,lx,ly]=widget_valset(0,val,but);

if scr_xs; xsz=scr_xs; elseif xsize; xsz=xsize; else, xsz=lx; end;
if scr_ys; ysz=scr_ys; elseif ysize; ysz=ysize; else, ysz=ly; end;
          pos=[0  0  xsz  ysz];
if vert,  pos=[0  0  min([xsz ysz]) max([xsz ysz])]; end;
smi=1/abs(maximum-minimum); if smi > 1, smi=1; end;
sma=smi*5;                  if sma > 1, sma=1; end;
step=[smi sma];
if minimum > maximum, spec=minimum; minimum=maximum; maximum=spec; spec=spec+minimum; valu=spec-valu; title=['.' title]; else, spec=[]; end;

wid=uicontrol(fig,'style','slider','position',pos,'string',title,'min',minimum,'max',maximum,'value',valu,'SliderStep',step,'userdata',uval); fii=double(int16(wid));

[pos,szz]=widget_font    (wid,font,pos,szz);
 bgc     =widget_resource(wid,bgc,resource_name,mop,sens);

I2Mfig(fii,1)   =wid; I2Mfig(fii, 2)=base; I2Mfig(fii, 3)=fig;
I2Mfig(fii,4)   =3;   I2Mfig(fii, 5)=map;  I2Mfig(fii, 6)=2;
I2Mfig(fii,7:10)=pos; I2Mfig(fii,15)=sens; I2Mfig(fii,16)=but;
I2Mfig(fii,11)  =I2Mfig(ibas,11);          I2Mfig(fii,17)=0;
I2Mfig(fii,12)  =I2Mfseq;                  I2Mfig(fii,18)=bgc;
I2Mfun{fii}=char(uname);                   I2Mfig(fii,19:20)=szz;

if ~isempty(not_r), I2Mnotr{fii}  =not_r; else, I2Mnotr{fii}  =''; end;
if ~isempty(f_get), I2Msget{fii,1}=f_get; else, I2Msget{fii,1}=''; end;
if ~isempty(p_set), I2Msget{fii,2}=p_set; else, I2Msget{fii,2}=''; end;

if     ~isempty(eventf); I2Mfevn{fii}=['___' eventf];
elseif ~isempty(eventp); I2Mfevn{fii}=eventp; else I2Mfevn{fii}=''; end;
ev=['struct(''structure_name'',''WIDGET_SLIDER'',''id'',' num2str(fii) ',''value'',v,''drag'',' num2str(drag) ')'];
set(wid,'Callback',['wid=' num2str(wid,20) '; v=get(wid,''value''); widget_i2mevent(' ev ');']);

I2Mfseq=I2Mfseq+1;

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
