function [wid,varargout]=widget_text(varargin)
%FUCTION widget_text, base, ...
%*** ***************
%***
%UNUSED : all_events, kbrd_focus

    I2Mkwn=char('I2M_a1' , 'notify_realize' , 'event_pro' , 'event_func' , 'func_get_value' , 'pro_set_value' , 'map' , 'value' , 'uvalue' , 'font' , 'xsize' , 'ysize' , 'scr_xsize' , 'scr_ysize' , 'sensitive' , 'editable' , 'uname' , 'resource_name' , 'I2M_pos' );
    I2Mkwv=    {'bise'   , 'not_r'          , 'eventp'    , 'eventf'     , 'f_get'          , 'p_set'         , 'map' , 'vol'   , 'uval'   , 'font' , 'xsize' , 'ysize' , 'scr_xs'    , 'scr_ys'    , 'sens'      , 'edi'      , 'uname' , 'resource_name' , 'I2M_pos' };
    bise=[]; not_r=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; map=[]; vol=[]; uval=[]; font=[]; xsize=[]; ysize=[]; scr_xs=[]; scr_ys=[]; sens=[]; edi=[]; uname=[]; resource_name=[]; I2M_pos=[];

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
if isempty(map);  map=1;    end; mop=I2Mfig(int16(base),5); if mop, mop=map; end;
if isempty(edi);  edi=0;    end;
if isempty(uval); uval=[];  end;
if isempty(vol);   val=' '; else val=vol; end;
if isempty(sens); sens=1;   end;
ibas=int16(base);
fig=I2Mfig(ibas,3);
grp=I2Mfig(ibas,17);
bgc=I2Mfig(ibas,18);
szz=[1 1];
but=13;
[val,lx,ly]=widget_valset(0,val,but);

if scr_xs; xsz=scr_xs; xzz(1)=0; elseif xsize,  xsz=(xsize+2)*I2Mfcar(1); else, xsz=lx; end;
if scr_ys; ysz=scr_ys; xzz(2)=0; elseif ysize,  ysz=(ysize  )*I2Mfcar(2); else, ysz=ly; end;
pos=[0  0  xsz  ysz];

wid=uicontrol(fig,'style','edit','min',0,'max',ly/I2Mfcar(2),'position',pos,'string',val,'HorizontalAlignment','left','userdata',uval); fii=double(int16(wid));

[pos,szz]=widget_font    (wid,font,pos,szz);
 bgc     =widget_resource(wid,bgc,resource_name,mop,sens);

if xsize & isempty(scr_xs), pos(3)=xsz*szz(1); szz(1)=0; set(wid,'position',pos); end;
if ysize & isempty(scr_ys), pos(4)=ysz*szz(2); szz(2)=0; set(wid,'position',pos); end;

if ~edi, set(wid,'Enable','inactive'); end;

I2Mfig(fii,1)   =wid; I2Mfig(fii, 2)=base; I2Mfig(fii, 3)=fig;
I2Mfig(fii,4)   =3;   I2Mfig(fii, 5)=map;  I2Mfig(fii, 6)=2;
I2Mfig(fii,7:10)=pos; I2Mfig(fii,15)=sens; I2Mfig(fii,16)=but;
I2Mfig(fii,11)  =I2Mfig(ibas,11);          I2Mfig(fii,17)=grp;
I2Mfig(fii,12)  =I2Mfseq;                  I2Mfig(fii,18)=bgc;
I2Mfun{fii}=char(uname);                   I2Mfig(fii,19:20)=szz;

if ~isempty(not_r), I2Mnotr{fii}  =not_r; else, I2Mnotr{fii}  =''; end;
if ~isempty(f_get), I2Msget{fii,1}=f_get; else, I2Msget{fii,1}=''; end;
if ~isempty(p_set), I2Msget{fii,2}=p_set; else, I2Msget{fii,2}=''; end;

if     ~isempty(eventf); I2Mfevn{fii}=['___' eventf];
elseif ~isempty(eventp); I2Mfevn{fii}=eventp; else I2Mfevn{fii}=''; end;
ev=['struct(''structure_name'',''WIDGET_TEXT_STR'',''id'',' num2str(fii) ',''type'',1,''offset'',0,''str'','' '')'];
set(wid,'Callback',['widget_i2mevent(' ev ');']);

I2Mfseq=I2Mfseq+1;

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
