function [wid,varargout]=widget_base(varargin)
%FUNCTION widget_base, base, ...
%*** ****************
%***
%DEFAULT: Unit is pixel(all widgets)
%UNUSED : scroll, xsize,ysize, scr_xsize,scr_ysize, kbrd_focus, modal

    I2Mkwn=char('I2M_a1' , 'title' , 'row' , 'column' , 'notify_realize' , 'event_pro' , 'event_func' , 'func_get_value' , 'pro_set_value' , 'uvalue' , 'xsize' , 'ysize' , 'sensitive' , 'map' , 'frame' , 'exclusive' , 'nonexclusive' , 'kill_notify' , 'group_leader' , 'resource_name' , 'tlb_size_events' , 'tlb_kill_request_events' , 'uname' , 'mbar' , 'I2M_pos');
    I2Mkwv=    {'base'   , 'titl'  , 'row' , 'col'    , 'not_r'          , 'eventp'    , 'eventf'     , 'f_get'          , 'p_set'         , 'uval'   , 'xsize' , 'ysize' , 'sens'      , 'map' , 'frame' , 'exclusive' , 'nonexclusive' , 'kill_notify' , 'group_leader' , 'resource_name' , 'tlb_size_events' , 'tlb_kill'                , 'uname' , 'mbar' , 'I2M_pos'};
    base=[]; titl=[]; row=[]; col=[]; not_r=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; uval=[]; xsize=[]; ysize=[]; sens=[]; map=[]; frame=[]; exclusive=[]; nonexclusive=[]; kill_notify=[]; group_leader=[]; resource_name=[]; tlb_size_events=[]; tlb_kill=[]; uname=[] ; mbar=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig I2Mfevn I2Msget I2Mnotr I2Mfseq I2Mfgap I2Mfcar I2Mftim I2Mfun i2m_wcolor

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top, 12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc, 19:20=szz
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

but=0;
grp=0;
bgc=0;
if col; lay=1; elseif row; lay=2; else lay=3; end;
if ~isempty(exclusive),    but=1; end;
if ~isempty(nonexclusive), but=2; end;
if ~isempty(group_leader), grp=group_leader;  end;

if  isempty(map);  map =1;   end;
if  isempty(uval); uval=[];  end;
if  isempty(sens); sens=1;   end;
uic=1;
if isempty(base); if isempty(titl); titl=''; end;
    if isempty(I2Mfseq); I2Mfseq=1; I2Mfgap=4; I2Mfcar=[7 17]; I2Mftim=0; I2Mfun=cellstr(['','','']); end;
    fig=figure('name',titl,'visible','off','numbertitle','off','menubar','none','color',i2m_wcolor,'BusyAction','queue','Interruptible','off');
    uic=3; parent=fig;
else; parent=base; ibas=int16(parent); fig=I2Mfig(ibas,3); bgc=I2Mfig(ibas,18); end;

wid=uicontrol(fig,'style','frame','visible','off','userdata',uval); fii=double(int16(wid));

bgc=widget_resource(wid,bgc,resource_name,frame,sens);

if but, if lay==3, if uic==3, lay=2; else, lay=I2Mfig(int16(parent),4); if lay==3, lay=2; end; end; end;
else, but=10; end;

I2Mfig(fii,1)   =wid; I2Mfig(fii, 2)=parent; I2Mfig(fii, 3)=fig;
I2Mfig(fii,4)   =lay; I2Mfig(fii, 5)=map;    I2Mfig(fii, 6)=uic;
I2Mfig(fii,7:10)=0;   I2Mfig(fii,15)=sens;   I2Mfig(fii,16)=but;
I2Mfig(fii,17)  =grp; I2Mfig(fii,18)=bgc;    I2Mfig(fii,19:20)=0;
I2Mfun{fii}=char(uname);

if uic == 3; I2Mfig(fii,11)=fii;
else; I2Mfig(fii,11)=I2Mfig(int16(parent),11); end;
top=  I2Mfig(I2Mfig(fii,11),1);

I2Mfig(fii,12)  =I2Mfseq;

if ~isempty(not_r), I2Mnotr{fii}  =not_r; else, I2Mnotr{fii}  =''; end;
if ~isempty(f_get), I2Msget{fii,1}=f_get; else, I2Msget{fii,1}=''; end;
if ~isempty(p_set), I2Msget{fii,2}=p_set; else, I2Msget{fii,2}=''; end;

if     ~isempty(eventf); I2Mfevn{fii}=['___' eventf];
elseif ~isempty(eventp); I2Mfevn{fii}=eventp; else I2Mfevn{fii}='';end;

if ~isempty(tlb_kill), kill_notify=['___' tlb_kill]; end;
if ~isempty(kill_notify); widget_control('I2M_a1',wid,'kill_notify',kill_notify);
elseif uic == 3,          widget_control('I2M_a1',wid,'kill_notify','i2mrec'); end;

if ~isempty(tlb_size_events),
    ev =['ev=struct(''structure_name'',''WIDGET_BASE'',''id'',' num2str(wid,20) ',''top'',' num2str(top,20) ',''handler'',0,''x'',1,''y'',1);'];
    set(fig,'ResizeFcn',['pp=get(gcf,''position'');' ev 'ev.x=pp(3); ev.y=pp(4); widget_i2mevent(ev);']);
end;

I2Mfseq=I2Mfseq+1;

if ~isempty(mbar), but=4; uic=1; lay=2; parent=wid; ibas=int16(parent);
   mbar=uicontrol(fig,'style','frame','visible','off'); fii=double(int16(mbar));
   I2Mfig(fii,1)   =mbar;I2Mfig(fii, 2)=parent; I2Mfig(fii, 3)=fig;
   I2Mfig(fii,4)   =lay; I2Mfig(fii, 5)=map;    I2Mfig(fii, 6)=uic;
   I2Mfig(fii,7:10)=0;   I2Mfig(fii,15)=sens;   I2Mfig(fii,16)=but;
   I2Mfig(fii,17)  =0;   I2Mfig(fii,18)=bgc;

   I2Mfig(fii,11)  =I2Mfig(int16(parent),11);
   I2Mfig(fii,12)  =I2Mfseq; I2Mfseq=I2Mfseq+1;
end;

set(fig,'CloseRequestFcn','my_delete');


if I2M_out, eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
