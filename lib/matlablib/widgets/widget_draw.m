function [wid,varargout]=widget_draw(varargin)
%FUNCTION widget_draw, base, ...
%**** ***************
%***
%UNUSED : scroll
%TO DO:
%colors, frame, graphics_level, motion_events(figure)

    I2Mkwn=char('I2M_a1' , 'notify_realize' , 'event_pro' , 'event_func' , 'func_get_value' , 'pro_set_value' , 'map' , 'uvalue' , 'xsize' , 'ysize' , 'scr_xsize' , 'scr_ysize' , 'x_scroll_size' , 'y_scroll_size' , 'sensitive' , 'uname' , 'button_events' , 'I2M_pos' );
    I2Mkwv=    {'bise'   , 'not_r'          , 'eventp'    , 'eventf'     , 'f_get'          , 'p_set'         , 'map' , 'uval'   , 'xsize' , 'ysize' , 'src_xs'    , 'scr_ys'    , 'x_scr'         , 'y_scr'         , 'sens'      , 'uname' , 'but_ev'        , 'I2M_pos' };
    bise=[]; not_r=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; map=[]; uval=[]; xsize=[]; ysize=[]; scr_xs=[]; scr_ys=[]; x_scr=[]; y_scr=[]; sens=[]; uname=[]; but_ev=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig I2Mfevn I2Mnotr I2Msget I2Mfseq I2Mfcar I2Mfun

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top, 12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc, 19:20=szz 21=wi0
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

base=I2Mfig(int16(bise),1); 
if isempty(map);  map=1;    end;  mop=I2Mfig(int16(base),5); if mop, mop=map; end;
if isempty(uval); uval=[];  end;
if isempty(sens); sens=1;   end;
ibas=int16(base);
fig=I2Mfig(ibas,3);
but=14;


if x_scr, xsz=x_scr; elseif scr_xs; xsz=scr_xs; elseif xsize; xsz=xsize; else, xsz=50; end;
if y_scr, ysz=y_scr; elseif scr_ys; ysz=scr_ys; elseif ysize; ysz=ysize; else, ysz=50; end;
pos=[0  0  xsz  ysz];

wwc=uicontrol(fig,'style','frame','visible','off','userdata',uval,'backgroundColor','red'); fii=double(int16(wwc));
I2Mfig(fii,1)   =wwc; I2Mfig(fii, 2)=base;   I2Mfig(fii,3) =fig;
I2Mfig(fii,4)   =3;   I2Mfig(fii, 5)=1;      I2Mfig(fii,6) =1;
I2Mfig(fii,7:10)=pos; I2Mfig(fii,15)=1;      I2Mfig(fii,16)=but;
I2Mfig(fii,11)  =I2Mfig(ibas,11);            I2Mfig(fii,17)=0;
I2Mfig(fii,12)  =I2Mfseq;                    I2Mfig(fii,18)=0;
I2Mfun{fii}='';
I2Mfseq=I2Mfseq+1;

%
wi0=axes('Parent',fig,'units','pixels','position',pos,'FontSize',8,'FontWeight','bold','NextPlot','add','HitTest','off','userdata',uval,'color','none','xtick',[],'ytick',[],'box','off','xlim',[1 xsz],'ylim',[1 ysz],'DataAspectRatioMode','manual');
   %tco=get(wi0,'color'); set(wi0,'xcolor',tco,'ycolor',tco);
fi0=double(int16(wi0));
if ~mop;  set(wi0,'visible','off'); end;
if ~sens; set(wi0,'enable' ,'off'); end;
I2Mfig(fi0,1)   =wi0; I2Mfig(fi0, 2)=wwc;  I2Mfig(fi0, 3)=fig;
I2Mfig(fi0,4)   =3;   I2Mfig(fi0, 5)=map;  I2Mfig(fi0, 6)=2;
I2Mfig(fi0,7:10)=pos; I2Mfig(fi0,15)=sens; I2Mfig(fi0,16)=but;
I2Mfig(fi0,11)  =I2Mfig(ibas,11);          I2Mfig(fi0,17)=0;
I2Mfig(fi0,12)  =I2Mfseq;                  I2Mfig(fi0,18)=0;
I2Mfun{fi0}='';                            I2Mfig(fi0,19:20)=0; I2Mfig(fi0,21)=0;
I2Mfseq=I2Mfseq+1;

wid=axes('Parent',fig,'units','pixels','position',pos,'FontSize',8,'FontWeight','bold','NextPlot','replace','HitTest','off','userdata',uval,'color','none','xtick',[],'ytick',[]);
   %tco=get(wid,'color'); set(wid,'xcolor',tco,'ycolor',tco);
    axis off;
fii=double(int16(wid));
if ~mop;  set(wid,'visible','off'); end;
if ~sens; set(wid,'enable' ,'off'); end;
I2Mfig(fii,1)   =wid; I2Mfig(fii, 2)=wwc;  I2Mfig(fii, 3)=fig;
I2Mfig(fii,4)   =3;   I2Mfig(fii, 5)=map;  I2Mfig(fii, 6)=2;
I2Mfig(fii,7:10)=pos; I2Mfig(fii,15)=sens; I2Mfig(fii,16)=but;
I2Mfig(fii,11)  =I2Mfig(ibas,11);          I2Mfig(fii,17)=0;
I2Mfig(fii,12)  =I2Mfseq;                  I2Mfig(fii,18)=0;
I2Mfun{fii}=char(uname);                   I2Mfig(fii,19:20)=0; I2Mfig(fii,21)=wi0; I2Mfig(fi0,21)=-wid;

if ~isempty(not_r), I2Mnotr{fii}  =not_r; else, I2Mnotr{fii}  =''; end;
if ~isempty(f_get), I2Msget{fii,1}=f_get; else, I2Msget{fii,1}=''; end;
if ~isempty(p_set), I2Msget{fii,2}=p_set; else, I2Msget{fii,2}=''; end;

if     ~isempty(eventf); I2Mfevn{fii}=['___' eventf];
elseif ~isempty(eventp); I2Mfevn{fii}=eventp; else I2Mfevn{fii}=''; end;
if ~isempty(but_ev); if but_ev, widget_control('I2M_a1',wid,'draw_button_events',1); end; end;

I2Mfseq=I2Mfseq+1;

wset(wid);

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
