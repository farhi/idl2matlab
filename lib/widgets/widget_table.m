function [wid,varargout]=widget_table(varargin)
%FUCTION widget_table, base, ...
%*** ****************
%***
%UNUSED : all_events, kbrd_focus, scroll, xsize,ysize
%TODO   : column_labels, row_labels, (select in control)

    I2Mkwn=char('I2M_a1' , 'notify_realize' , 'column_widths' , 'row_heights' , 'column_labels' , 'row_labels' , 'column_major' , 'row_major' , 'frame' , 'no_headers' , 'event_pro' , 'event_func' , 'func_get_value' , 'pro_set_value' , 'map' , 'value' , 'uvalue' , 'font' , 'xsize' , 'ysize' , 'scr_xsize' , 'scr_ysize' , 'sensitive' , 'editable' , 'uname' , 'resource_name' , 'I2M_pos' );
    I2Mkwv=    {'base'   , 'not_r'          , 'col_sx'        , 'row_sy'      , 'col_lab'       , 'row_lab'    , 'col_maj'      , 'row_maj'   , 'frame' , 'no_headers' , 'eventp'    , 'eventf'     , 'f_get'          , 'p_set'         , 'map' , 'vol'   , 'uval'   , 'font' , 'xsize' , 'ysize' , 'scr_xs'    , 'scr_ys'    , 'sens'      , 'edi'      , 'uname' , 'resource_name' , 'I2M_pos' };
    base=[]; not_r=[]; col_sx=[]; row_sy=[]; col_lab=[]; row_lab=[]; col_maj=[]; row_maj=[]; frame=[]; no_headers=[]; eventp=[]; eventf=[]; f_get=[]; p_set=[]; map=[]; vol=[]; uval=[]; font=[]; xsize=[]; ysize=[]; scr_xs=[]; scr_ys=[]; sens=[]; edi=[]; uname=[]; resource_name=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig I2Mfcar
but=19;

if isnumeric(vol), vol=strung(vol); nume=1; else, nume=0; end;
if isa(vol,'i2mstr'), val=vol(:); elseif ischar(vol),   val={vol}; elseif iscell(vol), val=vol;
				  elseif isstruct(vol), val= vol;  else,  val={'Empty'};   end;
if isempty(col_maj) , row=1; else,  row=0;  end;
if isempty(no_headers),      no_headers=0;  end;

%Dims
%****
s=size(val);
if iscell(val),	nx=s(2);        ny=s(1);
else,           nx=n_tags(val); ny=s(2); if ~row, tm=nx; nx=ny; ny=tm; end; end;
lx=zeros(1,nx);
ly=zeros(1,ny)+1;

%size
%****
if iscell(val),	for i=1:nx, for j=1:ny, lx(i)=max(lx(i),length(val{j,i})); end; end;
else, names=fieldnames(val(1));
                for i=1:nx, for j=1:ny, if row, lx(i)=max(lx(i),length(strung(getfields(val(j),names{i}))));
                                        else,   lx(i)=max(lx(i),length(strung(getfields(val(i),names{j})))); end; end; end; end;
fct=widget_font(0,font,0);
lx=round((lx+2)*I2Mfcar(1) *fct);
ly=round( ly   *I2Mfcar(2) *fct);

if ~isempty(col_sx), lx(:)=col_sx; end;
if ~isempty(row_sy), ly(:)=row_sy; end;

%labels
%******
if ~no_headers; end;

%base
%****
wit=widget_base('I2M_a1',base);
wid=widget_base('I2M_a1',wit,'column',1,'notify_realize',not_r,'frame',frame,'event_pro',eventp,'event_func',eventf,'func_get_value',f_get,'pro_set_value',p_set,'map',map,'uvalue',uval,'sensitive',sens,'uname',uname);
I2Mfig(int16(wid),16)=but;
wtx=[];

%texts
%*****
for j=1:ny,
    wic=widget_base('I2M_a1',wid,'row',1);
    for i=1:nx,
      if j==1 & ~no_headers, if i==1, end; end;
      if iscell(val), value=val{j,i};
      elseif    row , value=strung(getfields(val(j),names{i}));
      else          , value=strung(getfields(val(i),names{j})); end;
      wtx(i,j)=widget_text('I2M_a1',wic,'value',value,'uvalue',[19 i-1 j-1 nume row no_headers],'font',font,'editable',edi,'resource_name',resource_name,'scr_xsize',lx(i),'scr_ysize',ly(i));
    end;
end;
widget_control('I2M_a1',wit,'set_uvalue',wtx);

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
