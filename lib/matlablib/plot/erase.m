% erase
% --------------------------------------------
% Equivalent to :
% function ERASE [, Background_Color]
%                [, COLOR=value]      CHANNEL not available
% in IDL

function [varargout]=erase(varargin)

I2Mkwn=char('I2M_a1' , 'channel' , 'color' , 'I2M_pos');
I2Mkwv=    {'d1'     , 'channell', 'colorr', 'I2M_pos'};
    
d1=[]; colorr=[]; channell=[];

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global i2mDecomposed i2mvs_p

if isempty(d1), d1=colorr; end;
if isempty(d1), d1=i2mvs_p.background; end;


wi0=widget_gca(gca);   isTV=0;
if wi0 <0, tv_gca=gca; pl_gca=abs(wi0); isTV=1; end %TV   axis
if wi0 >0, tv_gca=wi0; pl_gca=gca;      isTV=0; end %PLOT axis

wset(tv_gca); cla reset; set(tv_gca,'xtick',[],'ytick',[],'ztick',[]); box off;
wset(pl_gca); cla reset; set(pl_gca,'xtick',[],'ytick',[],'ztick',[]); box off; %Now gca is PLOT

c=i2mColormap(d1);

set(tv_gca,'Color',c,'XColor',c,'YColor',c,'ZColor',c);
set(pl_gca,'Color',c,'XColor',c,'YColor',c,'ZColor',c);

if I2M_out; eval(I2M_out); end;
