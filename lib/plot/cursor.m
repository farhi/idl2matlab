function [X,Y,varargout]=cursor(varargin)
%Pro cursor,x,y, ...
%*** **********
%***

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'nowait' , 'wait' , 'data' , 'device' , 'I2M_pos' );
    I2Mkwv=    {'X'      , 'Y'      , 'wt'     , 'swt'    , 'wet'  , 'data' , 'dev'    , 'I2M_pos' };
    X=[]; Y=[]; wt=[]; swt=[]; wet=[]; data=[]; dev=[];

     I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;


if isempty(wt), wwt=1; else, wwt=wt; end;
if wet        , wwt=1; end;
if swt        , wwt=0; end;

if wwt, k=waitforbuttonpress; end;

if isempty(data) & isempty(dev);  dev=1; end;

if data; M=get(gca,'CurrentPoint'); X=M(1,1); Y=M(1,2); end;

if dev;  spos=get(0,'PointerLocation'); fpos=get(gcf,'position'); apos=get(gca,'position');
         X=spos(1)-fpos(1)-apos(1);
         Y=spos(2)-fpos(2)-apos(2);
         if X<0 | Y<0, X=-1; Y=-1 ; end;
end;

if I2M_out, eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
