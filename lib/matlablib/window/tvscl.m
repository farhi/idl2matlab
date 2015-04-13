% tvscl
% --------------------------------------------
% Equivalent to :
% function  TVSCL, Image [, Position]
%    ou
%           TVSCL, Image [, X, Y [, Channel]]
%                   /CENTIMETERS
%                   /CHANNEL
%                   /INCHES
%                   /ORDER
%                   /TRUE  -
%                   /WORDS -
%                   /XSIZE -
%                   /YSIZE -
%                   /NAN    -
%                   /TOP    -
%                   
% in IDL

function [varargout]=tvscl(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4'   ,'centimeters' ,'channel','inches','order','true','words','xsize','ysize','t3d'  ,'data','normal','device','z','nan' ,'top' ,'I2M_pos');
I2Mkwv=    {'a1'     , 'd2'     , 'd3'     , 'chanArg'  ,'cent'        ,'chanKG' ,'inches','order','truK','words','sizex','sizey','t3dKG','data','normal','device','z','nanK','topK','I2M_pos'};

a1=[]; d2=[]; d3=[]; chanArg=[]; cent=[]; chanKG=[]; inches=[]; order=[]; truK=[]; words=[]; sizex=[]; sizey=[]; t3dKG=[]; data=[]; normal=[]; device=[]; z=[]; nanK=[]; topK=[];

    I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

tv('I2M_a1',a1,'I2M_a2',d2,'I2M_a3',d3,'I2M_a4',chanArg,'centimeters',cent,'channel',chanKG, ...
  'inches',inches,'order',order,'true',truK,'words',words,'xsize',sizex,'ysize',sizey,...
  't3d',t3dKG,'data',data,'normal',normal,'device',device,'z',z,'nan',nanK,'top',topK,'scale', 1);

if I2M_out; eval(I2M_out); end;
