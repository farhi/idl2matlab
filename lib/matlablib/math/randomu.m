function [res,varargout]=randomu(varargin)
%Function randomu
%******** *******
%**

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'I2M_a5' , 'I2M_a6' , 'I2M_a7' , 'I2M_a8' , 'I2M_a9' , 'binomial' , 'gamma' , 'normal' , 'poisson' , 'uniform' , 'long' , 'double' , 'I2M_pos');
    I2Mkwv=    {'seed'   , 'd1'     , 'd2'     , 'd3'     , 'd4'     , 'd5'     , 'd6'     , 'd7'     , 'd8'     , 'binomial' , 'gamma' , 'normal' , 'poisson' , 'uniform' , 'long' , 'double' , 'I2M_pos'};
    seed=[]; d1=[]; d2=[]; d3=[]; d4=[]; d5=[]; d6=[]; d7=[]; d8=[]; binomial=[]; gamma=[]; normal=[]; poisson=[]; uniform=[]; long=[]; double=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

sz=d1; if isempty(d2), sz=[d1 1]; end;
if ~isempty(d2), sz=[sz d2];
if ~isempty(d3), sz=[sz d3];
if ~isempty(d4), sz=[sz d4];
if ~isempty(d5), sz=[sz d5];
if ~isempty(d6), sz=[sz d6];
if ~isempty(d7), sz=[sz d7];
if ~isempty(d8), sz=[sz d8];
end; end; end; end; end; end; end;

if  isempty(seed), rand('state',0); elseif length(seed) == 1, rand('state',seed); else rand('state',seed); end;

res =rand(sz);
seed=rand('state');

if ~isempty(I2M_out), eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
