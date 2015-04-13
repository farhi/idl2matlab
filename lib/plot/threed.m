% threed
% --------------------------------------------
% Equivalent to :
% function THREED   , A [, Sp] 
%                   , TITLE=string
%                   , XTITLE=string
%                   , YTITLE=string
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : THREED
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 24 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [varargout] = threed(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('I2M_a1', 'I2M_a2', 'title', 'xtitle', 'ytitle', 'I2M_pos');
  I2Mkwv={'a', 'sp', 'title', 'xtitle', 'ytitle', 'I2M_pos'};
  a=[]; sp=''; title=[]; xtitle=[]; ytitle=[];

     I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

  %global i2mvs_c % SYSTEM VARIABLES

  idem = eval('sizz(a)','[0 0 1]');
  m = idem(2);
  n = idem(3);
  
  if isempty(sp)
    % FAKE SPACING
    sp = (maax(a) - miin(a)) ./ float(n);
  end %if
  [x, m] = indgen('I2M_a1', m, 'I2M_pos', [1]);
  % ARRAY + INCREMENT
  for i=1:m
      for j=1:n
        inter(j,i)=j-1;
      end;
  end;
 
  add = a + inter .* abs(sp);
  [ymax, add] = maax('I2M_a1', add, 'I2M_pos', [1]);
  [ymin, add] = miin('I2M_a1', add, 'I2M_pos', [1]);
 
  % DISABLE CURSOR
  %i2mvs_c = 0;
 
  %  DETERMINE IF HIDDEN LINES SHOULD BE PLOTTED
  
  oldy = add(1,:);
  if (eval('n_elements(title)','0') == 0)
    % PLOT THE SCALE AND THE FIRST LINE
    title = '';
  end %if
  if (eval('n_elements(xtitle)','0') == 0)
    xtitle = '';
  end %if
  if (eval('n_elements(ytitle)','0') == 0)
    ytitle = '';
  end %if
  [x, oldy, title, xtitle, ytitle] = plott('I2M_a1', x, 'I2M_a2', oldy, 'xrange', d1_array(0.,m), 'yrange', d1_array(ymin,ymax), 'title', title, 'xtitle', xtitle, 'ytitle', ytitle, 'I2M_pos', [1, 2, 5, 6, 7]);
  for i = 1:n - 1,
    % 
    % LOOP TO PLOT EACH LINE
    % GET ROW
    y = add(i+1,:);
    if (sp >= 0.)
      %  REMOVE HIDDEN LINES
      y = max(y, oldy);
        
    end %if
    % PLOT IT
    [x, y] = oplott('I2M_a1', x, 'I2M_a2', y, 'I2M_pos', [1, 2]);
    oldy = y;
  end %for
  eval(I2M_out); return;

% TO UPDATE THE VALUE OF ![XYZ].CRANGE
majCrange;

eval(I2M_out);
return;
% end of function threed
