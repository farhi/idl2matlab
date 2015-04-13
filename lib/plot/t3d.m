% t3d
% --------------------------------------------
% Equivalent to :
% function T3D  , Array | /RESET , 
%               , MATRIX=variable
%               , OBLIQUE=vector
%               , PERSPECTIVE=p{eye at (0,0,p)} 
%               , ROTATE=[x, y, z] 
%               , SCALE=[x, y, z]
%               , TRANSLATE=[x, y, z]
%               , /XYEXCH | /XZEXCH | /YZEXCH
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
% Fonction : T3D
%
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 22 / 04 / 2003
% Modifications : 13 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [varargout] = t3d(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('I2M_a1', 'translate', 'scale', 'rotate', 'reset', 'perspective', 'oblique', 'xyexch', 'xzexch', 'yzexch', 'matrix', 'I2M_pos');
  I2Mkwv={'matrixin', 'trans', 'scale', 'rota', 'reset', 'pers', 'oblique', 'xyexch', 'xzexch', 'yzexch', 'matrixout', 'I2M_pos'};
  matrixin=[]; trans=[]; scale=[]; rota=[]; reset=[]; pers=[]; oblique=[]; xyexch=[]; xzexch=[]; yzexch=[]; matrixout=[];
  exch=[];

   I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

  global i2mvs_p i2mvs_radeg % SYSTEM VARIABLES

   id=eye(4,4);

  %  Start from identity?
  array = i2m_interro(keyword_set(reset), id, i2m_interro((eval('n_elements(matrixin)','0') == 16), matrixin, i2mvs_p.t));
  if (eval('n_elements(trans)','0') ~= 0)
    %  Use given or current
    % Translate?
    ri = id;
    for i = 1:3,
      ri(4,i) = trans(i);
    end %for
    % Apply translation
    array = array * ri;
  end %if
  if (eval('n_elements(scale)','0') ~= 0)
    % Scale
    ri = id;
    for i = 1:3,
      ri(i ,i ) = scale(i );
    end %for
    % Apply scale
    array = array * ri;
  end %if
  if (eval('n_elements(rota)','0') ~= 0)
    % Rotate?
    % Use 3 by 3's
    ri = id((1):(3),(1):(3));
    r = ri;
    sx = sin(rota ./ i2mvs_radeg);
    cx = cos(rota ./ i2mvs_radeg);
    if (rota(1) ~= 0.0)
      % X Angle
      r(2,2) = cx(1);
      r(2,3) = sx(1);
      r(3,2) = -sx(1);
      r(3,3) = cx(1);
    end %if
    if (rota(2) ~= 0)
      % Y angle
      rr = ri;
      rr(1,1) = cx(2);
      rr(1,3) = -sx(2);
      rr(3,1) = sx(2);
      rr(3,3) = cx(2);
      r = r * rr;
    end %if
    if (rota(3) ~= 0)
      % Z angle
      rr = ri;
      rr(1,1) = cx(3);
      rr(1,2) = sx(3);
      rr(2,1) = -sx(3);
      rr(2,2) = cx(3);
      r = r * rr;
    end %if
    rr = dblarr(4,4);
    rr(1:3,1:3) = r;
    rr(4,4) = 1.0;
    % Apply cumulative rot transforms
    array = array * rr;
  end %if
  if (eval('n_elements(pers)','0') ~= 0)
    % Perspective?
    r = id;
    r(3,4) = -1. ./ pers;
    array = array * r;
  end %if
  if (eval('n_elements(oblique)','0') ~= 0)
    % Oblique projection?
    r = id;
    r(3,3) = 0.0;
    r(3,1) = oblique(1) .* cos(oblique(2) ./ i2mvs_radeg);
    r(3,2) = oblique(1) .* sin(oblique(2) ./ i2mvs_radeg);
    array = array * r;
  end %if
  if (keyword_set(xyexch))
    % Code to exchange axes.
    exch = d1_array(0,1);
  end %if
  if (keyword_set(xzexch))
    exch = d1_array(0,2);
  end %if
  if (keyword_set(yzexch))
    exch = d1_array(1,2);
  end %if
  if (eval('n_elements(exch)','0') ~= 0)
    % Exchange axes.
    t = array(exch(1) +1,:);
    array(exch(1) +1,1) = array(exch(2) +1,:);
    array(exch(2) +1,1) = t;
  end %if
  if ~isempty(matrixout)
    % Save final projection
    matrixout = array;
  else
    i2mvs_p.t = array;
  end %if


eval(I2M_out);
return;
% end of function t3d
