%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : constructeur varsysX
%            pour gerer la variable systeme !Z
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 09 / 04 / 2003
% Modifications : 10 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function z = varsysZ(varargin)
% constructor

switch nargin
case 0
% cree un objet par defaut
	%%%%%%%%%%%%%%%% CHAMPS TRADUITS %%%%%%%%%%%%%%%%%%%%%%
	z.tickname = [];
		set(0,'DefaultAxesZTickLabelMode','auto');
	z.title = '';
	z.s = [0 1];
	z.range = [0 0];
		set(0,'DefaultAxesZLimMode','auto');
	z.type = 0;
		set(0,'DefaultAxesZScale','linear');
		
	z.minor = 0;
		set(0,'DefaultAxesZMinorTick','on');

	z.charsize = 0;
	z.ticklen = 0;
	z.tickv = zeros(1,60);
	%%%%%%%%%%%%% CHAMPS NON ENCORE TRADUITS %%%%%%%%%%%%%%
	z.style = 0;
	z.ticks = 0;
	z.thick = 0;
	z.crange = [0 0];
	z.margin = [10 3];
	z.omargin = [0 0];
	z.window = [0 0];
	z.region = [0 0];
	z.gridstyle = 0;
	z.tickformat = '';
	z.tickinterval = 0;
	z.ticklayout = 0;
	z.tickunits = strarr(10); % ??

	%%%%%%%%%%%%%%% CREATION DE L'OBJET %%%%%%%%%%%%%%%%%%%
	z = class(z,'varsysZ');
otherwise
	error('Wrong number of input arguments')
end
