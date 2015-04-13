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
%            pour gerer la variable systeme !X
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 03 / 04 / 2003
% Modifications : 23 / 07 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function x = varsysX(varargin)
% constructor

switch nargin
case 0
% cree un objet par defaut
	%%%%%%%%%%%%%%%% CHAMPS TRADUITS %%%%%%%%%%%%%%%%%%%%%%
	x.tickname = [];
		set(0,'DefaultAxesXTickLabelMode','auto');
	x.title = '';
	x.s = [0 1];
	x.range = [0 0];
		set(0,'DefaultAxesXLimMode','auto');
	x.type = 0;
		set(0,'DefaultAxesXScale','linear');
	x.minor = 0;
		set(0,'DefaultAxesXMinorTick','on');
	x.crange = [0 0];

	x.charsize = 0;
	x.ticklen = 0;
	x.tickv = zeros(1,60);
		set(0,'DefaultAxesXTickMode','auto');
	x.gridstyle = 0;

	x.style = 0;
		% champs n'existant pas en IDL mais qui vont nous etre utiles pour simplifier
		% le traitement du champ STYLE
		x.exact = 0;
		x.extend = 0;
		x.noAxis = 0;
		x.noBox = 0;
		x.ynozero = 0;
	%%%%%%%%%%%%% CHAMPS NON ENCORE TRADUITS %%%%%%%%%%%%%%
	x.ticks = 0;
	x.thick = 0;
	x.margin = [10 3];
	x.omargin = [0 0];
	x.window = [0 0];
	x.region = [0 0];
	x.tickformat = '';
	x.tickinterval = 0;
	x.ticklayout = 0;
	x.tickunits = strarr(10); % ??

	%%%%%%%%%%%%%%% CREATION DE L'OBJET %%%%%%%%%%%%%%%%%%%
	x = class(x,'varsysX');
otherwise
	error('Wrong number of input arguments')
end
