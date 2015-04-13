%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : constructeur varsysY
%            pour gerer la variable systeme !Y
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 03 / 04 / 2003
% Modifications : 10 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function y = varsysY(varargin)
% constructor

switch nargin
case 0
% cree un objet par defaut
	%%%%%%%%%%%%%%%% CHAMPS TRADUITS %%%%%%%%%%%%%%%%%%%%%%
	y.tickname = [];
		set(0,'DefaultAxesYTickLabelMode','auto');
	y.title = '';
	y.s = [0 1];
	y.range = [0 0];
		set(0,'DefaultAxesYLimMode','auto');
	y.type = 0;
		set(0,'DefaultAxesYScale','linear');
	y.minor = 0;
		set(0,'DefaultAxesYMinorTick','on');
	y.crange = [0 0];
	
	y.charsize = 0;
	y.ticklen = 0;
	y.tickv = zeros(1,60);
	%%%%%%%%%%%%% CHAMPS NON ENCORE TRADUITS %%%%%%%%%%%%%%
	y.style = 0;
	y.ticks = 0;
	y.thick = 0;
	y.margin = [10 3];
	y.omargin = [0 0];
	y.window = [0 0];
	y.region = [0 0];
	y.gridstyle = 0;
	y.tickformat = '';
	y.tickinterval = 0;
	y.ticklayout = 0;
	y.tickunits = strarr(10); % ??

	%%%%%%%%%%%%%%% CREATION DE L'OBJET %%%%%%%%%%%%%%%%%%%
	y = class(y,'varsysY');
otherwise
	error('Wrong number of input arguments')
end
