%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : constructeur varsysP pour gerer la variable
%            systeme IDL !P
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 01 / 04 / 2003
% Modifications : 06 / 05 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function p = varsysP(varargin)
% constructor

switch nargin
case 0
% cree un objet par defaut
	%%%%%%%%%%% Champs traduits %%%%%%%%%%%%%%%%%%%%%%%%%%%
	p.linestyle = 0;
		p.linestylem = '-';  % valeur Matlab equivalente au !P.LINESTYLE actuel d'IDL
	p.psym = 0;
		p.psymm = 'none'; % valeur Matlab equivalente au !P.PSYM actuel d'IDL
		set(0,'DefaultAxesLineStyle','-');
		set(0,'DefaultSurfaceLineStyle','-');

	set(0,'DefaultAxesGridLineStyle','-');
	p.thick = 0;
		set(0,'DefaultLineLineWidth',1.0);
		set(0,'DefaultSurfaceLineWidth',1.0);
	p.ticklen = 0.02;
		set(0,'DefaultAxesTickDirMode','manual');
		set(0,'DefaultAxesTickDir','in');
		set(0,'DefaultAxesTickLength',[0.02,0.02]);
	p.noerase = 0;
		set(0,'DefaultAxesNextPlot','replace');
		set(0,'DefaultFigureNextPlot','add');
	p.multi = [0 0 0 0 0];

	p.position = [0 0 0 0];
	p.region = [0 0 0 0];
	set(0,'DefaultAxesPosition','default');

	p.charthick = 0;
		set(0,'DefaultAxesFontWeight','default');
	p.charsize = 0;
		set(0,'DefaultAxesFontUnits','points');
		set(0,'DefaultAxesFontSize','default');
		set(0,'DefaultTextFontSize','default');
	p.title = '';

	p.background = 0;
	p.color = 16777215;
	set(0,'DefaultFigureColor',[0 0 0]);     % Pour mettre l'arriere-plan de la figure a noir comme dans IDL
	set(0,'DefaultAxesColor',[0 0 0]);       % Pour mettre l'arriere-plan des "plots" a noir par defaut comme dans IDL
	set(0,'DefaultAxesColorOrder',[1 1 1]);  % Pour que toutes les courbes soient affichees en blanc par defaut
	set(0,'DefaultAxesXColor',[1 1 1]);      % Pour que l'axe des X soit blanc par defaut
	set(0,'DefaultAxesYColor',[1 1 1]);      % Pour que l'axe des Y soit blanc par defaut
	set(0,'DefaultAxesZColor',[1 1 1]);      % Pour que l'axe des Z soit blanc par defaut
	set(0,'DefaultTextColor',[1 1 1]);       % Pour que le texte soit blanc par defaut
	set(0,'DefaultLineColor',[1 1 1]);       % pour que les lignes soient blanches par defaut
	set(0,'DefaultSurfaceEdgeColor',[1 1 1]);% pour que la couleur des segments dans le trace de surface soit blanc par defaut

	p.multioplot = 0; % pour aider a gerer les overplots dans les plots multiples


	%%%%%%%%%% Champs non encore traduits %%%%%%%%%%%%%%%%%

	p.clip = [0 0 639 511 0 0];  % N'a pas d'equivalent en Matlab
	p.font = 1;
	p.noclip = 0;
	p.nsum = 0;
	p.subtitle = '';
	p.symsize = 0;
	p.t = zeros(4);
	p.t3d = 0;

	p.channel = 0;

	%%%%%%%%%% Creation de l'objet %%%%%%%%%%%%%%%%%%%%%%%%
	p = class(p,'varsysP');
otherwise
	error('Wrong number of input arguments')
end
