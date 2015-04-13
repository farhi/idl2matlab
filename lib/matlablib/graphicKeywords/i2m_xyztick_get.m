% i2m_xyztick_get.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICK_GET = ...
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
% Fonction : fonction i2m_xyztick_get
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 17 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [xtick,ytick,ztick] = i2m_xyztick_get(xvalues,yvalues,zvalues)

if ~isempty(xvalues)
	xtick = get(gca,'xtick');
else
	xtick = [];
end

if ~isempty(yvalues)
	ytick = get(gca,'ytick');
else
	ytick = [];
end

if ~isempty(zvalues)
	ztick = get(gca,'ztick');
else
	ztick = [];
end

