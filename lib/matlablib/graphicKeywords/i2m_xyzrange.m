% i2m_xyzrange.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]RANGE = ...
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
% Fonction : fonction i2m_xyzrange
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 10 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyzrange(xrange,yrange,zrange)

if ~isempty(xrange)
	if xrange == 0
		set(gca,'XLimMode','auto');
	else
		set(gca,'XLim',xrange);
	end
end

if ~isempty(yrange)
	if yrange == 0
		set(gca,'YLimMode','auto');
	else
		set(gca,'YLim',yrange);
	end
end

if ~isempty(zrange)
	if zrange == 0
		set(gca,'ZLimMode','auto');
	else
		set(gca,'ZLim',zrange);
	end
end
