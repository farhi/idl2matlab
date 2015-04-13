%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : my_delete
%           Fonction de callback permettant de retirer les handles 
%           des axes appartenant au widget que l'on vient de fermer.
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 24 / 05 / 2003
% Modifications : 24 / 05 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

global axes_wd

h=findobj(gcf,'Type','axes');
for i=1:length(h)
    [i,j]=find(axes_wd(1,:) == h(i));
    axes_wd(:,j)=[];
end;

delete(gcf);