%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : delete_tv
%            Fonction de callback. Permet de replacer les axes au bon endroit si l'on est dans un widget.
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 27 / 05 / 2003
% Modifications : 02 / 06 / 2003       
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

global axes_wd I2Mfig I2Mfcar

gca=get(gcbo,'Parent');
if ishold == 0
    clear gca;
    if ~isempty(axes_wd) & sum(axes_wd(1,:)==gca)~=0
        fii=double(int16(gca));
        ppa=I2Mfig(fii,7:10); 
        [i,j]=find(axes_wd(1,:) == gca);
        if i==1
             ppa=ppa+[I2Mfcar(1)*8,I2Mfcar(2)*3,-I2Mfcar(1)*10,-I2Mfcar(2)*5]; 
        end;
        set(gca,'position',ppa);
        %set(gca,'Units','normalized')
    end;
else
end;
