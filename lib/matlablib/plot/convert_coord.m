% convert_coord
% --------------------------------------------
% Equivalent to :
% function Result = CONVERT_COORD 
%                               (
%                               X [, Y [, Z]]
%                               , /DATA | /DEVICE | /NORMAL
%                               , /DOUBLE
%                               , /TO_DATA | /TO_DEVICE | /TO_NORMAL
%                               )
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
% Fonction : CONVERT_COORD
%
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 25 / 04 / 2003
% Modifications : 30 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% A faire:
% /T3D

% /DOUBLE => On ne fait rien

% res = le vecteur resultat
% 1ere ligne = coordonnees X
% 2eme ligne = coordonnees YX
% 3eme ligne = coordonnees Z

function [res,varargout] = convert_coord(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'data' , 'device' , 'normal' , 'double' , 't3d' , 'to_data' , 'to_device' , 'to_normal' , 'I2M_pos');
I2Mkwv=    {'xinit'  , 'yinit'  , 'zinit'  , 'dataa', 'devicee', 'normall', 'doublee', 't3dd', 'to_dataa', 'to_devicee', 'to_normall', 'I2M_pos'};

% variables
x=[];y=[];z=[];
xinit=[];yinit=[];zinit=[];
% keywords présent si =1
dataa='';devicee='';normall='';doublee='';
t3dd='';to_dataa='';to_devicee='';to_normall='';

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% On travaille sur des copies de xinit, yinit et zinit
x=xinit;
y=yinit;
z=zinit;

% Mise a jour de !X.S, !Y.S et !Z.S a chaque appel de cette fonction
miseAJourVarSysXYZ;

s1=size(x);
s2=size(y);
s3=size(z);

% Si pour x on a une matrice alors :
% ligne1 = x
% ligne2 = y
% ligne3 = z
% => Mise a jour des variables x, y et z
if (s1(1)==2 & s2(1)==0 & s3(1)==0)
    y=x(2,:);
    x=x(1,:);
end

if (s1(1)==3 & s2(1)==0 & s3(1)==0)
    y=x(2,:);
    z=x(3,:);
    x=x(1,:);
end

% Traitement de /DATA, /NORMAL, /DEVICE, /TO_DATA, /TO_NORMAL, /TO_DEVICE
if ~isempty(dataa)
    if ~isempty(to_normall)
        res=dataToNormal(x,y,z);                % Convertir de data vers normal
    elseif ~isempty(to_devicee)
        res=dataToDevice(x,y,z);                % Convertir de data vers device
    end
else
    if ~isempty(normall)
        if ~isempty(to_dataa)                 % Convertir de normal vers data
            res=normalToData(x,y,z);
        elseif ~isempty(to_devicee)
            res=normalToDevice(x,y,z);          % Convertir de normal vers device
        end
    else
        if ~isempty(devicee)
            if ~isempty(to_normall)
                res=deviceToNormal(x,y,z);      % Convertir de device vers normal
            elseif ~isempty(to_dataa)
               res=deviceToData(x,y,z);        % Convertir de device vers data
            end
        end
    end
end

% Traitement de T3D
if ~isempty(t3dd)
    warning('Function CONVERT_COORD: keyword T3D not translated');
end;

if I2M_out, eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: miseAJourVarSysXYZ
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function miseAJourVarSysXYZ

% Declaration des variables systemes IDL !X, !y et !Z
global i2mvs_x i2mvs_y i2mvs_z;

%  Mise a jour de !X.S, !Y.S et !Z.S a chaque appel de cette fonction
xlim=get(gca,'Xlim');
ylim=get(gca,'Ylim');
zlim=get(gca,'Zlim');

i2mvs_x.s=[-xlim(1)/(xlim(2)-xlim(1)) , 1/(xlim(2)-xlim(1))];
i2mvs_y.s=[-ylim(1)/(ylim(2)-ylim(1)) , 1/(ylim(2)-ylim(1))];
i2mvs_z.s=[-zlim(1)/(zlim(2)-zlim(1)) , 1/(zlim(2)-zlim(1))];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=dataToNormal(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=dataToNormal(x,y,z)

% Declaration des variables systemes IDL !X, !y et !Z
global i2mvs_x i2mvs_y i2mvs_z;

for i=1:length(x)
    n(1,i)=i2mvs_x.s(1)+i2mvs_x.s(2)*x(i);
    if length(y)~=0
        n(2,i)=i2mvs_y.s(1)+i2mvs_y.s(2)*y(i);
    else
        n(2,i)=0;
    end
    if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=dataToDevice(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=dataToDevice(x,y,z)

% Declaration des variables systemes IDL !D, !X, !y et !Z
global i2mvs_d i2mvs_x i2mvs_y i2mvs_z;

for i=1:length(x)
    n(1,i)=i2mvs_d.x_vsize * (i2mvs_x.s(1)+i2mvs_x.s(2)*x(i));
    if length(y)~=0
        n(2,i)=i2mvs_d.y_vsize * (i2mvs_y.s(1)+i2mvs_y.s(2)*y(i));
    else
        n(2,i)=0;
    end
    if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=normalToData(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=normalToData(x,y,z)

% Declaration des variables systemes IDL !X, !y et !Z
global i2mvs_x i2mvs_y i2mvs_z;

for i=1:length(x)
    n(1,i)=(x(i) - i2mvs_x.s(1)) / i2mvs_x.s(2);
    if length(y)~=0
        n(2,i)=(y(i) - i2mvs_y.s(1)) / i2mvs_y.s(2);
    else
        n(2,i)=0;
    end
    if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=normalToDevice(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=normalToDevice(x,y,z)

% Declaration de !D
global i2mvs_d;

for i=1:length(x)
    n(1,i)=x(i) * i2mvs_d.x_vsize;
    if length(y)~=0
        n(2,i)=y(i) * i2mvs_d.y_vsize;
    else
        n(2,i)=0;
    end
    if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=deviceToData(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=deviceToData(x,y,z)

% Declaration des variables systemes IDL !D, !X, !y et !Z
global i2mvs_d i2mvs_x i2mvs_y i2mvs_z;

for i=1:length(x)
    n(1,i)=(x(i) / i2mvs_d.x_vsize - i2mvs_x.s(1)) / i2mvs_x.s(2);
    if length(y)~=0
        n(2,i)=(y(i) / i2mvs_d.y_vsize - i2mvs_y.s(1)) / i2mvs_y.s(2);
    else
        n(2,i)=0;
    end;
   if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: [res]=deviceToNormal(x,y,z)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [n]=deviceToNormal(x,y,z)

% Declaration des variables systemes IDL !D, !X, !y et !Z
global i2mvs_d;

for i=1:length(x)
    n(1,i)=x(i) / i2mvs_d.x_vsize;
    if length(y)~=0
        n(2,i)=y(i) / i2mvs_d.y_vsize;
    else
        n(2,i)=0;
    end;
    if length(z)~=0
        n(3,i)=z(i);
    else
        n(3,i)=0;
    end
end







