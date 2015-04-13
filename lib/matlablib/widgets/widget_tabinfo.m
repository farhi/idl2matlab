function val=widget_tabinfo(base,flag,use_tab,vol)
%*******     **************
%**
global I2Mfig

fii=int16(base); par=I2Mfig(fii,2);
val=[];
wtx=get(par,'userdata');

if flag == 'edit', elseif ~isempty(use_tab), use=use_tab+1;
    if prod(size(use_tab))  == 2, wtx=wtx(use(1),use(2)); 
    else,                         wtx=wtx(use(1):use(3),use(2):use(4)); end;
end;
nx =prod (size(wtx));
wta=cell (size(wtx));
wtn=zeros(size(wtx));

switch flag;
case 'getv'; for i=1:nx, wta(i)=get(wtx(i),'string'); end;
	     uva=get(wtx(1),'userdata');    nume=uva(4);
	     if nume, wta=float(wta);
	     elseif n_elements(wta) > 1, wta=i2mstr(wta); else, wta=wta{1}; end;
	     val=wta;

case 'setv'; if isnumeric(vol), vol=strung(vol); nume=1; else, nume=0; end;
	     if isa(vol,'i2mstr'), val=vol(:); elseif ischar(vol),   val={vol}; elseif iscell(vol), val=vol;
				               elseif isstruct(vol), val= vol;  else,  val={'Empty'};   end;
	     if ~isstruct(val),
	     for i=1:nx, set(wtx(i),'string',val(i)); end; end;
case 'edit'; for i=1:nx, if use_tab, set(wtx(i),'Enable','on'); else, set(wtx(i),'Enable','inactive');  end; end;

case 'edce'; for i=1:nx, set(wtx(i),'Enable','on'); end;

case 'c_xs'; for i=1:nx, pos=get(wtx(i),'position'); wtn(i)=pos(3); end; val=wtn;
case 'r_ys'; for i=1:nx, pos=get(wtx(i),'position'); wtn(i)=pos(4); end; val=wtn;
case 't_ed'; val=1;
case 't_ec'; val=I2Mfig(int16(par),19:20)-1;
case 't_se'; val=[-1 -1 -1 -1]

otherwise;
end;
