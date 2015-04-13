function widget_i2mevent(s)
%***********************
%**
global I2Mfig I2Mfevn
global I2Mlastevent

% I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top ,12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc, 19:20=szz
% lay= 1:column, 2:row, 3:none
% uic= 1:base,   2:uic, 3:fig
% but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar
fii=double(int16(s.id)); s.id=I2Mfig(fii,1); s.top=I2Mfig(I2Mfig(fii,11),1); fig=I2Mfig(fii,3);
uiresume(fig); %in case we are waiting in widget_event

typ=get(s.id,'type'); if typ == 'uicontrol', styl=get(s.id,'style'); else, styl='?'; end;

if I2Mfig(fii,16) == 1  ; %Radio Buttons*****
   s.select=get(s.id,'value');
   idx=find(I2Mfig(:,2) == I2Mfig(fii,2));
   if (idx), for i=1:n_elements(idx); me=idx(i); if me ~= fii,
                 if I2Mfig(me,16)==1, set(I2Mfig(me,1),'value',0); end; end; end; end;
end;
if I2Mfig(fii,16) == 2  ; s.select=get(s.id,'value'); end;

if I2Mfig(fii,16) == 3.6; %CascadeButton*****
   ctx=I2Mfig(fii,17); pos=I2Mfig(fii,7:8); set(ctx,'position',pos,'visible','on'); return; end;

if styl == 'edit';        %Editable Text*****
   spos=get(0,'PointerLocation'); pos=get(s.id,'position'); fpos=get(fig,'position');
   X=spos(1)-fpos(1); Y=spos(2)-fpos(2);
   if (X < pos(1)) | (X > pos(1)+pos(3)) | (Y < pos(2)) | (Y > pos(2)+pos(4)); return; end;

   uva=get(s.id,'userdata'); if length(uva)==6;
   if uva(1)==19;         %Table event  *****
      s.structure_name='WIDGET_TABLE_STR'; s.x=uva(2); s.y=uva(3);
      par=int16(I2Mfig(fii,2)); pir=int16(I2Mfig(par,2)); pir=int16(I2Mfig(pir,2));
      I2Mfig(pir,19:20)=uva(2:3)+1; %For last edit_cell
   end; end;
end;

if styl == 'slider';       %Slider value*****
   V=get(s.id,'value');  V=round(V); set(s.id,'value',V); tit=get(s.id,'string');
   if tit(1)=='.', V=get(s.id,'min')+get(s.id,'max') -V ; end; s.value =V ;  end;


% Search for an event handler
% ***************************
ok=1; eventp=lower(I2Mfevn{fii});
while ok;
	if (~isempty(eventp) | I2Mfig(fii,6) == 3); ok=0;
	else; fii=double(int16(I2Mfig(fii,2))); eventp=lower(I2Mfevn{fii}); end;
end
s.handler=0;
% Call the event handler function or procedure
% ********************** ******** ** *********
if ~isempty(eventp); s.handler=I2Mfig(fii,1); fct=0;
    p=strfind(eventp,'___'); if p, if p==1,   fct=1; eventp=strmid(eventp,3,50); end; end;
    if fct, try; ev=eval([lower(eventp) '(s)','']); if isstruct(ev), i2mevent(ev); end; catch; end;
    else,   try;    eval([lower(eventp) '(s)']);    catch; end; end;
end;

I2Mlastevent=s;
if ishandle(fig), set(fig,'pointer','arrow'); end;
