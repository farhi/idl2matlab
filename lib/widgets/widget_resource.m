function bgc=widget_resource(wid,bgc,resource_name,map,sens)
%******* *******************
%**

global i2m_wcolor

if resource_name,  switch resource_name,
	case 'lamp'; bgc=1; case 'ben';    bgc=2; case 'mic';    bgc=3; case 'don';    bgc=4;
	case 'touch';bgc=5; case 'lampben';bgc=6; case 'lampmic';bgc=7; case 'lampdon';bgc=8;
	case 'lampdid';bgc=9;
	otherwise; end;
	end;

if ~map;  set(wid,'visible','off'); end;
if ~sens; set(wid,'enable' ,'off'); end;

typ=get(wid,'type');
%if typ == 'uicontrol', styl=get(wid,'style'); switch styl,
%	case 'frame';       set(wid,'BackgroundColor',[0 1 1]);
%   case 'pushbutton';  set(wid,'BackgroundColor',[0 0.5 1],'ForegroundColor',[0 1 1]);
%   case 'radiobutton'; set(wid,'BackgroundColor',[0 1 1]);
%   case 'checkbox';    set(wid,'BackgroundColor',[0 1 1]);
%	case 'edit';        set(wid,'BackgroundColor',[0.5 1 1]);
%	case 'text';        set(wid,'BackgroundColor',[0 1 1]);
%   case 'slider';      set(wid,'BackgroundColor',[0.6 1 1]);
%	case 'listbox';     set(wid,'BackgroundColor',[0.8 1 1]);
%	case 'popupmenu';   set(wid,'BackgroundColor',[0 0.5 1]);
%	otherwise; end;
%end;

if typ == 'uicontrol', styl=get(wid,'style'); switch styl,
	case 'frame';       set(wid,'BackgroundColor',[0 0 0]);
	case 'pushbutton';  set(wid,'BackgroundColor',i2m_wcolor);
	case 'radiobutton'; set(wid,'BackgroundColor',i2m_wcolor);
	case 'checkbox';    set(wid,'BackgroundColor',i2m_wcolor);
	case 'edit';        set(wid,'BackgroundColor',i2m_wcolor);
	case 'text';        set(wid,'BackgroundColor',i2m_wcolor);
	case 'slider';      set(wid,'BackgroundColor',[0.95 0.95 0.95]);
	case 'listbox';     set(wid,'BackgroundColor',[1 1 1]);
	case 'popupmenu';   set(wid,'BackgroundColor',[1 1 1]);
	otherwise; end;
end;
