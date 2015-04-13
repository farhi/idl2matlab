function cmd=i2m_tr(command)
%*******     ******
%IDL command has to be parsed to this translator.

%Actually not available: ("Hex and 'oct # ?) , (pointers goto switch,endswitch) , (@) , ($) , (0L...)
%                        (function,pro,return,I2M_pos,_extra) (common on_ioerror catch n_elements) (<tab>)

% for line translation, less important lines are commented as %!
% for file translation, use WHICH instead of EVALIN

global I2M_tr_putend I2M_tr_mtc I2M_tr_block I2M_tr_incas

if length(I2M_tr_mtc) ==0, I2M_tr_mtc='[(,=:*#+-/\><'; I2M_tr_block=0; I2M_tr_incas=0; end;

cmd=command;

%force Char string separator to 'quote' and put string constants aside:
    dg=strfind(cmd,'"'); gg=strfind(cmd,''''); aside={};
    if ~isempty(dg); if ~isempty(gg); if dg(1) < gg(1); cmd(dg)=''''; cmd(gg)='"'; gg=dg; end; else cmd(dg)=''''; gg=dg; end; end;
    if ~isempty(gg); for i=2:2:length(gg); aside{i/2}=cmd(gg(i-1):gg(i)); cmd(gg(i-1)+1:gg(i)-1)='ç'; end; cmd=strrep(cmd,'ç',''); end;

%put comments aside:
    [cmd,comment]=strtok(cmd,';');

%put blank arount separators:
    s_ida=strfind(cmd,'{');     if ~isempty(s_ida); cmd=strrep(cmd,'{'    ,' { '); cmd=strrep(cmd,'}',' } '); end;
    b_ida=strfind(cmd,'[');     if ~isempty(b_ida); cmd=strrep(cmd,'['    ,' [ '); cmd=strrep(cmd,']',' ] '); end;
    p_ida=strfind(cmd,'(');     if ~isempty(p_ida); cmd=strrep(cmd,'('    ,' ( '); cmd=strrep(cmd,')',' ) '); end;
    v_ida=strfind(cmd,',');     if ~isempty(v_ida); cmd=strrep(cmd,','    ,' , '); end;
    a_ida=strfind(cmd,'&');     if ~isempty(a_ida); cmd=strrep(cmd,'&'    ,' & '); end;
    g_ida=strfind(cmd,'>');     if ~isempty(g_ida); cmd=strrep(cmd,'>'    ,' > '); end;
    l_ida=strfind(cmd,'<');     if ~isempty(l_ida); cmd=strrep(cmd,'<'    ,' < '); end;
    m_ida=strfind(cmd,' mod '); if ~isempty(m_ida); cmd=strrep(cmd,' mod ',' \ '); end;
    cmd=strrep(cmd,'=',' = ');  cmd=strrep(cmd,':',' : '); cmd=strrep(cmd,'##',' * ');
    cmd=strrep(cmd,'^',' ^ ');  cmd=strrep(cmd,'*',' * '); cmd=strrep(cmd,'#' ,' # ');
    cmd=strrep(cmd,'+',' + ');  cmd=strrep(cmd,'-',' - '); cmd=strrep(cmd,'/' ,' / ');

%lower all variables then compress:
    cmd=lower(cmd); while strfind(cmd,'  '); cmd=strrep(cmd,'  ',' '); end; cmd=[' ' cmd '  '];

%**************************************************************************************** TRADUCTOR ***********
cmt='%'; %comment char
cmc=';'; %command separator

%change some function names
%--------------------------
    cmd=strrep(cmd,' cdd '         ,' M2I_cdd ');	cmd=strrep(cmd,' cd ,'          ,' cdd ,');
    cmd=strrep(cmd,' fixt '        ,' M2I_fixt ');	cmd=strrep(cmd,' fix ('         ,' fixt (');
    cmd=strrep(cmd,' sizz '        ,' M2I_sizz ');	cmd=strrep(cmd,' size ('        ,' sizz (');
    cmd=strrep(cmd,' maax '        ,' M2I_maax ');	cmd=strrep(cmd,' max ('         ,' maax (');
    cmd=strrep(cmd,' miin '        ,' M2I_miin ');	cmd=strrep(cmd,' min ('         ,' miin (');
    cmd=strrep(cmd,' sortt '       ,' M2I_sortt ');	cmd=strrep(cmd,' sort ,'        ,' sortt ,');
%!  cmd=strrep(cmd,' cloze '       ,' M2I_cloze ');	cmd=strrep(cmd,' close ,'       ,' cloze ,'); %!
    cmd=strrep(cmd,' atann '       ,' M2I_atann ');	cmd=strrep(cmd,' atan ('        ,' atann (');
    cmd=strrep(cmd,' rotat '       ,' M2I_rotat ');	cmd=strrep(cmd,' rotate ('      ,' rotat (');
    cmd=strrep(cmd,' printt '      ,' M2I_printt ');	cmd=strrep(cmd,' print ,'       ,' printt ,');
    cmd=strrep(cmd,' strung '      ,' M2I_strung ');	cmd=strrep(cmd,' string ('      ,' strung (');
    cmd=strrep(cmd,' doubll '      ,' M2I_doubll ');	cmd=strrep(cmd,' double ('      ,' doubll (');
%!  cmd=strrep(cmd,' strcomp '     ,' M2I_strcomp ');	cmd=strrep(cmd,' strcmp ('      ,' strcomp ('); %!
    cmd=strrep(cmd,' transpos '    ,' M2I_transpos ');	cmd=strrep(cmd,' transpose ('   ,' transpos (');

    cmd=strrep(cmd,' ( ) '         ,' ');

%targetCode functions
%--------------------
    cmd=strrep(cmd,' now '         ,' M2I_now ');
    cmd=strrep(cmd,' rem '         ,' M2I_rem ');

%sys variables
%-------------
    cmd=strrep(cmd,' datestr '     ,' M2I_datestr ');	cmd=strrep(cmd,' !stime '      ,' datestr(now) ');
    cmd=strrep(cmd,' lasterr '     ,' M2I_lasterr ');	cmd=strrep(cmd,' !err_string ' ,' lasterr ');
    if strfind(cmd,'!'),
    cmd=strrep(cmd,' !c '          ,' i2mvs_c ');       cmd=strrep(cmd,' !version'     ,' i2mvs_version');
    cmd=strrep(cmd,' !quiet '      ,' i2mvs_quiet ');   cmd=strrep(cmd,' !error'       ,' i2mvs_error');
    cmd=strrep(cmd,' !d.flags '    ,' 65536 ');
    end
%equivalence
%-----------
    cmd=strrep(cmd,' log '         ,' M2I_log ');	cmd=strrep(cmd,' alog ('          ,' log (');
    cmd=strrep(cmd,' inv '         ,' M2I_inv ');	cmd=strrep(cmd,' invert ('        ,' inv (');
    cmd=strrep(cmd,' log10 '       ,' M2I_log10 ');	cmd=strrep(cmd,' alog10 ('        ,' log10 (');
%!  cmd=strrep(cmd,' struct '      ,' M2I_struct ');	cmd=strrep(cmd,' create_struct (' ,' struct ('); %!

%brackets inside
%---------------
    if b_ida; b_ida=strfind(cmd,'['); ln=length(b_ida);
          for i=1:ln, whh(i)=~isempty(strfind(I2M_tr_mtc,cmd(b_ida(i)-2)));   end;
          lst=str_brack(cmd,'[]');
          for i=1:ln, idx=find (lst(b_ida(i):end) < lst(b_ida(i)));    whe(i)=b_ida(i)+idx(1)-2;
                      cmd(whe(i))=')'; if ~whh(i),  cmd(b_ida(i))='('; p_ida=1; end;
          end;
          cmd=strrep(cmd,'[','d1_array (');
    end;

%structure inside
%----------------
    if s_ida; sida=str_brack(cmd,'{}'); bida=str_brack(cmd,'[]'); pida=str_brack(cmd,'()'); tmp=~(bida+pida+(~sida));
       sids=strfind(cmd,','); if sids; sids=sids.*(tmp(sids)); sids=sids(sids~=0); end;
       cids=strfind(cmd,':'); if cids; cids=cids.*(tmp(cids)); cids=cids(cids~=0); end;
       sepa=sort([strfind(cmd,'{') cids sids strfind(cmd,'}')]);
       j=length(cids);
       for i=1:j; x=find(sepa==cids(i)); ele(i*2-1)=sepa(x-1)+1; ele(i*2)=sepa(x)-1; end; ele(i*2+1)=length(cmd);
       %do translation
        cmd(cids)=','; new=cmd(1:ele(1));
        for i=1:2:length(ele)-2; new=[new '''' cmd(ele(i)+1:ele(i+1)-1) '''' cmd(ele(i+1):(ele(i+2)))];  end;
	%case structure_name
         tx =strfind(new,'{'); for i=1:length(tx); if strmid(new,tx(i)+1,1) ~='''';
                                                      new=[new(1:tx(i)+1) '''structure_name'' , ' new(tx(i)+2:end)];
                                                      tx =strfind(new,'{'); end; end;
       cmd=strrep (new,'{','struct ('); cmd=strrep(cmd,'}',')');
    end

%Special operators "<" ">" "\"(mod)
%----------------------------------
    if ~isempty(g_ida) | ~isempty(l_ida) | ~isempty(m_ida); gl_ida=[g_ida l_ida m_ida 0];
       while length(gl_ida)>1,
           gl_ida=sort([strfind(cmd,'>') strfind(cmd,'<') strfind(cmd,'\')]);
           lst=str_brack(cmd,'()');
           idx=strfind(cmd(gl_ida(1)+2:end),' ')+gl_ida(1)+1; tmp=find(lst(idx)==lst(gl_ida(1)));    idxr=idx(tmp);
				     for i=1:1:length(idxr) , if cmd(idxr(i)+1) ~= '(',              idxr=idxr(i); break; end; end;
           idx=strfind(cmd(1:gl_ida(1)-2)  ,' ');             tmp=find(lst(idx)==lst(gl_ida(1)));    idxl=idx(tmp);
				     for i=length(idxl):-1:1, if strfind(I2M_tr_mtc,cmd(idxl(i)-1)), idxl=idxl(i); break; end; end;
           if cmd(gl_ida(1))=='>', i2m='max'; elseif cmd(gl_ida(1))=='<', i2m='min'; else i2m='i2m_mod'; end;
           cmd=[cmd(1:idxl) i2m ' ( ' cmd(idxl+1:gl_ida(1)-1) ',' cmd(gl_ida(1)+1:idxr) ') ' cmd(idxr+1:end)];
       end;
    end;

%flow (for, if, while, repeat, case, begin)
%----
%Idl keywords:
%------------

   %change repeat->while
   %--------------------
    if strfind(cmd,'rep' ),
       cmd=strrep (cmd,' repeat begin ',' while 1 do begin ');                   %!
       cmd=strrep (cmd,' repeat '      ,' while 1 do begin ');                   %!
       cmd=strrep (cmd,' endrep '      ,' ');                                    %!
       s_til=strfind(cmd,' until '); if s_til;                                   %!
	  for i=1:length(s_til), idx=str_endbr(cmd,s_til(i)); cmd(idx)='à'; end; %!
	  cmd=strrep(cmd,' until ',' if ');                                      %!
	  cmd=strrep(cmd,'à'      ,' then break & endwhile '); end; end;         %!
   %--------------------
    if strfind(cmd,' end' ),
       cmd=strrep(cmd,' endif else '  ,' else '); cmd=strrep(cmd,' end else '  ,' else '); %!
       cmd=strrep(cmd,' end '         ,' end; ');
       cmd=strrep(cmd,' endif '       ,' end; ');
       cmd=strrep(cmd,' endfor '      ,' end; ');
       cmd=strrep(cmd,' endelse '     ,' end; ');
       cmd=strrep(cmd,' endwhile '    ,' end; '); end;            %!

    if strfind(cmd,' begin ' ),
       cmd=strrep(cmd,' do begin '    ,' do ïegin ');
       cmd=strrep(cmd,' then begin '  ,' then ïegin ');
       cmd=strrep(cmd,' else begin '  ,' else ïegin ');
       cmd=strrep(cmd,' begin '       ,' if 1 then ïegin '); end; %!

   %case statement
   %--------------
    s_cas=strfind(cmd,' case ' ); if s_cas, I2M_tr_incas=I2M_tr_incas+length(s_cas); %!
                                  cmd=strrep(cmd,' case ',' switch '); cmd=strrep(cmd,' of ',' '); end;
    if I2M_tr_incas > 0; cmd=strrep(cmd,' else : '  ,' otherwise ');             %!
       s_col=strfind(cmd,' : ');
       if s_col; for i=1:length(s_col), idx=str_endbl(cmd,s_col(i));  cmd(idx)='ç'; end;
                 cmd=strrep(cmd,'ç',' case '); cmd=strrep(cmd,' : ',' ');           end;
       s_cas=strfind(cmd,' endcase ' ); if s_cas, I2M_tr_incas=I2M_tr_incas-length(s_cas); cmd=strrep(cmd,' endcase ',' end '); end; end;

   %change FOR loop syntax
   %----------------------
    s_for=strfind(cmd,' for ' ); if s_for;
	  for i=1:length(s_for), dodo=strfind(cmd(s_for(i):end),' do '); eqeq=strfind(cmd(s_for(i):end),' = ');
		  loop =cmd(s_for(i)+eqeq(1)+1:s_for(i)+dodo(1)-1);      lst =str_brack(loop,'()');
		  v_ida=strfind(loop,','); vv_ida=[];
		  for j=1: length(v_ida),  if lst(v_ida(j))==0, vv_ida(end+1)=v_ida(j); end; end;
		  loop(vv_ida(1))=':';
		  if length(vv_ida)==2; loop=[loop(1:vv_ida(1)) loop(vv_ida(2)+1:end) ':' loop(vv_ida(1)+1:vv_ida(2)-1)]; end;
		  cmd(s_for(i)+eqeq(1)+1:s_for(i)+dodo(1)-1)=loop;
	  end;    end;

   %missing end;
   %-----------
    s_end=strfind(cmd,' end; '); s_els=strfind(cmd,' else ');
    s_do =strfind(cmd,' do ' );  s_if =strfind(cmd,' then '); s_ifdo=[s_do+3 s_if+5];
    if ~isempty(s_ifdo) | I2M_tr_block | s_end; lst(1:length(cmd))=I2M_tr_block;
     if ~isempty(s_ifdo); s_ifdo=sort(s_ifdo);
	       for i=1:length(s_ifdo), if cmd(s_ifdo(i)+1)  ~= 'ï', I2M_tr_putend=[I2M_tr_putend,I2M_tr_block];
	                               else I2M_tr_block=I2M_tr_block+1; lst(s_ifdo(i)+1:end)=I2M_tr_block; end; end;
     end;
     if s_end, for i=1:length(s_end),  if I2M_tr_block >0, I2M_tr_block=I2M_tr_block-1; lst(s_end(i)+5:end)=I2M_tr_block;
                                       else cmd(s_end(i):s_end(i)+4)='     '; end; end; s_end=[]; end;
     if ~isempty(I2M_tr_putend);
        cmd=[cmd ' & end']; I2M_tr_putend=[]; %only for simple if statement !!!!
                                              %****************************
     end;
	      cmd=strrep(cmd,' else ïegin '  ,' else ');
     if s_do; cmd=strrep(cmd,' do ïegin '    ,' ');   cmd=strrep(cmd,' do '  ,' '); end;
     if s_if; cmd=strrep(cmd,' then ïegin '  ,' ');   cmd=strrep(cmd,' then ',' '); end;
    end;

    cmd=strrep(cmd,' le '    ,' <= ');    cmd=strrep(cmd,' eq '    ,' == '); cmd=strrep(cmd,' ge '    ,' >= ');
    cmd=strrep(cmd,' lt '    ,' < ' );    cmd=strrep(cmd,' ne '    ,' ~= '); cmd=strrep(cmd,' gt '    ,' > ' );
    cmd=strrep(cmd,' and '   ,' é ' );    cmd=strrep(cmd,' or '    ,' | ' ); cmd=strrep(cmd,' not '   ,' ~ ' );
    cmd=strrep(cmd,' xor '   ,' .\ ' );

%!  if strfind(cmd,' compile_opt '),      cmd=['%' command]; return; end; %!
%!  if strfind(cmd,' forward_function '), cmd=['%' command]; return; end; %!

%parenthese, procedure       p_ida(i)->'(' , whe(i)->')' , whh(i)=0-> var in front
%---------------------       -------------   -----------   -----------------------
    lst=str_brack(cmd,'()');
    if ~isempty(v_ida); v_ida=strfind(cmd,','); vv_ida=[]; okp=0;
          for i=1: length( v_ida), if lst(v_ida(i))==0, vv_ida(end+1)=v_ida(i); end; end;
          i=1;  jj=length(vv_ida);
          while i<=jj; idx=strfind(cmd(1:vv_ida(i)-2),' '); var=cmd(idx(end)+1:vv_ida(i)-2);
          	if isnan(str2double(var)) & ~strcmp(var,'if') & ~strcmp(var,'for');
                         vv   =evalin('caller',['who(''' var ''');'],'vv={};');
                         %Name is procedure
                         %-----------------
                         if isempty(vv); p_ida=1; okp=1; cmd(vv_ida(i))='(';
                            cmdt=cmd(vv_ida(i):end);
                            idx1=strfind(cmdt,' & '); idx2=strfind(cmdt,' else'); idx3=strfind(cmdt,' end;');
                            idx =sort([idx1 idx2 idx3])+vv_ida(i)-1;
                            if ~isempty(idx), cmd(idx(1))='î'; for k=i:jj, if vv_ida(k)<idx(1), i=k; end; end;
                            else,             cmd(end)   ='î'; break; end;
                end;     end;
                i=i+1;
          end;
          if okp, cmd=strrep(cmd,'î' ,' ) '); lst=str_brack(cmd,'()'); end;
    end;
    if ~isempty(p_ida); p_ida=strfind(cmd,'('); ln=length(p_ida); oki=0; okk=0; okx=0;
          for i=1:ln, whh(i)=~isempty(strfind(I2M_tr_mtc,cmd(p_ida(i)-2)));   end;
          for i=1:ln, idx=find(lst(p_ida(i):end) < lst(p_ida(i)));     whe(i)=p_ida(i)+idx(1)-2;
                    %Check name(...)
                    %---------------
                    if cmd(p_ida(i)-2) ~= ')'
                    if ~whh(i), idx=strfind(cmd(1:p_ida(i)-2),' '); var=cmd(idx(end)+1:p_ida(i)-2);
          		if isnan(str2double(var)) & ~strcmp(var,'if') & ~strcmp(var,'for');
                              vv   =evalin('caller',['who(''' var ''');'],'vv={};');
                              v_ida=strfind(cmd(p_ida(i):whe(i)),',')+p_ida(i)-1;
                              rang =lst(p_ida(i));
                              %Name is variable (indice+1)
                              %----------------
                              if ~isempty(vv), c_ida=strfind(cmd(p_ida(i):whe(i)),':')+p_ida(i)-1;
                                       v_ida=[v_ida c_ida whe(i)];
                                       for ii=1:length(v_ida),
                                         jj =v_ida(ii);
                                         if lst(jj) == rang, if cmd(jj-2) == '*', cmd(jj-2)=':';
                                                             else, cmd(jj-1)='è'; oki=1; end; end; end;
                              %Name is function (keyword)
                              %----------------
                              else,  v_ida=[p_ida(i) v_ida whe(i)];
                                     e_ida=strfind(cmd(p_ida(i):whe(i)),' = ')+p_ida(i)-1 +1;
                                     h_ida=strfind(cmd(p_ida(i):whe(i)),', /')+p_ida(i)-1 +2; hh_ida=[];
                                     for h=1:length(h_ida), if lst(h_ida(h))==rang, hh_ida(end+1)=h_ida(h); end; end;
                                     if cmd(p_ida(i)+2)=='/', hh_ida=[p_ida(i)+2 hh_ida];  end;
                                     if ~isempty(e_ida) | ~isempty(hh_ida); vv_ida=[];
                                        for ii=1:length(v_ida), if lst(v_ida(ii))==rang,
                                                      vv_ida(end+1)=v_ida(ii); end; end;
                                        for ii=1:length(vv_ida)-1,
                                         jj =vv_ida(ii);
                                         if cmd(jj+2) == '/',   cmd(vv_ida(ii+1)-1)='ö';
                                         elseif isempty(strfind(cmd(jj:vv_ida(ii+1)),' = '));
                                               cmd(jj+1) ='û';
                                         else, cmd(jj+1) ='â';  end;
                                        end;
                                        okk=1; cmd(e_ida)='ä';  cmd(hh_ida)='ô';
                                     end;
                        end;  end;
                    end;
		    %indexed function (sizz(a))(i)
		    %-----------------------------
                    else, okx=1; cmd(whe(i)+1)='î'; end;
          end;
          if oki, cmd=strrep(cmd,'è' ,' + 1 '); end;                            %  idx+1
          if okk, cmd=strrep(cmd,'ô ','''');  cmd=strrep(cmd, 'ö',''' , 1 ');   % /key
                  cmd=strrep(cmd,'â' ,' '''); cmd=strrep(cmd,' ä',''' ,');      %  key=  !!!!!care undefined argument
                  nu=strfind(cmd,'û');
                  for ii=1:length(nu), rp=[' ''I2M_a' num2str(ii) ''' , '];     %  arg
                      mu=strfind(cmd,'û'); cmd(mu(1))='ü'; cmd=strrep(cmd,'ü',rp); end;
          end;
          if okx, cmd=strrep(cmd,') ) (' ,', 1 +'); cmd=strrep(cmd,'î',' ) '); end;
    end;

%d'nt change tag names & keywords
%--------------------------------
    cmd=strrep(cmd,'''M2I_' ,'''');

%******************************************************************************************* END **************

%replace "&" by ";"
    cmd=strrep(cmd,' &' ,cmc);
    cmd=strrep(cmd,' é ',' & ');

%indent
    cmd=strrep(cmd,'( ' ,'('); cmd=strrep(cmd,' )' ,')'); cmd=strrep(cmd,' , ',','); %!
    cmd=strrep(cmd,' = ','='); cmd=strrep(cmd,' + ','+'); cmd=strrep(cmd,' - ','-'); %!
    cmd=strrep(cmd,' * ','*'); cmd=strrep(cmd,' / ','/'); cmd=strrep(cmd,' ^ ','^'); %!
    cmd=strrep(cmd,' : ',':'); %!
    idx=find  (uint8(command) > 32);                                                 %!
    if ~isempty(idx), if idx(1) > 1, cmd=[command(1:idx(1)-1) cmd]; end; end;                              %!

%put string constants back into command:
    for i=1:length(aside); x=strfind(cmd,''''''); cmd=[cmd(1:x-1) aside{i} cmd(x+2:end)]; end;
    cmd=[cmd cmc cmt comment];

disp(cmd); disp(' ');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function lst=str_brack(cmd,typ)
%*******     *********
%**
lst(1:length(cmd))=0;
t1 =strfind(cmd,typ(1));
if ~isempty(t1); t2=strfind(cmd,typ(2));
    for i=1:length(t1);
        if t1(i) < t2(i); lst(t1(i):t2(i))=lst(t1(i):t2(i))+1; else lst(t1(i)+1:t2(i)-1)=lst(t1(i)+1:t2(i)-1)-1; end; end;
end

function idx=str_endbr(cmd,from)
%*******     *********
%**
cm =cmd(from:end); id1=strfind(cm,' & '); id2=strfind(cm,' else '); idx=[id1 id2];
if isempty  (idx), idx=length(cmd); else, idx=min(idx); idx=idx+from-1;  end;

function idx=str_endbl(cmd,to)
%*******     *********
%**
cm =cmd(1:to);     id1=strfind(cm,' & ');
if isempty (id1),  idx=1; else, idx=max(id1); idx=idx+2;  end;
