function  fmtc=i2m_format(fmtf)
%********      **********
%**

lst=str_sep(fmtf,','); fmtc='';
for i=1:n_elements(lst), elm=lst(i);
   %suppress repeat count
   %*********************
    p=strfind(elm,'(');
    if ~isempty(p), elm=strmid(elm,p,100); elm=strrep(elm,')',''); end;
    p=strfind(elm,')');
    if ~isempty(p), elm=strrep(elm,')',''); end;

   %double reserved string %
   %************************
    g=strfind(elm,'%');  if ~isempty(g), elm=strrep(elm,'%','%%'); end;
    
   %a string?
   %*********
    g=strfind(elm,'''');
    if ~isempty(g), elm=strrep(elm,'''',''); else, g=strfind(elm,'"');
    if ~isempty(g), elm=strrep(elm,'"','');

   %or a format code
   %****************
    else, c=lower(elm(1)); elm=strmid(elm,1,100); elm=[' %0' elm c ];
    end; end;
    
    if strcmp(elm,' '), elm=','; end;
    fmtc=[fmtc,elm];
end;
%disp(['i2m_format: ' fmtf '->' fmtc]);
