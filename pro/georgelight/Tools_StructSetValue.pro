;***************************************************************
;** Project: Light
;** File:    Tools.pro
;** Version: 0.1
;** Date:    Jan, 23rd, 2002
;** Author:  E. Farhi
;** Object:  Tools for handling Structures, Variables, etc...
;
;** Require: <none>
;
; Procedures:
;   Tools_StructSetValue, struct=S, tag=Tag, val=Val
;       operates S.Tag = Val.
;       Recursive for structures of structures (a.b.c=value)
;***************************************************************

;***************************************************************
pro Tools_StructSetValue, struct=S, tag=Tag, val=Val
;** Procedure that operates S.Tag = Val
;** even if dimension or type does not match
;** It can also add a tag to the structure
;** ex: Tools_StructSetValue, struct=DialVars.Global, Tag='LogText', val=LogText

  idx = strpos(Tag, '.')
  if idx ge 0 then begin
    RecUpperStruct = strtrim(strmid(Tag,0, idx(0)),2)
    RecLowerStruct = strtrim(strmid(Tag,idx(0)+1, strlen(Tag)),2)
    ok = execute('RecUpperStructVal = S.'+RecUpperStruct)
    Tools_StructSetValue, struct=RecUpperStructVal, Tag=RecLowerStruct, Val=val
    Tools_StructSetValue, struct=S, Tag=RecUpperStruct, Val=RecUpperStructVal
  endif else begin
    sv =size(Val)
    if strlen(Tag) eq 0 then Tag='VALUE' else Tag=strupcase(Tag)
    TagList=strupcase(tag_names(S))
    Index  =where(TagList eq Tag)
    sz     =[0,0,0]
    vardims = N_ELEMENTS(sv) - 2
    type = sv[vardims]
    if Index(0) ge 0 then dummy=execute('sz=SIZE(S.'+Tag+')')
    if (sz(sz(0)+1) ne sv(sv(0)+1)) or (sz(0) ne sv(0)) $
        or (sz(sz(0)+2) ne sv(sv(0)+2)) $
        or type eq 8 then begin
      EvalStr = ''
      for k=0,n_elements(TagList)-1 do begin
        case TagList(k) of
          Tag:
          else: EvalStr =EvalStr+','+TagList(k)+':S.'+TagList(k)
        endcase
      endfor
      dummy=execute('S={'+TAG+':Val'+ EvalStr +'}')
    endif else dummy=execute('S.' +Tag+'=Val')
  endelse

end; PRO Tools_StructSetValue
;***************************************************************
