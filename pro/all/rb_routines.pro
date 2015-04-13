; $Id: rb_routines.pro,v 1.12 2001/01/15 22:28:10 scottm Exp $
;
; Copyright (c) 1999-2001, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;The rotuines in this file were created for use by READ_BINARY and
;BINARY_TEMPLATE.  The "rb_" prefix used on the routine names
;stands for Read_Binary, but the routines are used by binary_template
;also.

function rb_dim_str, dims, num_dims
;COMPILE_OPT hidden, strictarr
;
;Assemble a dimension string from the strings in DIMS.
;A dimension string is like '[640, 512]', and can have
;variables or expressions in it, e.g. '[xsize, ysize].'
;
if num_dims eq 0 then begin
    result = 'scalar'
    end $
else begin
    result = '['
    for i=0,num_dims-1 do begin
        result = result + strtrim(dims[i], 2)
        if i lt num_dims-1 then $
            result = result + ', '
        end
    result = result + ']'
    end
return, result
end
;--------------------------------------------------------------------
function rb_is_integral, val
;COMPILE_OPT hidden, strictarr
;
;Purpose: test value to see if it is an integer.
;
tname = size(val, /tname)

if tname eq 'BYTE' $
or tname eq 'INT' $
or tname eq 'LONG' $
or tname eq 'UINT' $
or tname eq 'ULONG' $
or tname eq 'LONG64' $
or tname eq 'ULONG64' then $
    return, 1

return, 0
end
;--------------------------------------------------------------------
function rb_template_is_valid, rb_template, edit=edit, msg=msg
;COMPILE_OPT hidden, strictarr
;
;Purpose: return 1 if template is valid, else return 0.
;Also return a message string via the MSG keyword.
;
;If keword EDIT is set, test template for use with the template
;editor, BINARY_TEMPLATE, else test the template for use with
;READ_BINARY.
;
;Note: some variables in this routine are named with an
;"rb_" prefix.  This is to help avoid clashes with field
;names specified in the template.  "rb_" stands for
;"Read_Binary".
;
msg = 'Template is invalid.'

if n_elements(rb_template) gt 1 then begin
    msg = 'Template cannot be an array.'
    return, 0
    end

if size(rb_template, /tname) ne 'STRUCT' then begin
    msg = 'Template must be a structure.'
    case size(rb_template, /tname) of
        'OBJREF':
        'POINTER':
        'STRING':
        'UNDEFINED': msg = 'Supplied template is undefined.'
        else: begin
            if rb_template eq 0 then $
                msg = 'Template is zero.'
            end
        endcase
    return, 0
    end
;
;Make sure 'version' field is present.
;
tag_names_found = tag_names(rb_template)
void = where(tag_names_found eq 'VERSION', count)
if count ne 1 then begin
    msg = 'Version field is missing from template.'
    return, 0
    end
;
;Check the rest of the fields in the template.
;
case rb_template.version of
    1.0: begin
        tag_names_required = strupcase([ $
            'endian', $
            'fieldCount', $
            'Typecodes', $
            'Names', $
            'Offsets', $
            'NumDims', $
            'Dimensions', $
            'reverseflags', $
            'AbsoluteFlags', $
            'ReturnFlags', $
            'VerifyFlags', $
            'VerifyVals' $
            ])
        if keyword_set(edit) then begin
            tag_names_required = [ $
                tag_names_required, $
                strupcase([ $
                    'TemplateName', $
                    'DimAllowFormulas', $
                    'OffsetAllowFormulas' $
                    ]) $
                ]
            end
        end
    else: begin
        msg = 'The only recognized template version is: 1.0.'
        return, 0
        end
    endcase

for i=0,n_elements(tag_names_required)-1 do begin
    void = where(tag_names_found eq tag_names_required[i], count)
    if count ne 1 then begin
        msg = tag_names_required[i] + ' field missing from template.'
        return, 0
        end
    end

if size(rb_template.names, /tname) ne 'STRING' then begin
    msg = 'Invalid template: Names must be of type STRING.'
    return, 0
    end

if keyword_set(edit) then begin
    if size(rb_template.templatename, /tname) ne 'STRING' then begin
        msg = 'Invalid template: TemplateName must be of type STRING.'
        return, 0
        end
    if n_elements(rb_template.templatename) gt 1 then begin
        msg = 'Invalid template: TemplateName cannot have more than ' $
            + 'one element.'
        return, 0
        end
    end

if size(rb_template.offsets, /tname) ne 'STRING' then begin
    msg = 'Invalid template: Offsets must STRING expressions.'
    return, 0
    end

if size(rb_template.dimensions, /tname) ne 'STRING' then begin
    msg = 'Invalid template: Dimensions must be STRING expressions.'
    return, 0
    end

if size(rb_template.verifyvals, /tname) ne 'STRING' then begin
    msg = 'Invalid template: VerifyVals must be STRING expressions.'
    return, 0
    end

if n_elements(rb_template.endian) gt 1 then begin
    msg = 'Invalid template: Endian specification cannot have more ' + $
        'than one element.'
    return, 0
    end

if not rb_is_integral(rb_template.fieldCount) then begin
    msg = 'Invalid template: FieldCount is not an integer.'
    return, 0
    end

if rb_template.fieldCount lt 1 then begin
    msg = 'Invalid template: FieldCount is less than 1.'
    return, 0
    end

if not rb_is_integral(rb_template.Typecodes) then begin
    msg = 'Invalid template: Typecodes must be integers.'
    return, 0
    end
if not rb_is_integral(rb_template.NumDims) then begin
    msg = 'Invalid template: NumDims must be integers.'
    return, 0
    end
if not rb_is_integral(rb_template.reverseflags) then begin
    msg = 'Invalid template: ReverseFlags must be integers.'
    return, 0
    end
if not rb_is_integral(rb_template.absoluteflags) then begin
    msg = 'Invalid template: AbsoluteFlags must be integers.'
    return, 0
    end
if not rb_is_integral(rb_template.ReturnFlags) then begin
    msg = 'Invalid template: ReturnFlags must be integers.'
    return, 0
    end
if not rb_is_integral(rb_template.VerifyFlags) then begin
    msg = 'Invalid template: VerifyFlags must be integers.'
    return, 0
    end

if keyword_set(edit) then begin
    if not rb_is_integral(rb_template.DimAllowFormulas) then begin
        msg = 'Invalid template: DimAllowFormulas must be integers.'
        return, 0
        end
    if not rb_is_integral(rb_template.OffsetAllowFormulas) then begin
        msg = 'Invalid template: OffsetAllowFormulas must be integers.'
        return, 0
        end
    end

if n_elements(rb_template.Typecodes) $
ne rb_template.fieldCount then begin
    msg = 'Invalid template: number of Typecodes does ' + $
        'not match FieldCount.'
    return, 0
    end
if n_elements(rb_template.Names) $
ne rb_template.fieldCount then begin
    msg = 'Invalid template: number of Names does ' + $
        'not match FieldCount.'
    return, 0
    end
if n_elements(rb_template.Offsets) $
ne rb_template.fieldCount then begin
    msg = 'Invalid template: number of Offsets does ' + $
        'not match FieldCount.'
    return, 0
    end
if n_elements(rb_template.NumDims) $
ne rb_template.fieldCount then begin
    msg = 'Invalid template: number of NumDims does ' + $
        'not match FieldCount.'
    return, 0
    end

siz = size(rb_template.Dimensions)
if siz[0] lt 2 then begin
    msg = 'Invalid template: Dimensions field must be a 2D array.'
    return, 0
    end
if siz[1] ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: 1st dimension of Dimensions array ' + $
            'should be same as FieldCount.'
    return, 0
    end
if siz[2] ne 8 then begin
    msg = $
        'Invalid template: Dimensions'' 2nd dimension must be 8.'
    return, 0
    end
siz = size(rb_template.reverseflags)
if siz[0] lt 2 then begin
    msg = $
        'Invalid template: ReverseFlags field must have two dimensions.'
    return, 0
    end
if siz[1] ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: 1st dimension of reverseflags array ' + $
            'should be same as FieldCount.'
    return, 0
    end
if siz[2] ne 8 then begin
    msg = $
        'Invalid template: ReverseFlags''s 2nd dimension must be 8.'
    return, 0
    end
if n_elements(rb_template.absoluteflags) $
ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: number of AbsoluteFlags does ' + $
            'not match FieldCount.'
    return, 0
    end
if n_elements(rb_template.ReturnFlags) $
ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: number of ReturnFlags does ' + $
            'not match FieldCount.'
    return, 0
    end
if n_elements(rb_template.VerifyFlags) $
ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: number of VerifyFlags does ' + $
            'not match FieldCount.'
    return, 0
    end

if keyword_set(edit) then begin
    if n_elements(rb_template.dimallowformulas) $
    ne rb_template.fieldCount then begin
        msg = $
            'Invalid template: number of dimallowformulas does ' + $
                'not match FieldCount.'
        return, 0
        end
    if n_elements(rb_template.offsetallowformulas) $
    ne rb_template.fieldCount then begin
        msg = $
            'Invalid template: number of offsetallowformulas does ' + $
                'not match FieldCount.'
        return, 0
        end
    end

if n_elements(rb_template.verifyvals) $
ne rb_template.fieldCount then begin
    msg = $
        'Invalid template: number of verifyvals does ' + $
            'not match FieldCount.'
    return, 0
    end

if not keyword_set(edit) then begin
    returns_indx = where(rb_template.ReturnFlags ne 0)
    if returns_indx[0] eq -1 then begin
        msg = 'Invalid template: no fields are set to be returned.'
        return, 0
        end
    end

for rb_i=0,rb_template.fieldcount-1 do begin
;
;   Check field name syntax.
;
    if rb_template.names[rb_i] eq '' then begin
        msg = 'Invalid template: names[' $
            + strcompress(rb_i, /remove_all) $
            + '] is blank.'
        return, 0
        end
    if strpos(strupcase(rb_template.names[rb_i]), 'RB_') ne -1 $
    or strpos(strupcase(rb_template.names[rb_i]), 'BT_') ne -1 then begin
        msg = 'Invalid template: Invalid field name ' $
            + strupcase(rb_template.names[rb_i]) $
            + '. ("RB_" and "BT_" not allowed.)'
        return, 0
        end
    message, /reset ; clear !error_state
    if not execute(rb_template.names[rb_i] + ' = 0b', 1) then begin
        msg = [ $
            'Invalid template: field name ' $
                + strupcase(rb_template.names[rb_i]) $
                + ' is invalid.', $
            '(' + !error_state.msg + ')' $
            ]
        return, 0
        end
;
;   Check offset syntax.
;
    if strpos(strupcase(rb_template.offsets[rb_i]), 'RB_') ne -1 $
    or strpos(strupcase(rb_template.offsets[rb_i]), 'BT_') ne -1 then begin
        msg = 'Invalid template: offset for field ' $
            + strupcase(rb_template.names[rb_i]) $
            + ' is invalid. ("RB_" and "BT_" not allowed.)'
        return, 0
        end

    if rb_template.absoluteflags[rb_i] eq 0 then begin
        if strmid(rb_template.offsets[rb_i], 0, 1) ne '<' $
        and strmid(rb_template.offsets[rb_i], 0, 1) ne '>' then begin
            msg = 'Invalid template: relative offset should start with ' $
                + '">" or "<".'
            return, 0
            end
        end

    message, /reset ; clear !error_state
    void = execute( $
        'void = ' $
            + strmid( $
                rb_template.offsets[rb_i], $
                ([1, 0])[rb_template.absoluteflags[rb_i]] $
                ), $
        1 $
        )
    if !error_state.name eq 'IDL_M_BADSYNTAX' $
    or !error_state.name eq 'IDL_M_ILLOP' $
    or !error_state.name eq 'IDL_M_ILLCHAR' then begin
        msg = [ $
            'Invalid template: offset for field ' $
                + strupcase(rb_template.names[rb_i]) $
                + ' is invalid.', $
            '(' + !error_state.msg + ')' $
            ]
        return, 0
        end
;
;   Check dimensions syntax.
;
    if rb_template.numdims[rb_i] gt 0 then begin
        if max(strpos( $
            strupcase(rb_template.dimensions[rb_i, *]), $
            'RB_' $
            )) ne -1 $
        or max(strpos( $
            strupcase(rb_template.dimensions[rb_i, *]), $
            'BT_' $
            )) ne -1 $
        then begin
            msg = 'Invalid template: a dimension for field ' $
                + strupcase(rb_template.names[rb_i]) $
                + ' is invalid. ("RB_" and "BT_" not allowed.)'
            return, 0
            end
        message, /reset ; clear !error_state
        void = execute( $
            'void = ' $
                + rb_dim_str( $
                    rb_template.dimensions[rb_i, *], $
                    rb_template.numdims[rb_i] $
                    ), $
            1 $
            )
        if !error_state.name eq 'IDL_M_BADSYNTAX' $
        or !error_state.name eq 'IDL_M_ILLOP' $
        or !error_state.name eq 'IDL_M_ILLCHAR' then begin
            msg = [ $
                'Invalid template: dimensions for field ' $
                    + strupcase(rb_template.names[rb_i]) $
                    + ' are invalid.', $
                '(' + !error_state.msg + ')' $
                ]
            return, 0
            end
        end
;
;   Check verify value syntax.
;
    if rb_template.VerifyFlags[rb_i] eq 1 then begin
        if strpos(strupcase(rb_template.verifyvals[rb_i]), 'RB_') ne -1 $
        or strpos(strupcase(rb_template.verifyvals[rb_i]), 'BT_') ne -1 $
        then begin
            msg = 'Invalid template: verifyval expression for field ' $
                + strupcase(rb_template.names[rb_i]) $
                + ' is invalid. ("RB_" and "BT_" not allowed.)'
            return, 0
            end
        message, /reset ; clear !error_state
        void = execute('void = ' + rb_template.verifyvals[rb_i], 1)
        if !error_state.name eq 'IDL_M_BADSYNTAX' $
        or !error_state.name eq 'IDL_M_ILLOP' $
        or !error_state.name eq 'IDL_M_ILLCHAR' then begin
            msg = [ $
                'Invalid template: the verification value for field ' $
                    + strupcase(rb_template.names[rb_i]) $
                    + ' is invalid.', $
                '(' + !error_state.msg + ')' $
                ]
            return, 0
            end
        end
    end
;
;Check that specified field names are unique.
;
if n_elements( $
    uniq(strupcase(rb_template.names), sort(strupcase(rb_template.names))) $
    ) $
ne rb_template.fieldcount then begin
    msg = 'Invalid template: specified field names are not unique.'
    return, 0
    end
;
msg = 'Template is valid.'
return, 1
end

; dummy stub so that rb_routines can be compiled using RESOLVE_ROUTINE
pro rb_routines
	;COMPILE_OPT hidden
end
