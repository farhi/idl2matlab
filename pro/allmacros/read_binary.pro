; $Id: read_binary.pro,v 1.18 2001/05/22 23:29:17 kschultz Exp $
;
; Copyright (c) 1996-2001, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       READ_BINARY
;
; PURPOSE:
;       Load contents of a binary file into IDL.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       result = READ_BINARY([file])
;
; INPUTS:
;       FILE: The filename or logical unit number of a file to be read.
;           If a logical unit number is supplied, it must be open
;           on a file for reading. If no FILE argument is supplied,
;           READ_BINARY will call DIALOG_PICKFILE to prompt the user to
;           select a file for reading.
;
; INPUT KEYWORD PARAMETERS:
;       TEMPLATE: A template structure describing the file to be read.
;           A template can be created using BINARY_TEMPLATE.
;
;           Keyword TEMPLATE cannot be used simultaneously with keywords
;           DATA_START, HEADER, DATA_TYPE, DATA_DIMS or ENDIAN.
;
;       DATA_START: Where to begin reading in a file.  This value is
;           as an offset, in bytes, that will be applied to the
;           initial position in the file.  Default is 0.
;
;       DATA_TYPE: IDL typecode of the data to be read.  See
;           documentation for the IDL SIZE command for a listing
;           of typecodes.  Default is 1 (IDL's BYTE typecode).
;
;       DATA_DIMS: A scalar, or array of up to eight elements specifying
;           the size of the data to be read and returned.  For example,
;           DATA_DIMS=[512,512] specifies that a 2D, 512 by 512 array be
;           read and returned.  DATA_DIMS=0 specifies that a single,
;           scalar value be read and returned.  Default is -1, which,
;           if a TEMPLATE is not supplied that specifies otherwise,
;           indicates that READ_BINARY will read to end-of-file and
;           store the result in a 1D array.
;
;       ENDIAN: 'big', 'little' or 'native'.  Specifies the byte ordering
;           of the file to be read.  If the computer running Read_Binary
;           uses byte ordering that is different than that of the file,
;           Read_Binary will swap the order of bytes in multi-byte
;           data types read from the file.  Default: 'native' == perform
;           no byte swapping.
;
; OUTPUTS:
;       Function Read_Binary returns data read from the specified file.
;       If keyword TEMPLATE is used, Read_Binary returns a structure with
;       fields specified by the template.
;
; SIDE EFFECTS:
;       If a logical unit number is given for the file argument, the
;       current position of a file opened for reading on that logical
;       unit number is advanced.
;
; RESTRICTIONS:
;       Note: variables used in this routine are prefixed with "rb_".
;       This is to avoid conflicts with template-specified expressions
;       or field names.  Templates having field names, offset expressions
;       dimension expressions or verify value expressions containing
;       the character sequence "rb_" or "bt_" are not allowed.
;
;       READ_BINARY does not have functionality to read strings, but
;       strings can be read as an arrays of bytes, and then converted
;       via IDL's STRING command.
;
; EXAMPLES:
;
;       To select a file and read all of it as a simple, "raw" vector
;       of bytes...
;
;           result = READ_BINARY()
;
;       To read 149600 bytes from a file, and display as an image...
;
;           datafile = FILEPATH('hurric.dat', SUBDIR=['examples', 'data'])
;           TVSCL, READ_BINARY(datafile, DATA_DIMS=[440, 340])
;
;       or...
;
;           GET_LUN, lun
;           OPENR, lun, FILEPATH('hurric.dat', SUBDIR=['examples', 'data'])
;           TVSCL, REFORM(READ_BINARY(lun), 440, 340)
;           CLOSE, lun
;           FREE_LUN, lun
;
; MODIFICATION HISTORY
;       PCS, 6/1999 - Written.
;
;-
;
@rb_routines

function read_binary, $
    rb_file, $
    template=rb_template, $
    data_start=rb_data_start, $
;   header=rb_header, $
    data_type=rb_data_type, $
    data_dims=rb_data_dims, $
    endian=rb_endian, $
    debug=rb_debug

on_error, 2 ; Return to caller on error.
if keyword_set(rb_debug) then $
    on_error, 0

catch, rb_error_status
if rb_error_status ne 0 then begin
    catch, /cancel
    if keyword_set(rb_free_lun_on_cleanup) then $
        free_lun, rb_lun
    message, !error_state.msg
    end
if keyword_set(rb_debug) then $
    catch, /cancel

if !version.os_family eq 'vms' then $
    message, 'Read_Binary is not supported on VMS.', /noname
;
;Validate keywords.
;
if n_elements(rb_template) gt 0 or arg_present(rb_template) then begin
    if n_elements(rb_data_start) gt 0 then $
        message, $
            'DATA_START and TEMPLATE keywords cannot be used ' + $
                'simultaneously.', $
            /noname
;   if n_elements(rb_header) gt 0 then $
;       message, $
;           'HEADER and TEMPLATE keywords cannot be used ' + $
;               'simultaneously.', $
;           /noname
    if n_elements(rb_data_type) gt 0 then $
        message, $
            'DATA_TYPE and TEMPLATE keywords cannot be used ' + $
                'simultaneously.', $
            /noname
    if n_elements(rb_data_dims) gt 0 then $
        message, $
            'DATA_DIMS and TEMPLATE keywords cannot be used ' + $
                'simultaneously.', $
            /noname
    if n_elements(rb_endian) gt 0 then $
        message, $
            'ENDIAN and TEMPLATE keywords cannot be used ' + $
                'simultaneously.', $
            /noname

    if not rb_template_is_valid(rb_template, msg=rb_msg) then begin
        message, rb_msg[0], /noname
        end
    end

if n_elements(rb_data_start) gt 0 then begin
    if n_elements(rb_data_start) gt 1 then $
        message, 'DATA_START must be a scalar.', /noname
    if not rb_is_integral(rb_data_start) then $
        message, 'DATA_START is not an integer.', /noname
    end

if n_elements(rb_data_type) gt 0 then begin
    if n_elements(rb_data_type) gt 1 then $
        message, 'DATA_TYPE must be a scalar.', /noname
    if not rb_is_integral(rb_data_type) then $
        message, 'DATA_TYPE should be an integer value.', /noname

    case rb_data_type of
        0: message, 'DATA_TYPE is undefined type.', /noname
        1: rb_byte_size = 1   ; byte
        2: rb_byte_size = 2   ; int
        3: rb_byte_size = 4   ; long
        4: rb_byte_size = 4   ; float
        5: rb_byte_size = 8   ; double
        6: rb_byte_size = 8   ; complex
        7: message, $
            'Reading strings via READ_BINARY is not supported. ' + $
                '(Specify an array of bytes instead.)', $
            /noname
        8: message, $
            'Reading strucures via READ_BINARY is not supported.', $
            /noname
        9: rb_byte_size = 16  ; dcomplex
        10: message, $
            'Reading pointers via READ_BINARY is not supported.', $
            /noname
        11: message, $
            'Reading object references via READ_BINARY is not ' + $
                'supported.', $
            /noname
        12: rb_byte_size = 2   ; uint
        13: rb_byte_size = 4   ; ulong
        14: rb_byte_size = 8   ; long64
        15: rb_byte_size = 8   ; ulong64
        else: message, 'DATA_TYPE must be less than 16.', /noname
        endcase
    end

if n_elements(rb_data_dims) gt 0 then begin
    if n_elements(rb_data_dims) gt 8 then $
        message, 'DATA_DIMS must have 8 or less values.', /noname
    if not rb_is_integral(rb_data_dims) then $
        message, 'DATA_DIMS must be integer(s).', /noname
    if rb_data_dims[0] ne -1 then begin ; -1 == use the default
        if min(rb_data_dims) lt 0 then $
            message, 'Invalid DATA_DIMS.', /noname
        end
    end

if n_elements(rb_endian) gt 0 then begin
    if n_elements(rb_endian) gt 1 then $
        message, 'ENDIAN cannot be an array.', /noname
    if size(rb_endian, /tname) ne 'STRING' then $
        message, 'Endian must be a string.', /noname
    if strupcase(rb_endian) ne 'NATIVE' $
    and strupcase(rb_endian) ne 'LITTLE' $
    and strupcase(rb_endian) ne 'BIG' then $
        message, 'ENDIAN must be "native", "little" or "big."', /noname
    end
;
;Obtain a valid file.
;
rb_tname = size(rb_file, /tname)
if rb_tname eq 'UNDEFINED' then begin
    if arg_present(rb_file) then $
        message, 'Filename argument is undefined.', /noname $
    else begin
        rb_file = dialog_pickfile( $
            /must_exist, $
            /read $
            )
        if rb_file eq '' then $
            message, 'No file was selected for reading.', /noname
        rb_tname = 'STRING'
        end
    end

if rb_tname eq 'POINTER' $
or rb_tname eq 'STRUCT' $
or rb_tname eq 'OBJREF' $
or rb_tname eq 'COMPLEX' $
or rb_tname eq 'DCOMPLEX' then $
    message, $
        'First argument must be a file name or a logical unit number.', $
        /noname

if rb_tname eq 'STRING' then begin
    if rb_file eq '' then $
        message, 'The given file name is an empty string.'
    rb_void = findfile(rb_file, count=rb_count)
    if rb_count eq 0 then $
        message, 'Could not find file: ' + rb_file, /noname
    get_lun, rb_lun
    rb_free_lun_on_cleanup = 1b
    openr, rb_lun, rb_file, error=rb_error
    if rb_error ne 0 then $
        message, 'Unable to open ' + rb_file + ' for reading.', /noname
    rb_file_status = fstat(rb_lun)
    end $
else begin
    rb_lun = rb_file
    rb_file_status = fstat(rb_lun)
    case 1 of
        rb_file_status.open eq 0: $
            message, $
                'The supplied Logical Unit has not been Opened.', $
                /noname
        rb_file_status.isatty: $
            message, $
                'The supplied Logical Unit is a terminal ("TTY").', $
                /noname
        rb_file_status.isagui: $
            message, 'The supplied Logical Unit is not a file.', /noname
        rb_file_status.read eq 0: $
            message, $
                'The supplied Logical Unit is not open for read access.', $
                /noname
        else:
        endcase
    end
if rb_file_status.size le 0 then $
    message, 'The file to be read has zero length.', /noname
;
;Obtain a template.
;
if n_elements(rb_template) gt 0 then begin
    rb_template_use = rb_template
    rb_return_structure = 1b
    end $
else begin
;
;   Default template: specify one big byte vector into which the file
;   will be read.
;
    rb_fstat = fstat(rb_lun)
    rb_template_use = { $
        version: 1.0, $
        endian: 'native', $
        fieldcount: 1, $
        typecodes: 1, $ ; byte
        names: 'rb_result', $
        offsets: '>0', $
        numdims: 1, $
        dimensions: strcompress( $
            transpose([rb_fstat.size - rb_fstat.cur_ptr, intarr(7)]) $
            ), $
        reverseflags: transpose(intarr(8)), $
        absoluteflags: 0, $
        returnflags: 1, $
        verifyflags: 0 $
        }
;
;   Override defaults with any pertinant specified keywords.
;
    if n_elements(rb_data_start) gt 0 then begin
        if rb_fstat.size - rb_data_start le 0 then $
            message, 'DATA_START is at or beyond the end of file.', /noname
        rb_template_use.Dimensions[0] = $
            strcompress(rb_fstat.size - rb_data_start, /remove_all)
        rb_template_use.Offsets = $
            strcompress(rb_data_start, /remove_all)
        rb_template_use.absoluteflags = 1
        end

    if n_elements(rb_data_type) gt 0 then begin
        rb_template_use.Typecodes = rb_data_type

        if n_elements(rb_data_start) gt 0 then begin
            rb_template_use.Dimensions[0] = strcompress( $
                ((rb_fstat.size - rb_data_start) / rb_byte_size) > 1, $
                /remove_all $
                )
            end $
        else begin
            rb_template_use.Dimensions[0] = strcompress( $
                (rb_fstat.size / rb_byte_size) > 1, $
                /remove_all $
                )
            end
        end

    if n_elements(rb_data_dims) gt 0 then begin
        if rb_data_dims[0] ne -1 then begin ; -1 == use default.
            rb_template_use.Dimensions = strcompress(rb_data_dims)

            rb_template_use.NumDims = n_elements(rb_data_dims)
            for rb_i=n_elements(rb_data_dims)-1,0,-1 do begin
                if rb_data_dims[rb_i] le 0 then $
                    rb_template_use.NumDims = rb_i
                end
            end
        end

    if n_elements(rb_endian) gt 0 then begin
        rb_template_use.endian = rb_endian
        end

    rb_return_structure = 0b
    end
;
;Template has been obtained and validated. Proceed....
;
rb_varnames = strcompress(rb_template_use.names, /remove_all)
rb_machine_endian = (['BIG', 'LITTLE'])[(byte(1, 0, 2))[0] eq 1b]
for rb_i=0,rb_template_use.fieldcount-1 do begin
;
;   Declare variable.
;
    rb_str = rb_varnames[rb_i]
    if rb_template_use.numdims[rb_i] eq 0 then begin ; Scalar.
        rb_str = rb_str + ' = fix(0, type=rb_template_use.typecodes[rb_i])'
        end $
    else begin ; Array
        rb_str = rb_str $
            + ' = make_array(' $
            + 'dimension=' $
            +   rb_dim_str( $
                    rb_template_use.dimensions[rb_i, *], $
                    rb_template_use.numdims[rb_i] $
                    ) $
            +   ', ' $
            + '/nozero, ' $
            + 'type=rb_template_use.typecodes[rb_i]' $
            + ')'
        end
    if not execute(rb_str, 1) then begin
        print, 'Error executing string: ' + rb_str
        message, !error_state.msg, /noname
        end
;
;   Move to the appropriate file position.
;
    if rb_template_use.absoluteflags[rb_i] eq 1 then begin
        rb_str = 'point_lun, rb_lun, ' $
            + rb_template_use.offsets[rb_i]

        if not execute(rb_str, 1) then begin
            print, 'Error executing string: ' + rb_str
            message, !error_state.msg, /noname
            end
        end $
    else begin
        point_lun, -rb_lun, rb_pos ; Get the current position.
        if strpos(rb_template_use.offsets[rb_i], '>') eq 0 then begin
            rb_str = 'point_lun, rb_lun, rb_pos + ' $
                + strmid(rb_template_use.offsets[rb_i], 1)
            if not execute(rb_str, 1) then begin
                print, 'Error executing string: ' + rb_str
                message, !error_state.msg, /noname
                end
            end $
        else begin
            rb_str = 'point_lun, rb_lun, (rb_pos - ' $
                + strmid(rb_template_use.offsets[rb_i], 1) $
                + ') > 0'
            if not execute(rb_str, 1) then begin
                print, 'Error executing string: ' + rb_str
                message, !error_state.msg, /noname
                end
            end
        end
;
;   Read the declared variable.
;
    rb_str = 'readu, rb_lun, ' + rb_varnames[rb_i]
    if not execute(rb_str, 1) then begin
        print, 'Error executing string: ' + rb_str
        message, !error_state.msg, /noname
        end
;
;   Swap endian-ness.
;
    if rb_template_use.typecodes[rb_i] gt 1 then begin
        if ( $
            strupcase(rb_template_use.endian) eq 'LITTLE' $
            and $
            rb_machine_endian eq 'BIG' $
            ) $
        or ( $
            strupcase(rb_template_use.endian) eq 'BIG' $
            and $
            rb_machine_endian eq 'LITTLE' $
            ) $
        then begin
            rb_str = rb_varnames[rb_i] $
                + ' = swap_endian(' $
                + rb_varnames[rb_i] $
                + ')'
            if not execute(rb_str, 1) then begin
                print, 'Error executing string: ' + rb_str
                message, !error_state.msg, /noname
                end
            end
        end
;
;   Verify the value we read.
;
    if rb_template_use.VerifyFlags[rb_i] then begin
        rb_str = 'rb_verify_val = ' + rb_template_use.verifyvals[rb_i]
        message, /reset
        if not execute(rb_str, 1) then begin
            message, $
                'Could not determine the verification value for field ' $
                    + strupcase(rb_varnames[rb_i]) $
                    + '. ' $
                    + !error_state.msg $ ; e.g. syntax error.
                    + ' ' $
                    + !error_state.sys_msg, $ ; e.g. we ran out of memory.
                /noname
            end
        rb_str = 'rb_test = ' $
            + rb_varnames[rb_i] $
            + ' eq rb_verify_val'
        if not execute(rb_str, 1) then begin
            print, 'Error executing string: ' + rb_str
            message, !error_state.msg, /noname
            end
        if rb_test eq 0 then begin
            message, $
                'Value read from file did not pass verification: ' $
                    + strupcase(rb_varnames[rb_i]) $
                    + ' does not equal ' $
                    + strtrim(rb_template_use.verifyvals[rb_i], 2) $
                    + '.', $
                /noname
            end
        end
;
;   Reverse in the first three dimensions of the variable, if desired.
;   (The IDL REVERSE command can only operate on the first
;   three dimensions of an array.)
;
    for rb_j=0,2<(rb_template_use.NumDims[rb_i]-1) do begin
        if rb_template_use.reverseflags[rb_i, rb_j] eq 1 then begin
            rb_str = rb_varnames[rb_i] $
                + ' = reverse(temporary(' $
                + rb_varnames[rb_i] $
                + '), rb_j + 1)'
            if not execute(rb_str, 1) then begin
                print, 'Error executing string: ' + rb_str
                message, !error_state.msg, /noname
                end
            end
        end
    end
if keyword_set(rb_free_lun_on_cleanup) then $
    free_lun, rb_lun

if rb_return_structure then begin
;
;   Put the fields into a structure.
;
    rb_returns_indx = where(rb_template_use.ReturnFlags ne 0)
    rb_str = 'rb_result = create_struct(' $
        + 'rb_varnames[rb_returns_indx[0]], ' $
        + 'temporary(' + rb_varnames[rb_returns_indx[0]] + ')' $
        + ')'
    if not execute(rb_str, 1) then begin
        print, 'Error executing string: ' + rb_str
        message, !error_state.msg, /noname
        end
    for rb_i=1,n_elements(rb_returns_indx)-1 do begin
        rb_str = 'rb_result = create_struct(' $
            + 'temporary(rb_result), ' $
            + 'rb_varnames[rb_returns_indx[rb_i]], temporary(' $
            + rb_varnames[rb_returns_indx[rb_i]] $
            + '))'
        if not execute(rb_str, 1) then begin
            print, 'Error executing string: ' + rb_str
            message, !error_state.msg, /noname
            end
        end
    end

return, rb_result
end
