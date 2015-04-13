; $Id: xdistfile.pro,v 1.4 1995/08/04 15:40:40 idl Exp $

; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO XDistfile, FILENAME, SUBDIRECTORY, _EXTRA=extra
;+
; NAME: 
;	XDISTFILE
;
; PURPOSE:
;	Displays ASCII text files from the IDL distribution. Unlike
;	XDISPLAYFILE, this routine understands that under VMS, IDL
;	routines are packed in VMS text libraries.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISTFILE, Filename, Subdirectory
;
; INPUTS:
;     Filename:	A scalar string that contains the filename of the file
;		to display NOT INCLUDING the '.pro' extension or any
;		path information.
;     Subdirectory: Subdirectory information in the style of the
;		FILEPATH user library routine.
;
; KEYWORD PARAMETERS:
;	Any keywords allowed by XDISPLAYFILE are also allowed.
;
; OUTPUTS:
;	No explicit outputs.  A file viewing widget is created.
;
; SIDE EFFECTS:
;	Triggers the XMANAGER if it is not already in use.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	This is a thin wrapper over the XDISPLAYFILE routine.
;	In the case of VMS text libraries, the last element of the
;	SUBDIRECTORY argument is taken to be the TLB file name, and
;	FILENAME is actually the module name in that library.
;
; MODIFICATION HISTORY:
;	1 June 1994, AB
;-

  if (!version.os eq 'vms') then begin
    n = n_elements(subdirectory)
    subn = n
    if (subn lt 1) then message,'SUBDIRECTORY doesn''t contain enough elements.'
 
    module = subdirectory(n-1)
    subd = subdirectory(0:n-subn)
    n=n-1

    library=filepath(module+'.tlb', SUBDIRECTORY=subd)
    ;
    ; WARNING:
    ;
    ;	READ_VMSTLB is undocumented and likely to disappear or change
    ;   radiacally in future IDL releases. Use at your own risk.
    ;
    text = READ_VMSTLB(library, filename, count=c)
    if (c ne 0) then begin
      title = library + '  (' + STRUPCASE(filename) + '.PRO)'
      XDISPLAYFILE, TEXT=TEXT, title=title, _extra=extra
      return
    endif
  endif


  XDISPLAYFILE, FILEPATH(filename + '.pro', SUBDIRECTORY=SUBDIRECTORY), $
			 _extra=extra

END
