; $Id: psafm.pro,v 1.2 1993/10/18 16:21:56 doug Exp $

;+
; NAME:
;	PSAFM
;
; PURPOSE:
;	Given an Abobe Font metric file, this procedure generates an AFM
;	file in the format that IDL likes. This new file differs from the
;	original in the following ways:
;
;		[] Information not used by IDL is removed.
;		[] AFM files with the AdobeStandardEncoding are
;		   supplemented with an ISOLatin1Encoding.
;
; CATEGORY:
;	Misc., PostScript, Fonts
;
; CALLING SEQUENCE:
;	PSAFM, input_filename, output_filename
;
; INPUTS:
;	Input_Filename:	 Name of existing AFM file from Adobe.
;	Output_FIlename: Name of new AFM file to be created.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Generates an output file.
;
; MODIFICATION HISTORY:
;	8 January 1993, Written by AB, RSI.
;-
;
;

function PSAFM_ISOMAP
; Generate a table mapping character index to font name. Unmapped indices are
; represented by a null string.

  iso_map = strarr(256)

  ; These assignments were taken from the "PostScript Language Reference
  ; Manual" Second edition, Section E.5, "Standard Roman Character Set".
  ; To make trouble shooting easier, the assignments are in the same order as
  ; the book. Comparing the results of these assignments to the table in
  ; Section 7, "ISOLatin1Encoding Vector" reveals a few missing characters.
  ; These were derived by comparing the graphic in the twop sections. These
  ; extra assignments are at the end.
  iso_map('101'o) = 'A'
  iso_map('306'o) = 'AE'
  iso_map('301'o) = 'Aacute'
  iso_map('302'o) = 'Acircumflex'
  iso_map('304'o) = 'Adieresis'
  iso_map('300'o) = 'Agrave'
  iso_map('305'o) = 'Aring'
  iso_map('303'o) = 'Atilde'
  iso_map('102'o) = 'B'
  iso_map('103'o) = 'C'
  iso_map('307'o) = 'Ccedilla'
  iso_map('104'o) = 'D'
  iso_map('105'o) = 'E'
  iso_map('311'o) = 'Eacute'
  iso_map('312'o) = 'Ecircumflex'
  iso_map('313'o) = 'Edieresis'
  iso_map('310'o) = 'Egrave'
  iso_map('320'o) = 'Eth'
  iso_map('106'o) = 'F'
  iso_map('107'o) = 'G'
  iso_map('110'o) = 'H'
  iso_map('111'o) = 'I'
  iso_map('315'o) = 'Iacute'
  iso_map('316'o) = 'Icircumflex'
  iso_map('317'o) = 'Idieresis'
  iso_map('314'o) = 'Igrave'
  iso_map('112'o) = 'J'
  iso_map('113'o) = 'K'
  iso_map('114'o) = 'L'
  iso_map('115'o) = 'M'
  iso_map('116'o) = 'N'
  iso_map('321'o) = 'Ntilde'
  iso_map('117'o) = 'O'
  iso_map('323'o) = 'Oacute'
  iso_map('324'o) = 'Ocircumflex'
  iso_map('326'o) = 'Odieresis'
  iso_map('322'o) = 'Ograve'
  iso_map('330'o) = 'Oslash'
  iso_map('325'o) = 'Otilde'
  iso_map('120'o) = 'P'
  iso_map('121'o) = 'Q'
  iso_map('122'o) = 'R'
  iso_map('123'o) = 'S'
  iso_map('124'o) = 'T'
  iso_map('336'o) = 'Thorn'
  iso_map('125'o) = 'U'
  iso_map('332'o) = 'Uacute'
  iso_map('333'o) = 'Ucircumflex'
  iso_map('334'o) = 'Udieresis'
  iso_map('331'o) = 'Ugrave'
  iso_map('126'o) = 'V'
  iso_map('127'o) = 'W'
  iso_map('130'o) = 'X'
  iso_map('131'o) = 'Y'
  iso_map('335'o) = 'Yacute'
  iso_map('132'o) = 'Z'
  iso_map('141'o) = 'a'
  iso_map('341'o) = 'aacute'
  iso_map('342'o) = 'acircumflex'
  iso_map('222'o) = 'acute'
  iso_map('264'o) = 'acute'
  iso_map('344'o) = 'adieresis'
  iso_map('346'o) = 'ae'
  iso_map('340'o) = 'agrave'
  iso_map('46'o) = 'ampersand'
  iso_map('345'o) = 'aring'
  iso_map('136'o) = 'asciicircum'
  iso_map('176'o) = 'asciitilde'
  iso_map('52'o) = 'asterisk'
  iso_map('100'o) = 'at'
  iso_map('343'o) = 'atilde'
  iso_map('142'o) = 'b'
  iso_map('134'o) = 'backslash'
  iso_map('174'o) = 'bar'
  iso_map('173'o) = 'braceleft'
  iso_map('175'o) = 'braceright'
  iso_map('133'o) = 'bracketleft'
  iso_map('135'o) = 'bracketright'
  iso_map('226'o) = 'breve'
  iso_map('246'o) = 'brokenbar'
  iso_map('143'o) = 'c'
  iso_map('237'o) = 'caron'
  iso_map('347'o) = 'ccedilla'
  iso_map('270'o) = 'cedilla'
  iso_map('242'o) = 'cent'
  iso_map('223'o) = 'circumflex'
  iso_map('72'o) = 'colon'
  iso_map('54'o) = 'comma'
  iso_map('251'o) = 'copyright'
  iso_map('244'o) = 'currency'
  iso_map('144'o) = 'd'
  iso_map('260'o) = 'degree'
  iso_map('250'o) = 'dieresis'
  iso_map('367'o) = 'divide'
  iso_map('44'o) = 'dollar'
  iso_map('227'o) = 'dotaccent'
  iso_map('220'o) = 'dotlessi'
  iso_map('145'o) = 'e'
  iso_map('351'o) = 'eacute'
  iso_map('352'o) = 'ecircumflex'
  iso_map('353'o) = 'edieresis'
  iso_map('350'o) = 'egrave'
  iso_map('70'o) = 'eight'
  iso_map('75'o) = 'equal'
  iso_map('360'o) = 'eth'
  iso_map('41'o) = 'exclam'
  iso_map('241'o) = 'exclamdown'
  iso_map('146'o) = 'f'
  iso_map('65'o) = 'five'
  iso_map('64'o) = 'four'
  iso_map('147'o) = 'g'
  iso_map('337'o) = 'germandbls'
  iso_map('221'o) = 'grave'
  iso_map('76'o) = 'greater'
  iso_map('253'o) = 'guillemotleft'
  iso_map('273'o) = 'guillemotright'
  iso_map('150'o) = 'h'
  iso_map('235'o) = 'hungarumlaut'
  iso_map('255'o) = 'hyphen'
  iso_map('151'o) = 'i'
  iso_map('355'o) = 'iacute'
  iso_map('356'o) = 'icircumflex'
  iso_map('357'o) = 'idieresis'
  iso_map('354'o) = 'igrave'
  iso_map('152'o) = 'j'
  iso_map('153'o) = 'k'
  iso_map('154'o) = 'l'
  iso_map('74'o) = 'less'
  iso_map('254'o) = 'logicalnot'
  iso_map('155'o) = 'm'
  iso_map('257'o) = 'macron'
  iso_map('55'o) = 'minus'
  iso_map('265'o) = 'mu'
  iso_map('327'o) = 'multiply'
  iso_map('156'o) = 'n'
  iso_map('71'o) = 'nine'
  iso_map('361'o) = 'ntilde'
  iso_map('43'o) = 'numbersign'
  iso_map('157'o) = 'o'
  iso_map('363'o) = 'oacute'
  iso_map('364'o) = 'ocircumflex'
  iso_map('366'o) = 'odieresis'
  iso_map('236'o) = 'ogonek'
  iso_map('362'o) = 'ograve'
  iso_map('61'o) = 'one'
  iso_map('275'o) = 'onehalf'
  iso_map('274'o) = 'onequarter'
  iso_map('271'o) = 'onesuperior'
  iso_map('252'o) = 'ordfeminine'
  iso_map('272'o) = 'ordmasculine'
  iso_map('370'o) = 'oslash'
  iso_map('365'o) = 'otilde'
  iso_map('160'o) = 'p'
  iso_map('266'o) = 'paragraph'
  iso_map('50'o) = 'parenleft'
  iso_map('51'o) = 'parenright'
  iso_map('45'o) = 'percent'
  iso_map('56'o) = 'period'
  iso_map('267'o) = 'periodcentered'
  iso_map('53'o) = 'plus'
  iso_map('261'o) = 'plusminus'
  iso_map('161'o) = 'q'
  iso_map('77'o) = 'question'
  iso_map('277'o) = 'questiondown'
  iso_map('42'o) = 'quotedbl'
  iso_map('140'o) = 'quoteleft'
  iso_map('47'o) = 'quoteright'
  iso_map('162'o) = 'r'
  iso_map('256'o) = 'registered'
  iso_map('232'o) = 'ring'
  iso_map('163'o) = 's'
  iso_map('247'o) = 'section'
  iso_map('73'o) = 'semicolon'
  iso_map('67'o) = 'seven'
  iso_map('66'o) = 'six'
  iso_map('57'o) = 'slash'
  iso_map('40'o) = 'space'
  iso_map('243'o) = 'sterling'
  iso_map('164'o) = 't'
  iso_map('376'o) = 'thorn'
  iso_map('63'o) = 'three'
  iso_map('276'o) = 'threequarters'
  iso_map('263'o) = 'threesuperior'
  iso_map('224'o) = 'tilde'
  iso_map('62'o) = 'two'
  iso_map('262'o) = 'twosuperior'
  iso_map('165'o) = 'u'
  iso_map('372'o) = 'uacute'
  iso_map('373'o) = 'ucircumflex'
  iso_map('374'o) = 'udieresis'
  iso_map('371'o) = 'ugrave'
  iso_map('137'o) = 'underscore'
  iso_map('166'o) = 'v'
  iso_map('167'o) = 'w'
  iso_map('170'o) = 'x'
  iso_map('171'o) = 'y'
  iso_map('375'o) = 'yacute'
  iso_map('377'o) = 'ydieresis'
  iso_map('245'o) = 'yen'
  iso_map('172'o) = 'z'
  iso_map('60'o) = 'zero'

  ; Extra assignments from Section 7.
  iso_map('225'o) = 'macron'
  iso_map('230'o) = 'dieresis'
  iso_map('233'o) = 'cedilla'
  iso_map('240'o) = 'space'

  return, iso_map
end







PRO PSAFM, input_file, output_file
;  on_error, 2		; Return to caller if an error occurs
  do_iso_encoding = 0
  iso_map = PSAFM_ISOMAP()
  iso_lines = strarr(256)
  message,/INFO,'Processing ' + input_file


  openr, in, input_file, /GET_LUN
  openw, out, output_file, /GET_LUN

  printf,out,'Comment This file was generated by PSAFM.PRO for use by IDL'
  printf,out,'Comment on ', systime(0), '. It may not work properly with'
  printf,out,'Comment other software because it does not contain the complete'
  printf,out,'Comment contents of the original file and some features are different.'
  printf, out, 'Comment'

  line = ''
  while not eof(in) do begin
    readf, in, line
    token = strcompress(line)
    i = strpos(line, ' ')
    if (i ne -1) then token = strmid(token, 0, i)
    case token of
	'Comment': do_verbatim = ((strpos(line, 'Copyright') ne -1) $
		                  or (strpos(line, 'Creation') ne -1))
	'EncodingScheme': begin
		do_verbatim = 1
		if (strpos(line,'AdobeStandardEncoding') ne -1) then $
		    do_iso_encoding = 1
		end
	'FontName': do_verbatim =1
	'FullName': do_verbatim =1
        'StartCharMetrics': do_verbatim =1
	'EndCharMetrics': do_verbatim =1
	'StartFontMetrics': do_verbatim = 1
	'C': begin
	    do_verbatim = long(strmid(line, 2, 10000)) ne -1
	    ; Get name and match against ISO mapping.
	    i = strpos(line, '; N ')
	    if (i ne -1) then begin
	      name = strmid(line, i+4, 1000)
	      i = strpos(name, ' ;')
	      if (i ne -1) then name = strmid(name, 0, i)
	      tmp = where(iso_map eq name, i)
	      for count=0,i-1 do begin
		iso_lines(tmp(count)) = $
		  string(tmp(count),strmid(line, strpos(line, ';'), 1000), $
			format='("C ", I0, X, A)')
	      endfor
	    endif
	    end
        else: do_verbatim = 0
    endcase
    if (do_verbatim) then printf, out, line
  endwhile

  if (do_iso_encoding) then begin
    printf, out, 'Comment'
    printf, out, 'Comment A real Adobe AFM file wouldn''t have a second'
    printf, out, 'Comment encoding here. However, having this reduces the'
    printf, out, 'Comment runtime load when switching fonts.'
    printf, out, 'Comment'
    printf, out, 'EncodingScheme ISOLatin1Encoding'
    printf, out, 'StartCharMetrics'
    for i = 0, 255 do begin
	if (iso_lines(i) ne '') then begin
	    printf, out, iso_lines(i)
	endif else if (iso_map(i) ne '') then begin
	    print,format='("    Unmapped ISO character: ", A, "(",O0,")")', $
		iso_map(i), i
	endif
    endfor
    printf, out, 'EndCharMetrics'
  endif

  printf, out, 'EndFontMetrics'

  FREE_LUN, in, out
end
