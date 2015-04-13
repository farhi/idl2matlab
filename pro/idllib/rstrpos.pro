; $Id: rstrpos.pro,v 1.2 1993/10/06 16:50:03 doug Exp $

FUNCTION RSTRPOS, Expr, SubStr, Pos
;+
; NAME:
;       RSTRPOS
;
; PURPOSE:
;	This function finds the last occurrence of a substring within
;	an object string. If the substring is found in the expression,
;	RSTRPOS returns the character position of the match, otherwise
;	it returns -1.
;
; CATEGORY:
;	String processing.
;
; CALLING SEQUENCE:
;        Result = RSTRPOS(Expr, SubStr [, Pos])
;
; INPUTS:
;       Expr:	The expression string in which to search for the substring.
;	SubStr: The substring to search for.
;
; OPTIONAL INPUTS:
;	Pos:	The character position before which the search is bugun.
;	      	If Pos is omitted, the search begins at the last character
;	      	of Expr.
;
; OUTPUTS:
;        Returns the position of the substring, or -1 if the
;	 substring was not found within Expr.
;
; SIDE EFFECTS:
;        Unlike STRPOS, Expr and SubStr must be strings.
;
; EXAMPLE:
;	Expr = 'Holy smokes, Batman!'	; define the expression.
;	Where = RSTRPOS(Exp, 'smokes')	; find position.
;	Print, Where			; print position.
;		5			; substring begins at position 5
;					; (the sixth character).
;
; MODIFICATION HISTORY:
;        JWG, January, 1993
;-
	Len	= STRLEN(Expr)
	IF N_ELEMENTS(Pos) EQ 0 THEN Start=0 ELSE Start = Len - Pos

	;	Reverse the string
	RString	= REVERSE(BYTE(Expr))

	;	Reverse the substring

	RSubStr	= REVERSE(BYTE(SubStr))

	SubPos	= STRPOS(STRING(RString),STRING(RSubStr),Start)
	IF SubPos NE -1 THEN SubPos = Len - SubPos - STRLEN(SubStr)
	RETURN, SubPos
END
