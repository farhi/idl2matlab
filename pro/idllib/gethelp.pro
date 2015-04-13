; $Id: gethelp.pro,v 1.2 1994/04/28 21:46:47 idl Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;
;+
; NAME:
;	 GETHELP
;
; PURPOSE:
;	This function is used to get information on variables in the 
;	routine that called this function. The function builts a string array
;	that contains information that follows the format that is used
;	by the IDL HELP command. 
;
; CATEGORY:
;	Help
;
; CALLING SEQUENCE:
;	Result = GetHelp([Vname])
;
; INPUTS:
;	Vname:	Optional parameter that contains the name of a variable
;		the user wants information about.
;
; KEYWORD PARAMETERS:
;	ONELINE:	If a variable name is greater than 15 charaters
;			it is usally returned as 2 two elements of the
;			output array (Variable name in 1st element, 
;			variable info in the 2nd element). Setting this 
;			keyword will put all the information in one string,
;			seperating the name and  data with a space.
;
;	FULLSTRING:	Normally a string that is longer than 45 chars
;			is truncated and followed by "..." just like 
;			the HELP command. Setting this keyword will cause
;			the full string to be returned.
;
;	PROCEDURES:	Setting this keyword will cause the function
;			to return all current IDL compiled procedures.
;
;	FUNCTIONS:	Setting this keyword will cause the function
;			to return all current IDL compiled functions.
;
;	SYS_PROCS:	Setting this keyword will cause the function
;			to return the names of all IDL system (built-in)
;			procedures.
;
;	SYS_FUNCS:	Setting this keyword will cause the function
;			to return the names of all IDL system (built-in)
;			functions.
;
; OUTPUTS:
;	This function returns a string array that normally contains
;	variable data that is in the same format as used by the IDL HELP
;	procedure. The variables in this list are for the routine that
;	called GetHelp(). If other info is requested via keywords, this
;	data is returned.
;
;	Upon an error or if no data is found the function returns an 
;	Null ('') string. 
;
; COMMON BLOCKS:
;	None
;
; SIDE EFFECTS:
;	None
;
; RESTRICTIONS:
;	Due to the diffuculties in determining if a variable is of type
;	associate, the following conditions will result in the variable 
;	being listed as a structure. These conditions are:
;
;	 	o Associate record type is structure.
;		o Associated file is opened for update (openu).
;		o Associate file is not empty.
;
;       Another difference between this routine and the IDL help command
;	is that if a variable is in a common block, the common block name 
;	is not listed next to the variable name. Currently there is no
;	method available to get the common block names used in a routine.
;
; PROCEDURE:
;	This function uses the IDL routine Routine_Names() to get the 
;	names and values of the variables contained in the calling 
;	routine. These values are then placed in a string array using
;	the format of the IDL HELP command. If there are no variables
;	in the calling routine, a null ('') scalar string is returned.
;
; EXAMPLE:
;	To obtain a listing in a help "format" of the variables contained
;	in the current routine you would make the following call:
;
;		HelpData = GetHelp()
;
;	The variable HelpData would be a string array containing the 
;	requested information.
;
; MODIFICATION HISTORY:
;	Initial Coding  April 1994	- KDB
;
;-
;
;=============================================================================

   FUNCTION IS_ASSOC, Unit

; PURPOSE:
;	This function is used to determine if an IDL variable is a
;	Assoc() file variable. This function is needed since there is no
;	built in method for a program to determine if a variable is an
;	associated variable.
;
; OPERATION:
;	This functions depends on several properities of an associate 
;	variable to determine if a variable is one. These properties are:
;
;	   o Size indicates that an associate variable is an array
;	   o N_Elements always returns one for an associate variable
;	   o ON_IOERROR will trap I/O errors that happen during associate
;	     file operations.
;	   o The following command will result in a scalar for a normal
;	     array, an array for an associate variable:
;		 DUM = Var(0)
;	  
; RESTRICTIONS:
;	Due to the diffuculties in determining if a variable is of type
;	associate, the following conditions will result in this function 
;	returning a false. These conditions are:
;
;	 	o Associate record type is structure.
;		o Associated file is opened for update (openu).
;		o Associate file is not empty.
;
; Start by getting the size of the input parameter

  UnitSize = Size(Unit)

; All Assocs are arrays and N_Elements() only returns 1. Check if this 
; variable is a scalar or N_elements() > 1.

  if((UnitSize(0) eq 0)or(N_Elements(Unit) gt 1))then $
     Return, 0  ; no need to continue

; Set up Error handling for input/ouput. This will catch any errors I/O we
; cause with associate variables. This function depends on this trapping.

  On_IOerror, IOERR

; Set the first element of Unit equal to itself. If Unit is an array we
; will continue processing, but if Unit is an associate variable and the 
; file is not open for "update" we will trigger an error which is trapped
; by on_ioerror.

  Unit(0) = Unit(0)

; A single diminsion array would of passed the above test, see if we
; can get the first value of Unit.

  DumVal = Unit(0)
  
; Get the Size of DumVal

  DumSize = Size(DumVal)

; If DumVal is a scalar (So Unit was a 1 dim array) or if DumVal is a 
; struct (so Unit is a struct or assoc to a struct record opened for update)
; return a false.

  if((DumSize(0) eq 0)or(DumSize(N_Elements(DumSize)-2) eq 8))then $
    Return, 0

; The only type of variable that should get here is an associate variable.
; Get the file information for this variable

   UnitStat = Fstat(Unit)

; Make sure that the file is not a tty or has no name

  if((UnitStat.IsAtty eq 1)or(UnitStat.Name eq ''))then  $
       return,0     ;not an assoc

; Anything that gets below this point is an associate variable 

IOERR:

  Return, 1  ; it is an assoc() variable

END

;=============================================================================

 FUNCTION MakeHelpString, Vname, Vvalue, Vsize, ASSOC = ASSOC, $
	 		  ONELINE=ONELINE, FULLSTRING=FULLSTRING

; PURPOSE:
;	This function takes information about a variable and creates
;	a string that contains this information following the IDL HELP
;	format.
;
; INPUTS:
;	Vname:		The name of the Variable
;	Vvalue:		The actual value of the variable
;	Vsize;		The results of the Size() function on Vvalue
;
; KEYWORD PARAMETERS:
;	ASSOC:		Indicates that the variable is an Associate 
;			variable
;	
;	ONELINE:	If a variable name is greater than 15 charaters
;			it is usally returned as 2 a two element array 
;			(Variable name in 1st elements, Variable info
;			in the 2nd element). Setting this keyword
;			will put all the information in one string.
;
;	FULLSTRING:	Normally a string that is longer than 45 chars
;			is truncated and followed by "..." just like 
;			the HELP command. Setting this keyword will cause
;			the full string to be returned.
;
; OUTPUTS:
;	This function returns a string(s) that is in a HELP command format
;	for the given information. A 2 element string array is returned if
;	the Variable name (Vname) is longer that 15 characters and the
;	ONELINE keyword is not set.
;
; Declare the TypeTokens

  TypeTokens = [ 'UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', $
		 'DOUBLE', 'COMPLEX', 'STRING', 'STRUCT']

  Vtype = Vsize(N_Elements(Vsize)-2) ; Get the type of variable

; See if the variable is not an array (a Scalar or an Undef).

  if( Vsize(0) eq 0 )then BEGIN
   
  ; If the Value is a string we need to put '' around it and add ... to it
  ; if it is longer than 45 chars

    if(Vtype eq 7)then BEGIN

       if( (StrLen(Vvalue) gt 45)and(not Keyword_Set(FULLSTRING)))then  $
	   ValueField = "'" + StrMid(Vvalue, 0, 45) + "'..."  	        $
       else		$
	   ValueField = "'" + Vvalue +"'"

    ENDif else if(Vtype eq 1)then  $  
    ;  have a byte, cant just string it, use integer format code 

       ValueField = String(Vvalue, FORMAT='(I4)')	$

    else	$   ;not a string or a byte, just do a String() to the value

       ValueField = String(Vvalue)      

  ENDif else     $                           
  ;  We have an array, convert the diminsions of the array to strings
  ;  The format statement has a repeat value of 20. Should only need up
  ;  to 8 (the max number of dimensions, but doesnt hurt to have a little
  ;  extra just incase this changes later on.

     ValueField= String( StrCompress(Vsize(1:Vsize(0)), /REMOVE_ALL), $
		        FORMAT = '("Array(",20(a, :, ", ") )' ) +")"

; Now check out the Assoc() variable possibility

  if(Keyword_Set(ASSOC))then BEGIN

  ;  Need to get the filename associated with the variable. Fstat() it.

     StatStruc = Fstat(Vvalue)
  
  ;  An Assoc() can be done on a file that is not open. See if the 
  ;  name is null. If file is closed (name =''), put in HELP closed message

     if(StatStruc.Name eq '')then             $
	StatStruc.Name = "Closed file unit "+ $
		StrCompress(StatStruc.Unit, /REMOVE_ALL)

     ValueField = "File<"+StatStruc.Name+"> "+ValueField

  ENDif

; Is the data a structure?

  if(Vtype eq 8)then 				$
  ;  Add the struct symbol and name to Value Field
     
     ValueField = "-> " + Tag_Names(Vvalue, /STRUCTURE_NAME)+ " " + $
	          ValueField
    
; Now lets build the line and return it. Check for long identifiers

  if( StrLen(Vname) gt 15)then BEGIN
  ;  We have a long indentifier. See if the user want the line on one line
  ;  or 2
   
     if(Keyword_Set(ONELINE))then 				$
	Return, Vname + String(TypeTokens(Vtype), ValueField, 	$
			FORMAT='(" ", A, T11,"= ", A)' )  	$
     else							$
	Return, [Vname, String(TypeTokens(Vtype), ValueField,   $
			FORMAT='(T16," ", A,T27,"= ", A)' )] 
  ENDif 

; Variable name must be < 15 chars. Just return the old fashioned type 
; of listing

  Return, String(Vname, TypeTokens(Vtype), ValueField,$
		 FORMAT= '(A,T16," ", A,T27,"= ", A)' )

END
;=============================================================================

  FUNCTION Gethelp, Uservar, ONELINE=ONELINE, PROCEDURES=PROCEDURES, $
		   FUNCTIONS = FUNCTIONS, SYS_PROCS=SYS_PROCS,       $
		   SYS_FUNCS=SYS_FUNCS, FULLSTRING=FULLSTRING

; PURPOSE:
;	This function is used to get information on variables in the 
;	routine that called this function. The function builts a string array
;	that contains information that follows the format that is 
;	printed out by the IDL HELP command. 
;
; CALLING SEQUENCE:
;	Result = GetHelp([Vname])
;
; INPUTS:
;	Vname:	Optional parameter that contains the name of a variable
;		the user wants information about.
;
; KEYWORD PARAMETERS:
;	ONELINE:	If a variable name is greater than 15 charaters
;			it is usally returned as 2 two elements of the
;			output array (Variable name in 1st elements, 
;			Variable info in the 2nd element). Setting this 
;			keyword will put all the information in one string,
;			seperating the name and  data with a space.
;
;	FULLSTRING:	Normally a string that is longer than 45 chars
;			is truncated and followed by "..." just like 
;			the HELP command. Setting this keyword will cause
;			the full string to be returned.
;
;	PROCEDURES:	Setting this keyword will cause the function
;			to return all current IDL compiled procedures.
;
;	FUNCTIONS:	Setting this keyword will cause the function
;			to return all current IDL compiled functions.
;
;	SYS_PROCS:	Setting this keyword will cause the function
;			to return the names of all IDL system (built-in)
;			procedures.
;
;	SYS_FUNCS:	Setting this keyword will cause the function
;			to return the names of all IDL system (built-in)
;			functions.
;
; OUTPUTS:
;	This function returns a string array that normally contains
;	variable data that is in the same format as used by the IDL HELP
;	procedure. The variables in this list are for the routine that
;	called GetHelp(). If other info is requested via keywords, this
;	data is returned.
;
;	Upon an error the function returns an Null ('') string. 
;
;
; See if the user just wants the names of routines or functions:

  if(Keyword_Set(PROCEDURES))then  $
     Return, Routine_Names(PROCEDURES=-1)
 
  if(Keyword_Set(FUNCTIONS))then   $
     Return, Routine_Names(FUNCTIONS=-1 )

; See if the user wants the system procedure names

  if(Keyword_Set(SYS_PROCS))then   $
     Return, Routine_Names(S_PROCEDURES=-1)

; And now check for system functions

  if(Keyword_Set(SYS_FUNCS))then   $
     Return, Routine_Names(S_FUNCTIONS=-1)

; Start by getting the variable names for the calling procedure

  VarNames = Routine_Names(VARIABLES=-1)

; See if we got any variables. 

  NumVars = N_Elements(VarNames) ; number of variables 

  if( NumVars eq 1)then $
     if(VarNames(0) eq '')then $
	Return,''		;No Variables, Why continue?

; Check for a parameter

  if(N_Params() eq 1)then BEGIN

  ; The user wants the help value for just one variable, See if it 
  ; is in the list of valid variables.

    Vindx = Where(StrUpcase(UserVar) eq VarNames, Vcnt)

    if(Vcnt eq 1)then BEGIN

    ; Just place this variable in place of the found variables

      VarNames = VarNames(Vindx)
      NumVars = 1

    ENDif else Return,''  ; not a valid variable

  ENDif

; Initialize the value of OutText. Check for long parameters.
; For each long parameter ( >15 chars), add a line to the OutText Array. 
; Only check this is if keyword ONELINE is set.

  LongCnt = 0
  if( not KeyWord_Set(ONELINE))then $
      tmp = Where(StrLen(VarNames) gt 15, LongCnt)
  
  OutText = Strarr(NumVars + LongCnt )
  OutCnt = 0 & tmp = 0

; Now loop though each variable, get its value, format its help string and
; place the string into the OutText array.

  for i=0, NumVars-1 do BEGIN

  ;  Get the *size* of the variable value. This will also handle udefs.

     VarSize = Size(Routine_Names( VarNames(i), FETCH=-1) )

  ;  If the variable is not of type undefined, get its value

     if( VarSize(N_Elements(VarSize)-2) ne 0 )then 		$ ;not undef
	VarValue = Routine_Names( VarNames(i), FETCH=-1 ) 	$
     else 							$
	VarValue = '<Undefined>'   ;From help output

  ;  See if the variable is an Assoc() variable. If the variable is 
  ;  an array or struct it might be.

     if( VarSize(0) gt 0 )then        $  ;not undefined and not a scalar
        ASSOC = IS_ASSOC( VarValue )  $  ; ? is it an Assoc()
     else		              $
        ASSOC = 0	;no an associate variable

  ;  Now send the variable name, value and size to MakeHelpString

     TmpTxt = MakeHelpString(VarNames(i), VarValue, VarSize, $
	      ASSOC=ASSOC, ONELINE=ONELINE, FULLSTRING=FULLSTRING ) 

  ;  Place the results in OutText
  ;  Could use [ ] to append, but with large arrays this can be slow.

     OutText(OutCnt) = TmpTxt   ; overload index if need be (TmpTxt is 2 el.)
     OutCnt = OutCnt + N_Elements(TmpTxt)

  ENDfor

; That should be it, return the array.

  Return, OutText

END


















