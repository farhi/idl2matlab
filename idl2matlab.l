%{
/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*   
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Syntax description of the IDL Language
*   Auteurs             :       Azizi Mourier Karim
                                Benzeghioua Abdeslam
                                Gardon Lucien
                                Sylvestre Nadege
*   Date                :       February 21 2002
*   Modifications       :       
*                               
*****************************************************************************/

extern int nb_lines ;
#include <stdio.h>
#include <stdlib.h>
#include "type.h"
#include "y.tab.h"

char* lexError=NULL; 



%}

space 		[ \t]
bl 		{space}+
digit 		[0-9]
character 	[a-zA-Z]
hexa 		[a-fA-F]
oct		[0-7]
digit_char 	({digit}|{character}|"$"|"_")
real  		({digit}*"."(({digit}+([eEdD][-+]?({digit})+)?)|({digit}+[eEdD]))|{digit}+"."(({digit}*([eEdD][-+]?({digit})*)?)|({digit}+[eEdD])))
exposant  	{digit}+[eEdD][-+]?{digit}*
integer  	{digit}+
identifier 	[_]*{character}{digit_char}*
hexadecimal	[']({digit}|{hexa})+['][Xx][Uu]?([Bb]|[Ll]|[lL][lL])?
octal		((["]{oct}+)|([']{oct}+['][Oo]))[uU]?([Ll]|[Bb]|[lL][lL])?
decimal		{digit}+[Uu]?([Bb]|[lL]|[sS]|[lL][lL])
sep 		{bl}
strings		((['][^'\n]*['])*|(["][^"\n]*["])*)

/*------------------------------*/
%%
{sep} ;			/* saute les blancs et tabulations */
;.*   {addComment(yytext, nb_lines);}

[fF][Oo][Rr][Ww][Aa][Rr][Dd][_][Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]  {return(Forward_function);}
[cC][oO][mM][pP][iI][lL][eE][_][Oo][pP][tT]    	{return(COMPILE_OPT);}
[bB][eE][gG][iI][nN] 			{return(Begin);}
[eE][nN][dD] 				{return(End);}
[iI][fF]	 			{return(If);}
[tT][hH][eE][nN] 			{return(Then);}
[oO][fF]	 			{return(Of);}
[cC][aA][sS][eE] 			{return(Case);}
[oO][rR]	 			{return(Or);}
[aA][nN][dD]	 			{return(And);}
[lL][tT]	 			{return(LT);}
[gG][tT]	 			{return(GT);}
[lL][eE]	 			{return(LE);}
[gG][eE]	 			{return(GE);}
[eE][qQ]	 			{return(EQ);}
[nN][eE]	 			{return(NE);}
[pP][rR][oO]	 			{return(Pro);}
[nN][oO][tT]	 			{return(Not);}
[mM][oO][dD]	 			{return(Mod);}
[wW][hH][iI][lL][eE] 			{return(While);}
[rR][eE][pP][eE][aA][tT]		{return(Repeat);}
[fF][oO][rR]		 		{return(For);}
[eE][nN][dD][rR][eE][pP]		{return(Endrep);}
[eE][nN][dD][wW][hH][iI][lL][eE]	{return(Endwhile);}
[eE][nN][dD][fF][oO][rR]		{return(Endfor);}
[dD][oO]				{return(Do);}
[uU][nN][tT][iI][lL]			{return(Until);}
[eE][nN][dD][eE][lL][sS][eE]		{return(Endelse);}
[eE][lL][sS][eE]			{return(Else);}
[eE][nN][dD][iI][fF]			{return(Endif);}
[fF][uU][nN][cC][tT][iI][oO][nN]	{return(Function);}
[cC][oO][mM][mM][oO][nN]		{return(Common);}
[_][eE][xX][tT][rR][aA]			{return(Extra);}
[_][rR][eE][fF][_][eE][xX][tT][rR][aA]  {return(Ref_Extra);}
[eE][nN][dD][cC][aA][sS][eE] 		{return(Endcase);}
[rR][eE][tT][uU][rR][nN]		{return(Return);}
[cC][aA][tT][cC][hH]			{return(Catch);}
[xX][oO][rR]  	      	      	      	{return(XOR);}
"##"        	      	      	      	{return(DIESE2);}
[iI][nN][hH][eE][rR][iI][tT][sS]      	{return(INHERITS);}

"+" {return('+');}
"-" {return('-');}
"*" {return('*');}
"/" {return(SLASH);}
"=" {return('=');}
"," {return(',');}
"{" {return('{');}
"}" {return('}');}
"(" {return('(');}
"$".*\n{sep}?"(" {nb_lines++;return('(');}  /* start of line times */
"$".*\n{sep}?"*" {nb_lines++;return('*');}  /* start of line times */
\n{sep}?"*" {nb_lines++;return(SOL_TIMES);}  /* start of line times */
\n{sep}?"(" {nb_lines++;return(SOL_PARENT);}  /* start of line parenthese */
"&"{sep}?"*" {return(SOL_TIMES);}  /* start of line times */
"&"{sep}?"(" {return(SOL_PARENT);}  /* start of line parenthese */
")" {return(')');}
"[" {return('[');}
"]" {return(']');}
":" {return(':');}
"." {return('.');}
"!" {return('!');}
"#" {return(DIESE);}
">" {return('>');}
"<" {return('<');}
"^" {return('^');}
"->" {return(FLECHE);}
"?" {return('?');}
\n   {nb_lines++;}	/* incremente nb_lines sur un CR */
&     ;			/* ignore & */
\$    ;


{identifier}  	{strcpy(yylval.uChar,yytext);return (IDENTIFIER);}
{integer} 	{yylval.uInt=atoi(yytext);return(INTEGER);}
{exposant}	{strcpy(yylval.uReal,yytext);return(REAL);}
{real} 		{strcpy(yylval.uReal,yytext);return(REAL);}
{decimal} 	{strcpy(yylval.uChar,yytext);return (DECIMAL);}
{hexadecimal} 	{strcpy(yylval.uChar,yytext);return (HEXADECIMAL);}
{octal} 	{strcpy(yylval.uChar,yytext);return (OCTAL);}
{strings} 	{strcpy(yylval.uChar,yytext);return (STRING);}


. {    
   if (lexError!=NULL) {
     lexError = (char*) realloc (lexError, strlen(lexError)+strlen(yytext)+40);
     sprintf(lexError,"%slexical error (line %d) : %s\n",lexError,nb_lines,yytext);
   } else {
     lexError = (char*) malloc (strlen(yytext)+40);
     sprintf(lexError,"lexical error (line %d) : %s\n",nb_lines,yytext);
   }
  }

%%
int yywrap()
{
        return(1);  	/* par defaut ... */
}

int addComment(char* str, int nLine) {
  Comment *commentTmp;

  commentTmp = (Comment*) malloc (sizeof(Comment));
  commentTmp->commentString = (char*) malloc (strlen(str)+1);
  strcpy(commentTmp->commentString, str);
  commentTmp->lineInSource = nLine;
  commentTmp->nextComment = NULL;

  if (lastComment == NULL) {
    commentTable = commentTmp;
  } else {
    lastComment->nextComment = commentTmp;
  }
  lastComment = commentTmp;
  return(1);
}
