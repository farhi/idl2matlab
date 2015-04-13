/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ElseCase = 258,
     ACOMMENT = 259,
     DIESE2 = 260,
     IDENTIFIER = 261,
     INTEGER = 262,
     REAL = 263,
     DECIMAL = 264,
     HEXADECIMAL = 265,
     OCTAL = 266,
     STRING = 267,
     End = 268,
     Begin = 269,
     If = 270,
     Then = 271,
     Of = 272,
     Or = 273,
     Case = 274,
     And = 275,
     LT = 276,
     LE = 277,
     GT = 278,
     GE = 279,
     EQ = 280,
     NE = 281,
     Pro = 282,
     Not = 283,
     SLASH = 284,
     Mod = 285,
     While = 286,
     Repeat = 287,
     For = 288,
     Endrep = 289,
     Endwhile = 290,
     Endfor = 291,
     Do = 292,
     Until = 293,
     Assign = 294,
     Else = 295,
     Endelse = 296,
     Endif = 297,
     Function = 298,
     Common = 299,
     Extra = 300,
     Ref_Extra = 301,
     Endcase = 302,
     Return = 303,
     DIESE = 304,
     XOR = 305,
     Catch = 306,
     Forward_function = 307,
     FLECHE = 308,
     INHERITS = 309,
     OBJECT = 310,
     COMPILE_OPT = 311,
     DIESE2EQ = 312,
     DIESEEQ = 313,
     TIMESEQ = 314,
     PLUSEQ = 315,
     MOINSEQ = 316,
     SLASHEQ = 317,
     MINEQ = 318,
     MAXEQ = 319,
     ANDEQ = 320,
     EQEQ = 321,
     GEEQ = 322,
     GTEQ = 323,
     LEEQ = 324,
     LTEQ = 325,
     MODEQ = 326,
     NEEQ = 327,
     OREQ = 328,
     XOREQ = 329,
     POWEREQ = 330,
     ANDSHORTCUT = 331,
     ORSHORTCUT = 332,
     TILDE = 333,
     PlusPlus = 334,
     MoinsMoins = 335,
     CR = 336,
     UUPLUS = 337,
     UUMINUS = 338
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 2068 of yacc.c  */
#line 127 "idl2matlab.y"

	PNode uNode;
	char uChar[256];
	int uInt;
	char uReal[256];



/* Line 2068 of yacc.c  */
#line 142 "idl2matlab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


