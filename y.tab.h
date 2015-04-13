/* A Bison parser, made by GNU Bison 2.0.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

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
#define ElseCase 258
#define ACOMMENT 259
#define DIESE2 260
#define IDENTIFIER 261
#define INTEGER 262
#define REAL 263
#define DECIMAL 264
#define HEXADECIMAL 265
#define OCTAL 266
#define STRING 267
#define End 268
#define Begin 269
#define If 270
#define Then 271
#define Of 272
#define Or 273
#define Case 274
#define And 275
#define LT 276
#define LE 277
#define GT 278
#define GE 279
#define EQ 280
#define NE 281
#define Pro 282
#define Not 283
#define SLASH 284
#define Mod 285
#define While 286
#define Repeat 287
#define For 288
#define Endrep 289
#define Endwhile 290
#define Endfor 291
#define Do 292
#define Until 293
#define Assign 294
#define Else 295
#define Endelse 296
#define Endif 297
#define Function 298
#define Common 299
#define Extra 300
#define Ref_Extra 301
#define Endcase 302
#define Return 303
#define DIESE 304
#define XOR 305
#define Catch 306
#define Forward_function 307
#define FLECHE 308
#define INHERITS 309
#define OBJECT 310
#define COMPILE_OPT 311
#define DIESE2EQ 312
#define DIESEEQ 313
#define TIMESEQ 314
#define PLUSEQ 315
#define MOINSEQ 316
#define SLASHEQ 317
#define MINEQ 318
#define MAXEQ 319
#define ANDEQ 320
#define EQEQ 321
#define GEEQ 322
#define GTEQ 323
#define LEEQ 324
#define LTEQ 325
#define MODEQ 326
#define NEEQ 327
#define OREQ 328
#define XOREQ 329
#define POWEREQ 330
#define ANDSHORTCUT 331
#define ORSHORTCUT 332
#define TILDE 333
#define PlusPlus 334
#define MoinsMoins 335
#define CR 336
#define UUPLUS 337
#define UUMINUS 338




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
#line 127 "idl2matlab.y"
typedef union YYSTYPE {
	PNode uNode;
	char uChar[256];
	int uInt;
	char uReal[256];
} YYSTYPE;
/* Line 1318 of yacc.c.  */
#line 210 "y.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;



