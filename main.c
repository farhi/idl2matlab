/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Base de connaissance
*   Auteurs             :       Gardon Lucien
*                               Sylvestre Nadege
                                Cortina Stephane
                                Bourtembourg Reynald
*   Date creation       :       02 / 04 / 2002
*   Modification        :       25 / 08 / 2003
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <time.h>   /* pour le calcul de temps de traduction */
#include "code.h"
#include "hashtable.h"
#include "table_symb.h"
#include "type.h"

/* definition de l'extension ds fichiers de sorties */
#define OUTPUT_EXTENSION ".m"
#define OUTPUT_SCI_EXTENSION ".sci"

/* variables externes */

extern char* lexError;
  /* erreurs leve par lex */
extern FILE *yyin ;
  /* entree de yacc - on lui donne le fichier source */

int nb_lines=1 ;         	   /*+ le numero de ligne dans le source, incremente dans le lex  +*/
char outDir[256];  	           /*+ nom du repertoire cible +*/
char logFileName[256];   	   /*+ nom du fichier log +*/
char sourceFileName[256];      /*+ nom du fichier source +*/
char outFileName[256];         /*+ nom du fichier cible +*/
char sourceDirName[256];       /*+ repertoire source +*/
char* stringTranslationResult; /*+ chaine contenant la traduction +*/
int hashTableSize = 256;       /*+ Taille de la hashtable +*/
char oneFunctionName[256];     /*+ Nom de lafonction a traduire +*/

char strTmp2[256];

/*******************************/
/* fonctions locales au module */
/*******************************/



/* ouvre ou creer un fichier en ecriture et ecrit la chaine de caractere dedans */
int openFile(char *pfileName, char *pstrLine)
{
  /* la fonction renvoie 0 si tout se passe bien et -1 sinon */
  FILE *file;
  file = fopen(pfileName, "a+"); /* open or create a file for appending */

  if(file==NULL) {
    return -1;
  }
  else {
    fputs(pstrLine, file);
  }
  fclose(file);
  return 0;
}

/* affiche un message d'erreur en cas de mauvaise utilisation */
static void usage () {
  printf ("Usage : idl2matlab [-options] inputFile [outputDirectory]\n") ;
  printf ("  IDL2Matlab %c version %s\n", PATHSEP, I2M_VERSION_2);
  printf ("Action: translate IDL source code into Matlab\n");
  printf ("\nOptions :\n") ;
  printf ("  -s for script files translation\n") ;
  printf ("  -S for string translation\n");
  printf ("  -q to hide messages (quiet mode)\n") ;
  printf ("  -w to stop warnings writing\n");
  printf ("  -t to print the abstract tree (long output)\n");
  printf ("  -V to get idl2Matlab version\n");
  printf ("  -A to get idl2Matlab authors\n");
  printf ("  -Tx to get x spaces for a tabulation x<10\n");
  printf ("  -f to translate only one function\n");
  printf ("  -C to translate in Scilab (default : Matlab)\n");
  printf ("\n");
  printf ("The log of the translation is written to file 'idl2matlab.log' and\n");
  printf ("  the translation returns 0 when no error is found\n");
  printf ("The IDL2MATLAB environment variable can be set to specify where\n");
  printf ("  the translation library is installed.\n"); 
  printf ("  IDL2MATLAB=%s\n", getenv("IDL2MATLAB") ? getenv("IDL2MATLAB") : IDL2MATLAB );
  printf ("\n");
  printf ("Examples :\n") ;
  printf ("  idl2matlab -qw essai.pro outDir/\n") ;
  printf ("  idl2matlab -t essai.pro outDir/ > out\n") ;
  printf ("  idl2matlab -S \"print,\'IDL2Matlab\'\"\n") ;
  printf ("  idl2matlab -T4 essai.pro outDir/\n") ;
  printf ("  idl2matlab -f essai.pro functionName outDir/ > out\n") ;
  printf ("  idl2matlab -C essai.pro outDir/\n") ;

  exit (0) ;
}

/* test si le fichier peut etre ouvert en lecture et l ouvre par la meme occasion */
/* On donne a yacc le fichier source par l'intermediaire de yyin */
static void  check (char *s)
{
   yyin = fopen (s, "r") ; /* ouverture du fichier passe en argument */
   if (yyin == NULL) {
   	fprintf (stderr,
		"Cannot open %s\n", s) ;
	exit (1) ;  /* si probleme on arrete tout */
   } ;
}


/* met la chaine en minuscule */
void strToLower(char* str) {
  int i;

  i = 0;
  while (str[i] != '\0') {
    str[i] = tolower(str[i]);
    i++;
  }
}

/* Cherche un caractere dans une chaine */
/* renvoie 1 si s contient c / 0 sinon */
int containsChar(char* s, char c) {
  int result;

  result= 0;
  while ((*s != '\0') && (result == 0)) {
    /* on parcours la chaine s */
    if (*s == c) {  /* on trouve le caratere c dans s */
      result = 1;
    }
    s++;
  }
  return result;
}

/* procedure d'initialisation des parametres */
/* Elle prend en entree la chaine passee en parametre a la ligne de commande */
/* On teste les options voulues de l'utilisateur */
void initParam(char* s) {
  /* On test la presence de chaque lettre d'option */
  if (containsChar(s,'V')==1) {
    /* Cas de l'affichage de la version */
    fprintf(stderr,"#####################################\n");
    fprintf(stderr,"## IDL2Matlab %c version %s ##\n", PATHSEP, I2M_VERSION_2);
    fprintf(stderr,"#####################################\n");
  }
  if ((containsChar(s,'a')==1) || (containsChar(s,'A') == 1)) {
    /* Cas de l'affichage des auteurs */
    fprintf(stderr,"############################################\n");
    fprintf(stderr,"## IDL2Matlab - version %s        ##\n", I2M_VERSION_2);
    fprintf(stderr,"##   Project by D. RICHARD and E.FARHI    ##\n");
    fprintf(stderr,"##   (c) ILL, DS/CS 2001-2013             ##\n");
    fprintf(stderr,"##                                        ##\n");
    fprintf(stderr,"## Coders: initial version                ##\n");
    fprintf(stderr,"##        AZIZI MOURIER Karim             ##\n");
    fprintf(stderr,"##       BENZEGHIOUA  ABDESLAM            ##\n");
    fprintf(stderr,"##           GARDON Lucien                ##\n");
    fprintf(stderr,"##         SYLVESTRE  Nadege              ##\n");
    fprintf(stderr,"##                                        ##\n");
    fprintf(stderr,"##        BOURTEMBOURG Reynald            ##\n");
    fprintf(stderr,"##          CORTINA Stephane              ##\n");
    fprintf(stderr,"##         SZCZUCZAK  Nadege              ##\n");
    fprintf(stderr,"############################################\n");
  }

  if (containsChar(s,'f')==1) {
    oneFunctionTranslation = 1;    	/* traduction d une fonction */
  }
  if (containsChar(s,'T')==1) {
    if (containsChar(s,'0')==1) {tabVal = 0;}
    if (containsChar(s,'1')==1) {tabVal = 1;}
    if (containsChar(s,'2')==1) {tabVal = 2;}
    if (containsChar(s,'3')==1) {tabVal = 3;}
    if (containsChar(s,'4')==1) {tabVal = 4;}
    if (containsChar(s,'5')==1) {tabVal = 5;}
    if (containsChar(s,'6')==1) {tabVal = 6;}
    if (containsChar(s,'7')==1) {tabVal = 7;}
    if (containsChar(s,'8')==1) {tabVal = 8;}
    if (containsChar(s,'9')==1) {tabVal = 9;}
  }
  if (containsChar(s,'S')==1) {
    stringTranslation = 1;    	      	/* traduction de chaine */
  }
  if (containsChar(s,'s')==1) {
    scriptFileTranslation = 1;        	/* traduction de script */
  }
  if (containsChar(s,'h')==1) {
    usage ();        	/* help */
  }
  if (containsChar(s,'q')==1) {
    displayMessage = 0;    	      	/* affichage des messages si = 1 sinon 0 */
  }
  if (containsChar(s,'w')==1) {
    writeWarning = 0 ;     	      	/* ecrit les warning dans le fichier log si = 1 */
  }
  if (containsChar(s,'t')==1) {
    writeAbstractTree = 1;	      	/* affiche l'arbre abstrait si = 1 */
  }
	if (containsChar(s,'C')==1) {
    inScilabTranslation = 1;	      	/* traduction en scilab si = 1 */
    commentaire="//";
  }
}


/* renvoie le nom du fichier cible avec son extension */
static char* getTargetFileName (char *s) {
  char *result=NULL; /* variable stockant le resultat */
  int i = strlen(s); /* index parcourant le nom du fichier */
  int ok = 0;        /* boolean indiquant quand on a trouve le point */

  while ((i>0) && (ok ==0)) {
    /* on cherche la position du . */
    if (s[i]=='.') {
      ok =1;
    }
    i--;
  }
	if (inScilabTranslation != 1) { /* traduction en matlab */
    if (ok == 1) {	/* si on a trouve le slash on renvoie le nom du fichier */
			int len = strlen(s) + 1;
    	/* Allocation de la m�oire */
    	result = (char*)malloc(len*sizeof(char)+1);
    	strncpy(result, s, i+1);
    	result[i+1]='\0';
    	strcat (result, OUTPUT_EXTENSION);
  	} else { /* sinon on met juste l'extension */
    	result = (char*)malloc((strlen(s)+2)*sizeof(char));
    	strcpy(result,s);
    	strcat(result, OUTPUT_EXTENSION);
			/* on  place l'extension .m a la fin du nom de fichier */
 		}
	}
	else { /* traduction en scilab */
		if (ok == 1) {	/* si on a trouve le slash on renvoie le nom du fichier */
			int len = strlen(s) + 3;
    	/* Allocation de la m�oire */
    	result = (char*)malloc(len*sizeof(char)+1);
    	strncpy(result, s, i+1);
    	result[i+1]='\0';
    	strcat (result, OUTPUT_SCI_EXTENSION);
  	} else { /* sinon on met juste l'extension */
    	result = (char*)malloc((strlen(s)+4)*sizeof(char));
    	strcpy(result,s);
    	strcat(result, OUTPUT_SCI_EXTENSION);
			/* on  place l'extension .sci a la fin du nom de fichier */
		}
 	}
  return result;
}

/* renvoie le nom du fichier cible sans ses repertoires */
static char* getFileName (char *s)
{
  char *result=NULL; /* variable stockant le resultat */
  int i = strlen(s); /* index parcourant le nom du fichier */
  int j;
	int len;
  int ok = 0;        /* boolean indiquant quand on a trouve le dernier slash */
  while ((i > 0) && (ok == 0)) {
    if (s[i]== PATHSEP) { /* on cherche le slash ou le \ */
      ok =1;
    }
    i--;
  }
  if (ok == 1) {	/* si on a trouve le slash on renvoie le nom du fichier */
		if (inScilabTranslation != 1) { /* traduction en matlab */
    	len = strlen(s) + 1;
		}
		else {
			len = strlen(s) + 3;
		}
		/* Allocation de la m�oire */
    result = (char*)malloc(len*sizeof(char)+1);
    j=-i-1;
    while (*s != '\0'){ /* on copie la fin de la chaine */
      if (j>0) {
      	result[j-1] = *s;
      }
      s++;
      j++;
    }
    result[j-1]='\0';
  } else {
		if (inScilabTranslation != 1) { /* traduction en matlab */
    	result = (char*)malloc((strlen(s)+2)*sizeof(char));
		}
		else {
			result = (char*)malloc((strlen(s)+4)*sizeof(char));
		}
    strcpy(result,s);
  }
  return result;
}


/* renvoie le repertoire du fichier */
static char* getDirName (char *s) {
  char *result=NULL; /* variable stockant le resultat */
  int i = strlen(s); /* index parcourant le nom du fichier */
  int ok = 0;        /* boolean indiquant quand on a trouve le slash */

  while ((i>0) && (ok ==0)) {
    if (s[i]== PATHSEP) {  /* on cherche le dernier slash ou le \ */
      ok =1;
    }
    i--;
  }
  if (ok == 1) {	/* si on a trouve le slash on renvoie le nom du repertoire */
    int len = strlen(s) + 1;
    /* Allocation de la m�oire */
    result = (char*)malloc(len*sizeof(char)+1);
    strncpy(result, s, i+1);
    result[i+1]='\0';
  } else { /* si pas de repertoire on renvoie le repertoire courant (.) */
    result = (char*)malloc(256);

    strcpy(result, getenv("IDL2MATLAB") ? getenv("IDL2MATLAB") : IDL2MATLAB);
  }
  return result;
}

/* Parse le fichier en entree - recursive */
void parseArobase(char* inputFile, FILE* pOutputFile) {
  FILE *pInputFile;
  char c, startOfLine, containPoint;
  char strTmp[256];
  int i;
  /* startOfLine indique si on est au debut d une instruction */

  pInputFile = fopen(inputFile, "r");
  if (pInputFile == NULL) {
    fprintf(stderr, "Unable to open input file \"%s\"\n", inputFile);
    exit(1);
  } else {
    c = fgetc(pInputFile);
    startOfLine = 1;
    while( !feof(pInputFile)) {
      if ((c == '$') || (c == '\n')) {
        startOfLine = 1;
        fputc(c, pOutputFile);
      } else if ((c == '@') && (startOfLine == 1)) { /* on trouve un arobase */
        i=0;
        containPoint = 0;
        while( (!feof(pInputFile)) && !((c == '\n') || (c == '$') || (c == ';'))) {
          c = fgetc(pInputFile);
          if (c == '.') {
            containPoint = 1;
          }
          if ((c != ' ') && (c != '\t')) {
            strTmp[i++] = c;
          }
        }
        i--;
        if (containPoint == 0) {
          strTmp[i++] = '.';
          strTmp[i++] = 'p';
          strTmp[i++] = 'r';
          strTmp[i++] = 'o';
        }
        strTmp[i] = '\0';
        sprintf(strTmp2, "%s%c%s", sourceDirName, PATHSEP,strTmp);
        parseArobase(strTmp2, pOutputFile);
        if ((c == '\n') || (c == '$')) {
          fputc(c, pOutputFile);
          startOfLine = 1;
        }
        if (c == ';') {
          fputs("\n;", pOutputFile);
        }
      } else { /* cas courant */
        if (c != ' ') {
          startOfLine = 0;
        }
        fputc(c, pOutputFile);
      }
      c = fgetc(pInputFile);
    }
  }
  fclose(pInputFile);
}



/* Parse le fichier source pour remplacer les appels @ par le code correspondant */
void scanArobase(char* inputFile) {
  FILE *pOutputFile;
  char strTmp[256];

  sprintf(strTmp, "%s%cidl2matlab_sourcefile.pro", outDir,PATHSEP);
  pOutputFile =fopen(strTmp, "w");
  if (pOutputFile == NULL) {
    fprintf(stderr, "Unable to open output file !\n");
    exit(1);
  } else {
    parseArobase(inputFile, pOutputFile);
    fclose(pOutputFile);
  }
}

/***********************/
/* fonctions exportees */
/***********************/

/* affiche un message d'erreur de syntaxe */
void yyerror(char *s) {
/* cette fonction est appelle par yacc en cas d'erreur de syntaxe */
  char *errorString;
  errorString = (char*) malloc (256*sizeof(char));
  sprintf(errorString, "%s at line %d\n", s, nb_lines);
  openFile (logFileName,errorString);
  free(errorString);
  translationError = 1;
}

/* programme principal */
int main (int argc, char *argv[]) {
  FILE *pFile;  /* pointeur permettant la creation des fichiers logs et cible */
  char strTmp[256];
  int i;
  time_t tnow;	/* variable de temps */

  time(&tnow);	/* on prend l'heure courante */

  /* initialisation des parametres au valeurs par defauts */
  scriptFileTranslation = 0;  	      	/* traduction normale */
  displayMessage = 1;    	            	/* affichage des messages si = 1 sinon 0 */
  writeWarning = 1 ;     	            	/* ecrit les warning dans le fichier log si = 1 */
  writeAbstractTree = 0;	            	/* affiche l'arbre abstrait si = 1 */
  stringTranslation = 0;      	      	/* mode normal */
  oneFunctionTranslation = 0; 	      	/* mode de traduction */
  tabVal = 2; 	      	      	      	/* indentation par defaut */
  inScilabTranslation = 0;      	      /* traduction en matlab */
  commentaire="%";

  switch (argc) { /* on agit selon les parametres */
    case 2 :
      if (argv[1][0]=='-') { /* passage d'option */
        initParam(argv[1]);
        return 0;
      } else {
        strcpy (outDir, ".");
        strcpy(sourceFileName,argv[1]);
        strcpy(outFileName,getFileName(argv[1]));
      }
      break;
    case 3 :
      if (argv[1][0]=='-') { /* passage d'option */
        initParam(argv[1]);
        if (stringTranslation == 0) {/* Cas normal */
          strcpy(sourceFileName, argv[2]);
          strcpy(outFileName,getFileName(argv[2]));
					if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
					}
					else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
					}
        } else {/* Traduction de chaine */
          sprintf(sourceFileName, "%s%cidl2matlab_string_tmp.pro", outDir, PATHSEP);
          pFile = fopen(sourceFileName, "w"); /* on ecrase le fichier */
          fclose(pFile);
          openFile(sourceFileName, argv[2]);
          strcpy(outFileName,getFileName(sourceFileName));
          if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
					}
					else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
					}
        }
        strcpy (outDir, ".");
      } else {
        strcpy (outDir, argv[2]);
        if (outDir[strlen(outDir)-1] == PATHSEP) {
          outDir[strlen(outDir)-1] = '\0';
        }
        strcpy(sourceFileName,argv[1]);
        strcpy(outFileName,getFileName(getTargetFileName(argv[1])));
      }
      break ;
    case 4 :
      if (argv[1][0]=='-') { /* passage d'option */
        initParam(argv[1]);
        if (oneFunctionTranslation == 1) {
          strcpy(sourceFileName,argv[2]);
          strcpy(outFileName, getFileName(argv[2]));
          if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
					}
					else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
					}
          strcpy(oneFunctionName, argv[3]);
          strcpy(outDir, ".");
        } else {
          if (stringTranslation == 0) {
            strcpy(sourceFileName,argv[2]);
            strcpy(outFileName, getFileName(argv[2]));
            if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
						}
						else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
						}
          } else {
            sprintf(sourceFileName, "%s%cidl2matlab_string_tmp.pro", outDir, PATHSEP);
            pFile = fopen(sourceFileName, "w"); /* on ecrase le fichier */
            fclose(pFile);
            openFile(sourceFileName, argv[2]);
            strcpy(outFileName,getFileName(sourceFileName));
            if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
						}
						else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
						}
          }
          strcpy(outDir, argv[3]);
          if (outDir[strlen(outDir)-1] == PATHSEP) {
            outDir[strlen(outDir)-1] = '\0';
          }
        }
      } else { /* les autres usages sont mauvais donc on appelle usage() */
        usage();
      }
      break ;
    case 5 :
      if (argv[1][0]=='-') { /* passage d'option */
        initParam(argv[1]);
        if (oneFunctionTranslation == 1) {
          strcpy(sourceFileName,argv[2]);
          strcpy(outFileName, getFileName(argv[2]));
          if (inScilabTranslation==0) { /* traduction en matlab */
					          strcat(outFileName, OUTPUT_EXTENSION);
					}
					else { /* traduction en scilab */
										strcat(outFileName, OUTPUT_SCI_EXTENSION);
					}
          strcpy(oneFunctionName, argv[3]);
          strcpy(outDir, argv[4]);
          if (outDir[strlen(outDir)-1] == PATHSEP) {
            outDir[strlen(outDir)-1] = '\0';
          }
        } else {
          usage();
        }
      } else {
        usage();
      }
      break;
    default:
    usage ();
  } ;

  /* initialisation du detecteur d erreur a 0 */
  translationError = 0;
  nbGeneratedFile = 0;
  nbWarning = 0;
  strcpy(sourceDirName, getDirName(sourceFileName));
  /* On stocke le repertoire lib */
  strcpy(i2mDirName, getenv("IDL2MATLAB") ? getenv("IDL2MATLAB") : IDL2MATLAB);

  strToLower(oneFunctionName);

  if (displayMessage == 1) {   /* on affiche l'heure de debut */
    fprintf (stderr,"Started at : %s",ctime(&tnow));
    fprintf (stderr,"Expecting idl2matlab library in %s%c\n", i2mDirName, PATHSEP);
  }

  /* creation des fichiers logs et cibles avec test d acces */
  if (writeWarning == 1) {
    sprintf(logFileName,"%s%cidl2matlab.log",outDir,PATHSEP);
  } else { /* sinon on ecrira les warnings dans un fichier log tmp qu on supprimera */
    sprintf(logFileName,"%s%cidl2matlabtmp.log",outDir,PATHSEP);
  }



  pFile = fopen(logFileName, "w"); /* on ecrase le fichier log d erreurs */
  if (pFile == NULL) {
    if (displayMessage == 1) {
      fprintf (stderr,"Unable to write in the log file\n") ;
    }    /* si on arrive pas ecrire dans le fichier log on arrete */
    exit(1);
  }

  /*************  on scan la source pour les arobases ***********/
  if (displayMessage==1) {  /* syntaxe OK */
    fprintf (stderr,"Parsing IDL code ...\n") ;
  }
  scanArobase(sourceFileName);
  sprintf(sourceFileName, "%s%cidl2matlab_sourcefile.pro", outDir,PATHSEP);

  /* On donne le source a Yacc */
  check (sourceFileName) ;

  /*************  on appelle le parseur de Yacc ***********/
  if (yyparse() == 0) {
    if (displayMessage==1) {  /* syntaxe OK */
      fprintf (stderr,"Syntax analysis OK ...\n") ;
    }
  }
  fclose (yyin) ;


/*  lastComment = commentTable;
  while (lastComment != NULL) {
    printf("%d - %s\n", lastComment->lineInSource, lastComment->commentString);
    lastComment = lastComment->nextComment;
  }*/

  if (stringTranslation == 1) {
#ifdef WIN32
      sprintf(strTmp, "del %s\\idl2matlab_string_tmp.pro", outDir);
#else
      sprintf(strTmp, "rm -rf %s/idl2matlab_string_tmp.pro", outDir);
#endif
      system(strTmp);
  }

  /* on test si lex a renvoye des erreurs */
  if ((lexError!=NULL) && (displayMessage == 1)) {
      fprintf (stderr,"Lexical Error !!! ...\n") ;
  }
  if ((lexError!=NULL) && (writeWarning == 1)) {
    translationError = 1; /* on ecrit les erreurs lexicales */
    openFile(logFileName,lexError);
    free(lexError);
  }
  /********************************************************/

  if ((displayMessage==1) && (translationError == 1)) {
    fprintf (stderr,"SYNTAX ERROR LINE %d!!!\n", nb_lines) ;
    exit(1);
  }


  /*************  Affichage de l'arbre abstrait ***********/
  if (displayMessage==1) {
    fprintf (stderr,"Abstract tree created\n") ;
  }
  if (writeAbstractTree==1) {
    printf("Arbre abstrait : \n");
    printTree(root);
    printf("Fin Arbre abstrait\n");
  }
  /********************************************************/

  /*************  s'occupe de la table des symb ***********/
  hashTable = (nodeType **) calloc(hashTableSize, sizeof(nodeType *));
  genererTableSymb();
  if (displayMessage==1) {
    fprintf (stderr,"Symbols Table properly generated\n") ;
  }
  /********************************************************/

  /************* chargement des mots reserves *************/
  loadReservedWord();
  /************* chargement des traductions *************/
  loadFunctionsTranlations();
  /********************************************************/

  if (displayMessage==1) {
    if (inScilabTranslation==0) { /* traduction en matlab */
					   fprintf (stderr,"Generating Matlab code\n") ;
		}
		else { /* traduction en scilab */
						fprintf (stderr,"Generating Scilab code\n") ;
		}
  }

  stringTranslationResult = NULL;
  if (stringTranslation == 1) {
    stringTranslationResult = (char*) malloc (10000);
    /* on alloue de la place pour la chaine resultat */
    if (displayMessage == 1) {
      fprintf (stderr,"Translation of %s\n", argv[2]) ;
    }
  }

  /********************* Generation de code ******************/
  genereCode(outDir,logFileName,sourceFileName, sourceDirName, outFileName, stringTranslationResult, oneFunctionName, inScilabTranslation);

  if ((translationError == 0) && (writeWarning == 1)) { /* aucune erreur detectee pendant la traduction */
    openFile(logFileName, "Traduction complete - No error\n");
  }

  if (displayMessage==1) {
    if (translationError == 0) {
      fprintf (stderr,"Translation OK - ") ;
    } else {
      fprintf (stderr,"Translation has errors - ") ;
    }
  fprintf (stderr,"%d files generated - %d lines - %d Warnings\n", nbGeneratedFile, nbLinesTotal, nbWarning) ;
  }

  if (writeWarning == 0) {
#ifdef WIN32
    sprintf(strTmp, "del %s", logFileName);
#else
    sprintf(strTmp, "rm -f %s", logFileName);
#endif
    system(strTmp);
  }

  if  ((stringTranslation == 1) && (displayMessage == 1)) {
    printf("Result =>\n\n%s\n",stringTranslationResult);
  }


  if (displayMessage == 1) { /* on affiche l'heure de debut */
    fprintf (stderr,"\nStarted at  : %s",ctime(&tnow));
  }
  time(&tnow); /* on prend le temps a la fin */
  if (displayMessage == 1) { /* on affiche l'heure d'arret */
    fprintf (stderr,"Finished at : %s",ctime(&tnow));
  }

  if (translationError==0) {
    if (displayMessage == 1) { /* on affiche l'heure d'arret */
      char buffer[1024]; strcpy(buffer,"");
    
      getcwd(buffer, 1024);
      fprintf (stderr,"To execute (if you are NOT running idl2matlab from Matlab):\n"
      "1-Go into directory:\n  cd %s\n"
      "1-Launch Matlab\n"
      "2-Type at Matlab prompt: p=pwd; cd('%s'); i2m_init; cd(p);\n"
      "3-Type the translated program name at Matlab prompt\n",
      (strlen(buffer) ? buffer : "."), i2mDirName); 
      fprintf (stderr,"To execute (if you are running idl2matlab from Matlab):\n"
      "Type the translated program name at Matlab prompt\n");
    }
    return 0;
  } else {
    return 1;
  }
}
