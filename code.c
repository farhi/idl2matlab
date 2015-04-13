/******************************************************************************
*                                IDL2SCILAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Code generation
*   Auteurs             :       Gardon Lucien
*                               Benzeghioua Abdeslam
*                               Cortina Stephane
*                               Bourtembourg Reynald
*   Date creation       :       19 / 04 / 2002
*   Modification        :       26 / 09 / 2003
*
*****************************************************************************/

#include "code.h"

#define MAX_STR 50

int tab;    /*+ tabulation courante - elle est modifiee par les fonction incTab et decTab +*/

/*+  Variable de numerotation de variable - utilisee dans toutes les fonctions
     construisant les parametres des fonctions
 +*/
int numVar;
int numParamInCurProc; /* Le nombre de parametres de la procedure que l'on traite actuellement */

extern int nb_lines;

char strIdl2Matlab[1024]; /*+ contient le code Matlab de preparation des parametres +*/

char outFile[256];        /*+ nom du fichier cible +*/
char outDir[256];         /*+ nom du repertoire cible +*/
char logFileName[256];    /*+ nom du fichier log +*/
char sourceFileName[256]; /*+ nom du fichier source +*/
char outFileName[256];    /*+ nom du fichier source +*/
char sourceDirName[256];  /*+ nom du repertoire source +*/
char* stringTranslationResult;  /*+ chaine contenant la traduction +*/
char oneFunctionName[256];

FILE *pOutputFile, *pLogFile;

int errExec; /*+ erreur pendant l execution du exec dans le arobase +*/

/*+ Erreur renvoyee par la table des symboles +*/
statusEnum erreurHashTable;
/*+ Erreur renvoyee par le systeme +*/
statusEnum err;

/*+ variables locales  utilisees par parsetree +*/
int inMatrix = 0;            /*+ vaut 1 si on est dans une matrix sinon 0 +*/
int inRef_struct_list = 0;   /*+ vaut 1 si on est dans une ref_struct_list sinon 0 +*/
int inFunction = 0;          /*+ vaut 1 si on est dans une fonction sinon 0 +*/
int inFunctionWithParam = 0; /*+ vaut 1 si on est dans une fonction avec parametres sinon 0 +*/
int inProcWithParam = 0;	   /*+ vaut 1 si on est dans une procedure avec parametres sinon 0 +*/
int isFunctionCall = 0 ;     /*+ vaut 1 si on est dans un appel de fonction sinon 0 +*/
int common = 0;              /*+ vaut 1 si on est dans un common sinon 0 +*/
int appel_matrice_struct = 0;/*+ vaut 1 si on est dans un appel de matrice sinon 0 +*/
int inNamedCommon = 0;       /*+ vaut 1 si on est dans un common nomme +*/
int exp_for = 0;             /*+ vaut 1 si on est dans l'expression d'un "for" +*/
/*+ vaut 1 ou 0 selon la position de l'appel +*/
/*+ vaut 1 si dans un assignement +*/
int appelFonction = 0;
int inCondStruct = 0;        /*+ vaut 1 si on est dans un if, while ... +*/

unsigned char printOnOff = 1;/*+ indique si on ecrit (1) dans le fichier cible ou pas +*/
int Pas_tab = 0;

/*+ Tableau contenant l'ensemble des dimensions d'une matrice +*/
/*+ TabDim[n] contient la n eme dimension +*/
int TabDim[4096];

char undeclaredVariables[4096];

/*+ Longueur de TabDim +*/
int indDim = 0;

Node* nameAssignement;   /*+ pointe vers le noeud devant un =, +=, -=, ...(une affectation)+*/
char opAssignment[10];   /*+ pour stocker le type d'operation associee a l'affectation en cours ("=", "+=", "*=", ...) +*/
char *strTmpOp; /*+ Chaine de caractere utile pour stocker l'operateur avant de l'afficher dans le fichier de traduction +*/
Node* catchVar[256];     /*+ variable du Handle du catch +*/
Node* catchExecNode[256];/*+ instruction du catch +*/
Node* ioErrNode[256];
unsigned char catchCount;/*+ numero du catch courant +*/
unsigned char ioErrCount;

/*****************************************************************/
/***  fonctions de premieres necessite   **/
/*****************************************************************/

/*+ libere un noeud +*/
void freeNode(Node* n) {
  if (n != NULL) {
    freeNode(n->fg);
    freeNode(n->fd);
    free(n);
  }
}

/*+ ouvre un fichier en lecture et lit la premiere ligne
    pstrline doit deja etre alloue pour recevoir le resultat +*/
int readLineInFile(char *pfileName, char *pstrLine) {
  /*+ la fonction renvoie 0 si tout se passe bien et -1 sinon +*/
  FILE *fichero;
  int i, c;

  /* search in the file */
  fichero = fopen( pfileName, "r" );
  if( fichero == NULL ) {
    /* unable to find file */
    return -1;
  }
  while(! feof(fichero)) {
    i = 0;
    do {  /* on lit la ligne caractere par caractere */
      c = fgetc(fichero);
      pstrLine[i] = c;
      i++;
    } while ((c != EOF) && (c != '\n'));
    pstrLine[i-1] = '\0';
  }
  fclose(fichero);
  return 0;
}


/*+ la chaine standard strIdl2Matlab prepare les valeur des parametres
 Cette suite de commande est placee en entete de chaque fonction et procedure cree
 pour permettre de passer les parametres par adresse +*/
void setString() {
    if (inScilabTranslation==0) {
  		strcpy(strIdl2Matlab, "  I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;\n");
  		strcat(strIdl2Matlab, "  for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1; I2M_ok=0; break; end; eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end;\n");
  		strcat(strIdl2Matlab, "  if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;\n");
  		strcat(strIdl2Matlab, "  if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;\n");
	}
	else {

		strcpy(strIdl2Matlab, "  I2M_lst=[]; I2M_out=[];  lv=length(varargin); if (~(modulo(lv,2)==0)), I2M_ok=0;  else,   I2M_ok=1;\n");
		strcat(strIdl2Matlab, "  for I2M=1:2:lv, I2M_tmp=varargin(I2M); if (typeof(I2M_tmp)~='string'); I2M_ok=0;break; end;[varquisertarien,I2Mx]=grep(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1; I2M_ok=0;break; end; str=string(I2Mkwv(I2Mx)) + '=' + string(varargin(I2M+1)) + ';'; execstr([str]); I2M_lst((I2M+1)/2)=I2Mkwv(I2Mx); end; end;\n");
		strcat(strIdl2Matlab, "  if ~I2M_ok; for I2M=1:lv; str=string(I2Mkwv(I2M)) + '=' + string(varargin(I2M)) + ';';  execstr([str]);end; end;\n");
		strcat(strIdl2Matlab, "  if ~isempty(I2M_pos); for I2M=1:length(I2M_pos);str='varargout(' + string(I2M) + ') = ' + I2M_lst(I2M_pos(I2M)) + ';'; I2M_out= [I2M_out  str]; end; end;\n");

	}
}

/*+ enleve le caractere en parametre d une chaine de caractere en renvoie le resultat dans strString
 ne fait pas de difference entre les minuscules et majuscules
 ie b enleve B et b +*/
void removeCharacter(char *str, char car, char* strString) {
  int i;
  strcpy(strString, "");
  i = 0;
  while (*str != '\0') {
    if (tolower(*str) != tolower(car)) {
      strString[i++]= *str;
    }
    str++;
  }
  strString[i]='\0';
 }

/*+ remplace le caractere car par car2
 ne fait pas de difference entre les minuscules et majuscules
 ie b enleve B et b +*/
void replaceCharacter(char *str, char car, char car2, char* strString) {
  int i;
  strcpy(strString, "");
  i = 0;
  while (*str != '\0') {
    if (tolower(*str) != tolower(car)) {
      strString[i++]= *str;
    } else {
      strString[i++]= car2;
    }
    str++;
  }
  strString[i]='\0';
}

/*+ Transforme une chaine IDL en chaine Matlab
 result doit etre suffisamment alloue +*/
void adaptString(char *str, char* result) {
  int i, len;
  i=0;
  len = strlen(str);
  if (*str == '"') { /* cas de la chaine IDL avec des " */
    result[i++]= '\'';
    str++;
    while (*str != '\0') {
      if (*str == '\'') {
        result[i++]= '\''; /* on double le ' */
        result[i++]= '\'';
        str++;
      } else if (*str == '"') {
        result[i++]= '"';
        str++;
        if (*str == '"') { /* en idl le " est double donc on zap le 2eme */
          str++;
        }
      } else {
        result[i++]= *str;
        str++;
      }
    }
    i--; /* on se recale sur le dernier " */
    result[i++]='\'';
    result[i++]='\0';
  } else { /* cas de la chaine avec un ' */
    strcpy(result,str);
  }
}

/*+ lit dans str jusqu au premier ; et renvoie la chaine lu dans strString
 le reste de la chaine est renvoye par la fonction
 fonction utile pour parser le fichier de traduction avancee de la base de connaissance +*/
char* getLibFileWord(char* str, char*strString) {
  char* strTmp;
  int i,j,bOk;
  strTmp = (char*) malloc (256);
  strcpy(strTmp, "");
  i = 0; j=0; bOk = 0;
  while(*str != '\0') {
    if (*str != ';' && !bOk) {
      strString[i] = *str;  /* copie avant le ; */
      i++;
    } else {
      bOk = 1;
      if (*str == ';' && j == 0) { /* copie apres ;*/
      	/* on fait rien*/
      } else {
         strTmp[j] = *str;
         j++;
       }
     }
     str++;
   }
   strString[i] = '\0';
   strTmp[j] = '\0';
   return strTmp;
}

/*+ Ecrit la chaine dans le fichier cible +*/
void printOut(char *pstrLine) {
  if (stringTranslation == 1) { /* cas de la traduction d une chaine */
    strcat(stringTranslationResult, pstrLine);
  } else {  /* cas de traduction normal d un fichier */
    if (pOutputFile != NULL) {
      if (printOnOff == 1) {
        fputs(pstrLine, pOutputFile);
      }
    }
  }
}

/*+ ecrit la chaine dans le fichier de log +*/
void printToLog(char *pstrLine) {
  if (pLogFile != NULL) {
    if (printOnOff == 1) {
      translationError = 1;
      fileTranslationError = 1;
      nbWarningFile++;
      fputs(pstrLine, pLogFile);
    }
  }
}

/********************************************/
/** Fonctions d indentation du code cible ***/
/********************************************/

/*+ incremente la tabulation du code +*/
void incTab() {
  tab = tab + tabVal;
}

/*+ decremente la tabulation du code +*/
void decTab() {
  tab = tab - tabVal;
}

/*+ affiche la tabulation +*/
void printTab() {
  int i;
  for (i=0; i<tab; i++) {
    printOut(" ");
  }
}

/*+ Parse le noeud pour trouver le numero de ligne reel - ie le min +*/
int realLineInSource(Node* n) {
int result, resTmp;
  if (n == NULL) {
    return 0;
  } else {
    result = n->lineInSource;
    if (n->fg != NULL) {
      resTmp = realLineInSource(n->fg);
      if (resTmp < result) {
        result = resTmp;
      }
    }
    if (n->fd != NULL) {
      resTmp = realLineInSource(n->fd);
      if (resTmp < result) {
        result = resTmp;
      }
    }
  }
  return result;
}

/*+ Ecrit tous les commentaires non ecrits jusqu a la ligne dont le numero est passe en parametres +*/
void insertComment(int nLine) {
char *strTmp;
char *strTmp2;

  if (printOnOff == 1) {
    if (commentTable != NULL) {
      while (commentTable->lineInSource <= nLine) {
		printOut(commentaire);
        strTmp = (char*) malloc (strlen(commentTable->commentString)+1);
        removeCharacter(commentTable->commentString, '$', strTmp);
        strTmp2 = (char*) malloc (strlen(strTmp)+1);
        removeCharacter(strTmp, ';', strTmp2);
        printOut(strTmp2);
        /*printf("%s - tab = %d\n",strTmp2,tab);*/
        free(strTmp);
        free(strTmp2);
        printOut("\n");
        printTab();
        numCurrentLine++;
        commentTable = commentTable->nextComment;
        if (commentTable == NULL) break;
      }
    }
  }
}


/*****************************************************************/
/***  FIN fonctions de premieres necessite   **/
/*****************************************************************/

/*************************************************/
/*************************************************/
/*****  Generation des REF STRUCT                */
/*************************************************/
/*************************************************/

void parseTree(Node *n);
void generer_ref_struct(Node *ng, Node *nd);
void gerer_ident_parent(Node *n, Node *nd);

/*+ Type representant un nom de structure et un booleen
 ident = vrai si  nom_struct est un tableau de structure +*/
typedef struct {
  char*	nom_struct;	/*+ Nom de la structure analysee +*/
  int ident;		/*+ Booleen qui vaut vrai si la structure est indicee +*/
} struct_indice;

/*+ L'ensemble des structures sont contenues dans le tableau TabStruct +*/
struct_indice TabStruct[1024];

/*+ indStruct est la longueur de TabStruct +*/
int indStruct = 0;

/*+ Un appel de structure peut s'ecrire de cette facon : st_array(a1,a2,...,aN).field1 = X +*/
/*+ indice1 == N +*/
int ind_in_matrice = 0;



/*****************************************************************************/

/*+ Une structure est representee sous la forme : str_array = struct('field1',val1,'field2',val2...)
 genererStruct(n) genere a partir du noeud n, la suite "'field1',val1,'field2',val2..." +*/
void genererStruct(Node *n) {
  char *strTmp;

  if (n != NULL) {
    /* On affiche field entre cotes */
    printOut("'");
    printOut((n->valNode).uString);
    printOut("',");
    /* On affiche la valeur du field (val) */
    parseTree(n->fg);
    /* On n'affiche pas de virgule apres le dernier val */
    if (n->fd != NULL) {
      printOut(","); genererStruct(n->fd);
    }
  }
}

/*+ Une structure est representee sous la forme : str_array = struct('field1',val1,'field2',val2...)
 genererStruct(n) genere a partir du noeud n, la suite "'field1',val1,'field2',val2..." +*/
void genererNamedStruct(Node *n, Node *nval) {
/*+ n contient le nom des champs et nval les valeur d init +*/
  char *strTmp;

  if (n != NULL) {
    /* On affiche field entre cotes */
    printOut("'");
    printOut((n->valNode).uString);
    printOut("',");
    /* On affiche la valeur du field (val) */
    if (nval != NULL) {
      if (nval->typeNode == SUITE_CALL_LIST) {
        parseTree(nval->fg);
        nval = nval->fd;
      } else { /* cas du dernier param */
        parseTree(nval);
      }
    }
    /* On n'affiche pas de virgule apres le dernier val */
    if (n->fd != NULL) {
      printOut(","); genererNamedStruct(n->fd, nval);
    }
  }
}


/*****************************************************************************/

/*+  Creer le "recType" de nom "str" avec le num_type "REFERENCE" et le suite_comp "n"
 et insere ce "recType" dans la table des symboles +*/
void creer_ref_tab_symb(char *str, Node *n) {
  recType *rec;

  /* On alloue la memoire pour creer le "recType" */
  rec = malloc(sizeof(recType));
  rec->name = malloc(strlen(str)+5);
  rec->ptype = malloc(sizeof(type));
  rec->ptype->suite_comp = malloc(sizeof(Node *));
  /* On entre le nom dans le "recType" */
  sprintf(rec->name, "REF %s", str);
  rec->ptype->num_type = REFERENCE;
  rec->ptype->suite_comp = n;
  /* On insere le "recType" dans la table des symboles */
  insert(rec);
}

/*****************************************************************************/

/*+ st_array(a1,a2,...,aN).field1....fieldn = X
 n = l'arbre representant a1,a2,...,aN ///nd = X /// suite = field1....fieldn
 generer_suite_param(n, nd, suite) genere st_array(a1,a2,...,aN)
 en matlab sous forme de boucle imbriquee +*/
void generer_suite_param(Node *n, Node *nd, Node *suite){
  char* strTmp;
  Node *croch;

  /*+ ind_in_matrice est egal au numero du parametre traite de st_array(a1,a2,...,aN) +*/
  ind_in_matrice++;
  if (n != NULL){
    /* On regarde 2 cas : on traite le dernier parametre ou pas */
    if (n->typeNode != PARAM_MATRIX_SUITE && n->typeNode != SUITE_CALL_LIST) {
      /* On traite le cas du dernier parametre  */
      /* On memorise le nombre de parametres dans TabStruct */
      TabStruct[indStruct].ident = ind_in_matrice;

      /* On genere alors une boucle suivant la valeur de aN */
      /* On regarde le "type" de aN */
      switch (n->typeNode) {
        case INTERVALLE :
          /* aN est un intervalle */
          printOut("for ");
          strTmp = (char*) malloc (MAX_STR);
          printOut("j");
          /* On affice le numero du parametre (ind_in_matrice) */
          /* L'index de la boucle aura la valeur jind_in_matrice */
          /* Les valeurs des index seront alors toutes distinctes */
          sprintf(strTmp, "%d", ind_in_matrice);
          printOut(strTmp);
          free(strTmp);
          /* On affiche le "=" de la boucle */
          printOut("=");
          parseTree(n->fg);
          printOut("+1");
          /* On affiche le ":" de la boucle */
          printOut(":");

          /* On regarde si la partie droite de l'intervalle est une * */
          if (n->fd->typeNode == TIMES) {
            /* C'est une etoile. Dans ce cas, on fait jind_in_matrice */
            /* parcourt la boucle jusqu'a size(str_array,2). D'ou l'affichage suivant */
            printOut("end\n");
            numCurrentLine++;
          } else {
            /* Ce n'est pas une * */
            parseTree(n->fd);
            printOut("+1");
          }
          printOut("\n");
                numCurrentLine++;
                incTab();

          /* st_array(a1,a2,...,aN) a fini d'etre genere */
          /* Il faut maintenant generer field1....fieldn = X */
          generer_ref_struct(suite, nd);
          printOut("end");
                printOut("\n");
                numCurrentLine++;
                incTab();
          break;

        case PARAM_MATRIX_ETOILE :
                /* aN est une * */
          printOut("for j");
          strTmp = (char*) malloc (MAX_STR);
                /* On affice le numero du parametre (ind_in_matrice) */
          /* L'index de la boucle aura la valeur jind_in_matrice */
          /* Les valeurs des index seront alors toutes distinctes */
          sprintf(strTmp, "%d", ind_in_matrice);
                printOut(strTmp);
                free(strTmp);
                printOut("=1:end\n");
          numCurrentLine++;

          /* st_array(a1,a2,...,aN) a fini d'etre genere */
          /* Il faut maintenant generer field1....fieldn = X */
          generer_ref_struct(suite, nd);
          printOut("end\n");
                numCurrentLine++;
                incTab();
                break;

        case MATRIX :
          /* aN est une matrice */
          printOut("for j");
          /* On affiche le numero du parametre (ind_in_matrice) */
          /* L'index de la boucle aura la valeur jind_in_matrice */
          /* Les valeurs des index seront alors toutes distinctes */
          strTmp = (char*) malloc (MAX_STR);
                sprintf(strTmp, "%d", ind_in_matrice);
                printOut(strTmp);
                free(strTmp);
                printOut("=[");

          /* croch est le noeud contenant les parametres de la matrice */
          croch = n->fg;
          /* On genere la matrice en affichant ses parametres */
          while (croch->typeNode == SUITE_MATRIX) {
            parseTree(croch->fg);
            printOut("+1");
            printOut(",");
            croch = croch->fd;
          }
          /* On affiche le dernier parametre de la matrice */
          parseTree(croch);
          printOut("+1");
          printOut("]");
          printOut("\n");
          numCurrentLine++;
          incTab();

          /* st_array(a1,a2,...,aN) a fini d'etre genere */
          /* Il faut maintenant generer field1....fieldn = X */
          generer_ref_struct(suite, nd);
          printOut("\n");
          numCurrentLine++;
          incTab();
          printOut("end");
                printOut("\n");
                numCurrentLine++;
                incTab();
          break;

              default :
          /* aN represente un entier qui peut etre ecrit sous differentes formes */
                printOut("for j");
          strTmp = (char*) malloc (MAX_STR);
                /* On affice le numero du parametre (ind_in_matrice) */
          /* L'index de la boucle aura la valeur jind_in_matrice */
          /* Les valeurs des index seront alors toutes distinctes */
          sprintf(strTmp, "%d", ind_in_matrice);
                printOut(strTmp);
                free(strTmp);
                printOut("=");

          /* On genere aN comme si on generait une matrice */
          printOut("[");
          parseTree(n);
          printOut("+1");
          printOut("]\n");
                numCurrentLine++;
                incTab();

          /* st_array(a1,a2,...,aN) a fini d'etre genere */
          /* Il faut maintenant generer field1....fieldn = X */
          generer_ref_struct(suite, nd);
          printOut("\n");
          numCurrentLine++;
          incTab();
        printOut("end\n");
              numCurrentLine++;
              incTab();
          }
        } else {
          /* On traite le cas des parametres autre que le dernier */
          /* On regarde le "type" de aN */
          switch (n->fg->typeNode) {
      case INTERVALLE :
        /* aX est un intervalle */
        printOut("for ");
        strTmp = (char*) malloc (MAX_STR);
        printOut("j");
        /* On affice le numero du parametre (ind_in_matrice) */
        /* L'index de la boucle aura la valeur jind_in_matrice */
        /* Les valeurs des index seront alors toutes distinctes */
        sprintf(strTmp, "%d", ind_in_matrice);
        printOut(strTmp);
              free(strTmp);

        /* On affiche le "=" de la boucle */
        printOut("=");
        parseTree(n->fg->fg);
        printOut("+1");

        /* On affiche le ":" de la boucle */
        printOut(":");

        /* On regarde si la partie droite de l'intervalle est une * */
        if (n->fg->fd->typeNode == TIMES) {
          /* C'est une etoile. Dans ce cas, on fait jind_in_matrice */
          /* parcourt la boucle jusqu'a size(str_array,2). D'ou l'affichage suivant */
          printOut("end\n");
          numCurrentLine++;
        } else {
          /* Ce n'est pas une * */
          parseTree(n->fg->fd);
          printOut("+1");
        }
        printOut("\n");
              numCurrentLine++;
              incTab();

        /* aX a fini d'etre genere. Il faut maintenant generer a(X+1) */
        generer_suite_param(n->fd, nd, suite);

        printOut("end");
              printOut("\n");
              numCurrentLine++;
              incTab();
        break;

      case PARAM_MATRIX_ETOILE :
        /* aX est une * */
        printOut("for j");
        strTmp = (char*) malloc (MAX_STR);
              /* On affice le numero du parametre (ind_in_matrice) */
        /* L'index de la boucle aura la valeur jind_in_matrice */
        /* Les valeurs des index seront alors toutes distinctes */
        sprintf(strTmp, "%d", ind_in_matrice);
              printOut(strTmp);
              free(strTmp);
              printOut("=1:end\n");
        numCurrentLine++;

        /* aX a fini d'etre genere. Il faut maintenant generer a(X+1) */
        generer_suite_param(n->fd, nd, suite);

        printOut("end\n");
              numCurrentLine++;
              incTab();
        break;

            case MATRIX :
        /* aX est une matrice */
        printOut("for j");
        strTmp = (char*) malloc (MAX_STR);
              /* On affice le numero du parametre (ind_in_matrice) */
        /* L'index de la boucle aura la valeur jind_in_matrice */
        /* Les valeurs des index seront alors toutes distinctes */
        sprintf(strTmp, "%d", ind_in_matrice);
              printOut(strTmp);
              free(strTmp);
              printOut("=[");

        /* croch est le noeud contenant les parametres de la matrice */
        croch = n->fg->fg;

        /* On genere la matrice en affichant ses parametres */
        while (croch->typeNode == SUITE_MATRIX) {
          parseTree(croch->fg);
          printOut("+1");
          printOut(",");
          croch = croch->fd;
        }
        /* On affiche le dernier parametre de la matrice */
        parseTree(croch);
        printOut("+1");
        printOut("]\n");
        numCurrentLine++;
        incTab();

        /* aX a fini d'etre genere. Il faut maintenant generer a(X+1) */
        generer_suite_param(n->fd, nd, suite);
        printOut("\n");
          numCurrentLine++;
            incTab();
      printOut("end\n");
            numCurrentLine++;
            incTab();
      break;

    default :
      /* aN represente un entier qui peut etre ecrit sous differentes formes */
      printOut("for ");
      printOut("j");
      strTmp = (char*) malloc (MAX_STR);
            /* On affice le numero du parametre (ind_in_matrice) */
      /* L'index de la boucle aura la valeur jind_in_matrice */
      /* Les valeurs des index seront alors toutes distinctes */
      sprintf(strTmp, "%d", ind_in_matrice);
            printOut(strTmp);
            free(strTmp);
            printOut("=");

      /* On genere aN comme si on generait une matrice */
      printOut("[");
      parseTree(n->fg);
      printOut("+1");
      printOut("]");
      printOut("\n");
            numCurrentLine++;
            incTab();

      /* aX a fini d'etre genere. Il faut maintenant generer a(X+1) */
      generer_suite_param(n->fd, nd, suite);
            printOut("\n");
            numCurrentLine++;
            incTab();
      printOut("end");
            printOut("\n");
            numCurrentLine++;
            incTab();

      }
    }
  }
}

/*****************************************************************************/

/*+ str_array(a).field1...fieldn = X
 n = l'arbre representant str_array(a).field1...fieldn ///nd = X
 gerer_ident_parent(n, nd) genere str_array(a)
 en matlab sous forme de boucle imbriquee +*/
void gerer_ident_parent(Node *n, Node *nd){

char* strTmp;
  int i;
  Node *croch;

  /*+ La grammaire reconnait str_array(a) de 4 facons : soit c'est une FUNCTION_CALL_OU_REF_MATRIX +*/
  /*+ soit c'est un IDENTIFIER_PARENTHESE soit il fait parti d'un REF_STRUCT +*/
  /*+ soit il fait parti d'un REF_STRUCT_LIST +*/

  /*+ On genere alors str_array(a) selon ces 4 cas +*/
  if (n->typeNode == FUNCTION_CALL_OU_REF_MATRIX) {
    /* str_array(a) est representee par une FUNCTION_CALL_OU_REF_MATRIX */
    /* On regarde le "type" de a */
    switch (n->fg->typeNode){
      case INTERVALLE :
        /* a est un intervalle */
        printOut("for ");
        strTmp = (char*) malloc (MAX_STR);
        printOut("i");
        /* On affice le numero du parametre (ind_in_matrice) */
        /* L'index de la boucle aura la valeur iind_in_matrice */
        /* Les valeurs des index seront alors toutes distinctes */
        sprintf(strTmp, "%d", indStruct);
        printOut(strTmp);
        free(strTmp);
        /* On affiche le "=" de la boucle */
        printOut("=");
        parseTree(n->fg->fg);
        printOut("+1");
        /* On affiche le ":" de la boucle */
        printOut(":");
        /* On regarde si la partie droite de l'intervalle est une * */
        if (n->fg->fd->typeNode == TIMES) {
          /* C'est une etoile. Dans ce cas, on fait iind_in_matrice */
          /* parcourt la boucle jusqu'a size(str_array,2). D'ou l'affichage suivant */
          printOut("end\n");
          numCurrentLine++;
        } else {
          /* Ce n'est pas une * */
          parseTree(n->fg->fd);
          printOut("+1");
        }
        printOut("\n");
        numCurrentLine++;
        incTab();

        /* st_array(a) a fini d'etre genere */
        /* Il faut maintenant generer field1....fieldn = X */
        printTab();
        generer_ref_struct(n->fd, nd);
        decTab(); /* TEST ?? */
        printTab();
        printOut("end");
/*              numCurrentLine++;
              decTab();*/
        break;

      case PARAM_MATRIX_ETOILE :
	/* a est une * */
	printOut("for i");
	strTmp = (char*) malloc (MAX_STR);
        /* On affice le numero du parametre (ind_in_matrice) */
	/* L'index de la boucle aura la valeur iind_in_matrice */
	/* Les valeurs des index seront alors toutes distinctes */
	sprintf(strTmp, "%d", indStruct);
        printOut(strTmp);
        free(strTmp);

	/* iind_in_matrice parcourt la boucle jusqu'a size(str_array,2) */
	printOut("=1:end\n");
	numCurrentLine++;
    incTab(); /* TEST ?? */
	/* st_array(a) a fini d'etre genere */
	/* Il faut maintenant generer field1....fieldn = X */
    printTab();
	generer_ref_struct(n->fd, nd);
    decTab(); /* TEST ?? */
    printTab();
	printOut("end");
/*        numCurrentLine++;
        decTab();          */
	break;
      case MATRIX :
      	/* a est une matrice */
	printOut("for i");
	strTmp = (char*) malloc (MAX_STR);
        /* On affice le numero du parametre (ind_in_matrice) */
	/* L'index de la boucle aura la valeur iind_in_matrice */
	/* Les valeurs des index seront alors toutes distinctes */
	sprintf(strTmp, "%d", indStruct);
        printOut(strTmp);
        free(strTmp);
      	printOut("=[");

      	/* croch est le noeud contenant les parametres de la matrice */
        croch = n->fg->fg;

        /* On genere la matrice en affichant ses parametres */
        while (croch->typeNode == SUITE_MATRIX) {
          parseTree(croch->fg);
          printOut("+1");
          printOut(",");
          croch = croch->fd;
        }
        /* On affiche le dernier parametre de la matrice */
        parseTree(croch);
        printOut("+1");
        printOut("]\n");
        numCurrentLine++;
        incTab();

	/* st_array(a) a fini d'etre genere */
	/* Il faut maintenant generer field1....fieldn = X */
    printTab();
	generer_ref_struct(n->fd, nd);
	/*printOut("\n");
        numCurrentLine++;*/
    decTab();
    printTab();
	printOut("end");
        /*numCurrentLine++;
        decTab();*/
	break;
      case SUITE_CALL_LIST :
	/* a est une suite de parametre */
	generer_suite_param(n->fg, nd, n->fd);
	break;
      default :
	/* a represente un entier qui peut etre ecrit sous differentes formes */
	printOut("for i");
	strTmp = (char*) malloc (MAX_STR);
        /* On affice le numero du parametre (ind_in_matrice) */
        /* L'index de la boucle aura la valeur iind_in_matrice */
  /* Les valeurs des index seront alors toutes distinctes */
  sprintf(strTmp, "%d", indStruct);
  printOut(strTmp);
  free(strTmp);
  /* On genere aN comme si on generait une matrice */
  printOut("=[");
  parseTree(n->fg);
  printOut("+1");
  printOut("]\n");
  numCurrentLine++;
  incTab();

  /* st_array(a) a fini d'etre genere */
  /* Il faut maintenant generer field1....fieldn = X */
  printTab();
  generer_ref_struct(n->fd, nd);
  /*printOut("\n");
        numCurrentLine++;*/
  decTab();
  printTab();
  printOut("end");
        /*numCurrentLine++;
        decTab();*/

    }
  } else {
    if (n->typeNode == REF_STRUCT || n->typeNode == REF_STRUCT_LIST) {
      /* str_array(a) est representee par une REF_STRUCT ou une REF_STRUCT_LIST */
      /* On regarde le "type" de a */
      switch (n->fg->fg->typeNode){
  case INTERVALLE :
    /* a est un intervalle */
    printOut("for ");
    strTmp = (char*) malloc (MAX_STR);
    printOut("i");
    /* On affice le numero du parametre (ind_in_matrice) */
    /* L'index de la boucle aura la valeur iind_in_matrice */
    /* Les valeurs des index seront alors toutes distinctes */
    sprintf(strTmp, "%d", indStruct);
    printOut(strTmp);
    free(strTmp);

    /* On affiche le "=" de la boucle */
    printOut("=");
    parseTree(n->fg->fg->fg);
    printOut("+1");
    /* On affiche le ":" de la boucle */
    printOut(":");

    /* On regarde si la partie droite de l'intervalle est une * */
    if (n->fg->fg->fd->typeNode == TIMES) {
      /* C'est une etoile. Dans ce cas, on fait iind_in_matrice */
      /* parcourt la boucle jusqu'a size(str_array,2). D'ou l'affichage suivant */
      printOut("end\n");
      numCurrentLine++;
    } else {
      /* Ce n'est pas une * */
      parseTree(n->fg->fg->fd);
      printOut("+1");
    }
    printOut("\n");
    numCurrentLine++;
    incTab();

    /* st_array(a) a fini d'etre genere */
    /* Il faut maintenant generer field1....fieldn = X */
    printTab();
    generer_ref_struct(n->fd, nd);
    /*printOut("\n");
          numCurrentLine++;*/
    decTab();
    printTab();
    printOut("end");
    printOut("\n");
    numCurrentLine++;
          /*decTab();*/
    break;

  case PARAM_MATRIX_ETOILE :
    /* a est une * */
    printOut("for i");
    strTmp = (char*) malloc (MAX_STR);
    /* On affice le numero du parametre (ind_in_matrice) */
    /* L'index de la boucle aura la valeur iind_in_matrice */
    /* Les valeurs des index seront alors toutes distinctes */
    sprintf(strTmp, "%d", indStruct);
    printOut(strTmp);
    free(strTmp);
    printOut("=1:end\n");
    numCurrentLine++;
    incTab(); /* TEST */
    /* st_array(a) a fini d'etre genere */
    /* Il faut maintenant generer field1....fieldn = X */
    printTab();
    generer_ref_struct(n->fd, nd);
    /*printOut("\n");
          numCurrentLine++;*/
    decTab();
    printTab();
    printOut("end");
          /*numCurrentLine++;
          decTab();*/
    break;
        case MATRIX :
    /* a est une matrice */
    printOut("for i");
    strTmp = (char*) malloc (MAX_STR);
          /* On affice le numero du parametre (ind_in_matrice) */
    /* L'index de la boucle aura la valeur iind_in_matrice */
    /* Les valeurs des index seront alors toutes distinctes */
    sprintf(strTmp, "%d", indStruct);
          printOut(strTmp);
          free(strTmp);
          printOut("=[");

    /* croch est le noeud contenant les parametres de la matrice */
          croch = n->fg->fg->fg;

    /* On genere la matrice en affichant ses parametres */
    while (croch->typeNode == SUITE_MATRIX) {
      parseTree(croch->fg);
      printOut("+1");
      printOut(",");
      croch = croch->fd;
    }
    /* On affiche le dernier parametre de la matrice */
    parseTree(croch);
    printOut("+1");

    printOut("]");
    printOut("\n");
    numCurrentLine++;
    incTab();

    /* st_array(a) a fini d'etre genere */
    /* Il faut maintenant generer field1....fieldn = X */
    printTab();
    generer_ref_struct(n->fd, nd);
    /*printOut("\n");
          numCurrentLine++;*/
    decTab();
    printTab();
    printOut("end");
          printOut("\n");
          numCurrentLine++;
          /*decTab();*/
      break;

  case PARAM_MATRIX_SUITE :
    /* a est une suite de parametre */
    generer_suite_param(n->fg->fg, nd, n->fd);
    break;

  default :
    /* a represente un entier qui peut etre ecrit sous differentes formes */
    printOut("for i");
    strTmp = (char*) malloc (MAX_STR);
          /* On affice le numero du parametre (ind_in_matrice) */
    /* L'index de la boucle aura la valeur iind_in_matrice */
    /* Les valeurs des index seront alors toutes distinctes */
    sprintf(strTmp, "%d", indStruct);
          printOut(strTmp);
          free(strTmp);
          printOut("=");

    /* On genere aN comme si on generait une matrice */
    printOut("[");
    parseTree(n->fg->fg);
    printOut("+1");
    printOut("]\n");
    numCurrentLine++;
    incTab();

    /* st_array(a) a fini d'etre genere */
    /* Il faut maintenant generer field1....fieldn = X */
    printTab();
    generer_ref_struct(n->fd, nd);
    /*printOut("\n");
          numCurrentLine++;*/
    decTab();
    printTab();
    printOut("end");
          /*numCurrentLine++;
          decTab();*/
      break;
      }
    } else {
      if (n->typeNode == IDENTIFIER_PARENTHESE) {
        /* str_array(a) est representee par un IDENTIFIER_PARENTHESE */
        /* On regarde le "type" de a */
        switch (n->fg->typeNode){
    case INTERVALLE :
      /* a est un intervalle */
      printOut("for ");
      strTmp = (char*) malloc (MAX_STR);
            printOut("i");
      /* On affice le numero du parametre (ind_in_matrice) */
      /* L'index de la boucle aura la valeur iind_in_matrice */
      /* Les valeurs des index seront alors toutes distinctes */
      sprintf(strTmp, "%d", indStruct);
      printOut(strTmp);
            free(strTmp);

      /* On affiche le "=" de la boucle */
      printOut("=");
      parseTree(n->fg->fg);
      printOut("+1");
      /* On affiche le ":" de la boucle */
      printOut(":");

      /* On regarde si la partie droite de l'intervalle est une * */
      if (n->fg->fd->typeNode == TIMES) {
        /* C'est une etoile. Dans ce cas, on fait iind_in_matrice */
        /* parcourt la boucle jusqu'a size(str_array,2). D'ou l'affichage suivant */
        printOut("end\n");
        numCurrentLine++;
      } else {
        /* Ce n'est pas une * */
        parseTree(n->fg->fd);
        printOut("+1");
      }
      printOut("\n");
      numCurrentLine++;
      incTab();

      /* st_array(a) a fini d'etre genere */
      /* Il faut maintenant generer field1....fieldn = X */
      printTab();
      generer_ref_struct(n->fd, nd);
      /*printOut("\n");
            numCurrentLine++;*/
      decTab();
      printTab();
      printOut("end");
      /*numCurrentLine++;
      decTab();*/
      break;

    case PARAM_MATRIX_ETOILE :
      /* a est une * */
      printOut("for ");
      printOut("i");
      strTmp = (char*) malloc (MAX_STR);
            /* On affice le numero du parametre (ind_in_matrice) */
      /* L'index de la boucle aura la valeur iind_in_matrice */
      /* Les valeurs des index seront alors toutes distinctes */
      sprintf(strTmp, "%d", indStruct);
            printOut(strTmp);
            free(strTmp);
            printOut("=1:end\n");
      numCurrentLine++;
      incTab();  /* TEST */
      /* st_array(a) a fini d'etre genere */
      /* Il faut maintenant generer field1....fieldn = X */
      printTab();
      generer_ref_struct(n->fd, nd);
      /*printOut("\n");
            numCurrentLine++;*/
      decTab();
      printTab();
      printOut("end");
            /*numCurrentLine++;
            decTab();          */
      break;

    case MATRIX :
            /* a est une matrice */
      printOut("for i");
      strTmp = (char*) malloc (MAX_STR);
            /* On affice le numero du parametre (ind_in_matrice) */
      /* L'index de la boucle aura la valeur iind_in_matrice */
      /* Les valeurs des index seront alors toutes distinctes */
      sprintf(strTmp, "%d", indStruct);
            printOut(strTmp);
            free(strTmp);
            printOut("=[");

      /* croch est le noeud contenant les parametres de la matrice */
            croch = n->fg->fg;

      /* On genere la matrice en affichant ses parametres */
      while (croch->typeNode == SUITE_MATRIX) {
        parseTree(croch->fg);
        printOut("+1");
        printOut(",");
        croch = croch->fd;
      }
      /* On affiche le dernier parametre de la matrice */
      parseTree(croch);
      printOut("+1");
      printOut("]\n");
            numCurrentLine++;
            incTab();

      /* st_array(a) a fini d'etre genere */
      /* Il faut maintenant generer field1....fieldn = X */
      printTab();
      generer_ref_struct(n->fd, nd);
      /*printOut("\n");
            numCurrentLine++;*/
            decTab();
            printTab();
      printOut("end");
            /*numCurrentLine++;
            decTab();*/
      break;

    case PARAM_MATRIX_SUITE :
      /* a est une suite de parametre */
      generer_suite_param(n->fg, nd, n->fd);
      break;

          default :
      /* a represente un entier qui peut etre ecrit sous differentes formes */
      printOut("for i");
      strTmp = (char*) malloc (MAX_STR);
            /* On affice le numero du parametre (ind_in_matrice) */
      /* L'index de la boucle aura la valeur iind_in_matrice */
      /* Les valeurs des index seront alors toutes distinctes */
      sprintf(strTmp, "%d", indStruct);
            printOut(strTmp);
            free(strTmp);

      /* On genere aN comme si on generait une matrice */
      printOut("=[");
      parseTree(n->fg);
      printOut("+1");
      printOut("]\n");
            numCurrentLine++;
            incTab();

      /* st_array(a) a fini d'etre genere */
      /* Il faut maintenant generer field1....fieldn = X */
      printTab();
      generer_ref_struct(n->fd, nd);
      /*printOut("\n");
            numCurrentLine++;*/
            decTab();
            printTab();
      printOut("end");
            /*printOut("\n");
            numCurrentLine++;*/
            /*decTab();*/
      break;
  }
      }
    }
  }
}

/*****************************************************************************/

/*+ Genere "ng = nd" en matlab ou ng est un appel de structure
 Cette generation se fera sous forme de boucles imbriquees
 ng represente field1.field2....fieldn = X.  fieldX peuvent avoir des "parametres" +*/
void generer_ref_struct(Node *ng, Node *nd){
  char* strTmp;
  int i;
  int j = 1;
  recType *cherch;
  statusEnum err;
  int point_virgule = 0;

  /* indStruct est egal au numero du field traite */
  /*if (indStruct == 2)
     printTab();*/
  indStruct++;

  /*+ On genere en deux parties : 1ere partie : Les boucles imbriquees seules +*/
  /*+ 2eme partie : Ce qu'il y a a l'interieur de la boucle imbriquee +*/
  /*+ Voici un exemple de ce qui est genere : pour salut(1:3,*).bonjour([1,2]).salut = 6 +*/
  /*+
  for j1 = 1:3  	      	      	      	    <-- boucles imbriquees
    for j2 = 1:size(salut,2)    	      	    <-- boucles imbriquees
      for i2 = [1 2]    	      	      	    <-- boucles imbriquees
        salut(j1,j2).bonjour(i2).salut = 6    <-- ce qu'il y a a l'interieur
      end
    end
  end
  +*/

  /* Si ng est NULL, on genere "ce qu'il y a a l'interieur" */
  if (ng == NULL){
    for (i=1;i<indStruct ; i++) {/* On affiche le nom du field */
      strTmp = (char*) malloc (MAX_STR + sizeof(TabStruct[i].nom_struct));
      sprintf(strTmp, "%s", TabStruct[i].nom_struct);
      printOut(strTmp);
      free(strTmp);

      /* Il y a ensuite 3 cas : Le field n'a pas de parametres, le field a un seul parametre
      ou le field a plus d'un parametre */
      /* Si le field n'a pas de parametres, on ne fait rien */
      if (TabStruct[i].ident == 1) {
         point_virgule = 1;
        /* Le field a un seul parametre. On affiche alors l'index de la boucle
        a laquelle il appartient. L'index commence par i */
        printOut("(i");
        strTmp = (char*) malloc (MAX_STR);
        sprintf(strTmp, "%d", i);
        printOut(strTmp);
        free(strTmp);
        printOut(")");
      } else {
        if (TabStruct[i].ident > 1) {
           point_virgule = 1;
           /* Le field a plusieurs parametres. On affiche alors les index de la boucle
           a laquelle ils appartient. Les index commencent par j */
           printOut("(");
           while (j < TabStruct[i].ident) {
                 strTmp = (char*) malloc (MAX_STR);
                 printOut("j");
                 sprintf(strTmp, "%d", j);
                 printOut(strTmp);
                 free(strTmp);
                 printOut(",");
                 j++;
           }
           /* On affiche le dernier j */
          strTmp = (char*) malloc (MAX_STR);
          printOut("j");
          sprintf(strTmp, "%d", j);
          printOut(strTmp);
          free(strTmp);
          printOut(")");
          j++;
        }
      }
      /* On n'affiche pas de "." apres le dernier field */
      if (i != (indStruct - 1) ) {
         printOut(".");
      }
    }
    /* On genere la partie droite de "ce qu'il y a a l'interieur" */

    printOut(" = ");
    parseTree(nd);
    if (point_virgule == 1) {printOut(";\n"); numCurrentLine++;}

    /* La generation est finie. On remet les variables que l'on a utilisee a 0 */
    indStruct = 0;
    ind_in_matrice = 0;
  } else {
    /* On genere les boucles imbriquees */
    /* On genere la partie gauche puis on fait appel a des fonctions pour generer la partie droite */
    /* On regarde le "type" de la partie gauche pour pouvoir la generer */
    switch (ng->typeNode){
      case REF_STRUCT :
      case REF_STRUCT_LIST :
           if (ng->fg->typeNode == IDENTIFIER) {
              /* On met TabStruct a jour */
              TabStruct[indStruct].nom_struct = (ng->fg->valNode).uString;
              TabStruct[indStruct].ident = 0;
              /* fieldX n'a pas de parametres */
              /* On genere alors la suite (field(X+1)....fieldN) */
              generer_ref_struct(ng->fd, nd);
           };
           if (ng->fg->typeNode == IDENTIFIER_PARENTHESE) {
              /* On met TabStruct a jour */
              TabStruct[indStruct].nom_struct = (ng->fg->valNode).uString;
              TabStruct[indStruct].ident = 1;
              /* fieldX a des parametres */
              /* On genere les boucles imbriquees correspondant a fieldX */
              gerer_ident_parent(ng,nd);
           }
           break;
      case FUNCTION_CALL_OU_REF_MATRIX :
        /* On met TabStruct a jour */
        TabStruct[indStruct].nom_struct = (ng->valNode).uString;
        TabStruct[indStruct].ident = 1;
        /* fieldX a des parametres */
        /* On genere les boucles imbriquees correspondant a fieldX */
        gerer_ident_parent(ng,nd);
        break;
      case IDENTIFIER :
           /* On met TabStruct a jour */
           TabStruct[indStruct].nom_struct = (ng->valNode).uString;
           TabStruct[indStruct].ident = 0;
           /* Cas ou on a finit de generer les boucles imbriquees */
           /* Appel a generer_ref_struct pour afficher "ce qu'il y a a l'interieur" */
           generer_ref_struct(ng->fg, nd);
           break;
      case IDENTIFIER_PARENTHESE :
           /* On met TabStruct a jour */
           TabStruct[indStruct].nom_struct = (ng->valNode).uString;
           TabStruct[indStruct].ident = 1;
           /* fieldX a des parametres */
           /* On genere les boucles imbriquees correspondant a fieldX */
           gerer_ident_parent(ng,nd);
           break;
    }
  }
}

/*************************************************/

/*
int matrixDim(Node *n) {
  if (n != NULL ) {
    if (n->typeNode == MATRIX) {
      return (1 + matrixDim(n->fg));
    }
  }
  return 0;
}

/*+ n est ce qu'il y a dans la matrice. On calcule la dimension de cette matrice
 La dimension de cette matrice est egale au nombre d'elements de n +*/
int calculer_dim(Node *n) {

  /* S'il n'y a qu' 1 element (autre qu'une matrice), la dimension est alors de 1 */
  if (n->typeNode != SUITE_MATRIX && n->typeNode != MATRIX) {
    return 1;
  }

  /* Une matrice imbriquee dans une matrice (par exemple[[1 2]]) */
  /* la matrice autour ne compte pas */
  if (n->typeNode == MATRIX && inMatrix == 0) {
    return calculer_dim(n->fg);
  }

  /* S'il n'y a qu'une matrice, la dimension est de 1 */
  if (n->typeNode == MATRIX && inMatrix == 1) {
    return 1;
  } else {
    /* On calcule le nombre d'elements */
    return (1+ calculer_dim(n->fd));
  }
}


/*************************************************/

/*+ Affiche les dimensions entre "," de la matrice contenue dans TabDim
 IDL a au maximum 7 dimensions. On va donc afficher ces 7 dimensions +*/
void afficher_dims(){
  int i, j, tous, nb, divis, r;
  char* strTmp;
  /* Tab1 contient les dimensions a afficher sans les 0 de fin si la matrice est de dimension < a 7 */
  int Tab1[1024];

  /* On affiche les dimensions qui seront dans la fonction creee "reshape_array" */

  i=0;

  /* Cas ou il n'y a qu'une dimension, c-a-d [a] ou a n'est pas une matrice */
  if (indDim == 1) {
    printOut(",1,1,0,0,0,0,0");
  } else {
    r = indDim - 1;
    j = 0;

    /* TabDim contient plusieurs fois la dimension de la matrice
    Par exemple, si la matrice est [[1,2,3],[4,5,6]]
    TabDim aura comme valeur 2 3 3 et les dimensions a afficher seront 2,3
    */
    while(i != r) {
      /* Tab1 contiendra les dimensions finales a afficher ( 2,3 ) */
      /* On fait un calcul pour trouver les dimensions finales */
      nb = TabDim[i];
      tous = r - i;
      divis = tous/nb;
      if (divis == 1) {
        Tab1[j] = nb;
        j++;
        Tab1[j] = TabDim[i+1];
        i=r;
      } else {
        Tab1[j] = nb;
        r = r - (nb-1)*divis;
        i++; j++;
      }
    }

    /* On affiche les dimensions avec les "," */
    for (i=j; i>=0; i--) {
      printOut(",");
      strTmp = (char*) malloc (MAX_STR);
      sprintf(strTmp, "%d", Tab1[i]);
      printOut(strTmp);
      free(strTmp);
    }

    for (i=j+1; i<7; i++) {
      /* On affiche les 0 manquant */
      printOut(",0");
    }

  }
  /* On remet la variable utilisee a 0 */
  indDim = 0;
}

/*******************************************************************************/


/*******************************************************************************/

/*+ afficher_matrix_crochet(n) affiche la matrice n en rajoutant 1 a chaque cellule +*/
void afficher_matrix_crochet(Node *n){
  Node *croch;

  printOut("[");
  /* croch est le noeud contenant les parametres de la matrice */
  croch = n;
  /* On genere la matrice en affichant ses parametres */
  while (croch->typeNode == SUITE_MATRIX) {
    parseTree(croch->fg);
    /* On rajoute 1 */
    printOut("+1,");
    croch = croch->fd;
  }
  /* On affiche le dernier element de la matrice */
  parseTree(croch);
  printOut("+1");
  printOut("]");
}

/*+ Renvoie la dimension de la matrice de n
 Renvoie 1 si n = [x] - 2 si n = [[x]] ... +*/
int getMatrixDim(Node *n) {
  if (n != NULL ) {
    if (n->typeNode == MATRIX) {
      return (1 + getMatrixDim(n->fg));
    } else if ((n->typeNode == SUITE_MATRIX) && (n->fg != NULL)) {
      if (n->fg->typeNode == MATRIX) {
        return (1 + getMatrixDim(n->fg->fg));
      }
    }
  }
  return 0;
}


/*************************************************/

/*************************************************/
/*************************************************/
/*****  FIN Des REF STRUCT                       */
/*************************************************/
/*************************************************/

/*+ inserer_tuer(str) cherche s'il existe un bloc de nom "str" dans la table des symboles
 Si c'est le cas, il enleve le bloc sinon, il ne fait rien +*/
void inserer_tuer(char *str, int typeRec) {
/* Si l'identificateur est deja present, on l'enleve  */
  recType *resultat, *rec;

  resultat = malloc(sizeof(recType));
  if (find(str,resultat) != STATUS_OK) {
    rec = malloc(sizeof(recType));
    rec->name = malloc(strlen(str)+1);
    rec->ptype = malloc(sizeof(type));
    strcpy(rec->name, str);
    rec->ptype->num_type = typeRec;
    insert(rec);
  }
  free(resultat);
}

/****************************************************
**  3 fonction de generation d entete de fonction  **
*****************************************************/
/*+ ecrit la premiere ligne de commande de traitement des parametres +*/
void parseFuncParam1(Node* n) {
  char* strTmp;
  unsigned char c;

  if (n != NULL) {
    if (n->typeNode == SUITE_PARAM) {
      parseFuncParam1(n->fg);
      printOut(", ");
      parseFuncParam1(n->fd);
    }
    if (n->typeNode == PARAM) {
      if (n->fd == NULL) {
        strTmp = (char*) malloc (10);
        c = 'a' + numVar / 10;
        sprintf(strTmp, "'I2M_%c%d'",c, numVar % 10);
        printOut(strTmp);
        free(strTmp);
        numVar++;
        if (numVar % 10 == 0) {
          numVar++;
        }
      } else {
        strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
        sprintf(strTmp,"'%s'",(n->fg->valNode).uString);
        printOut(strTmp);
        free(strTmp);
      }
    }
  }
}

/*+ ecrit la 2eme ligne de commande de traitement des parametres +*/
void parseFuncParam2(Node* n) {
  char strTmp2[256];
  recType* resultat;
  char* strTmp;

  if (n != NULL) {
    if (n->typeNode == SUITE_PARAM) {
      parseFuncParam2(n->fg);
      printOut(", ");
      parseFuncParam2(n->fd);
    }
    if (n->typeNode == PARAM) {
      if (n->fd == NULL) { /* cas de la variable */
        sprintf(strTmp2, "RESERVED %s",(n->fg->valNode).uString);
        resultat = malloc(sizeof(recType));
        if (find(strTmp2,resultat) == STATUS_OK) { /* renommage ? */
          if ((resultat->ptype->num_type == RESERVED_VAR) ||
              (resultat->ptype->num_type == RESERVED)) {
              sprintf(strTmp2, "M2I_%s",(n->fg->valNode).uString);
              strcpy((n->fg->valNode).uString, strTmp2);
          }
        }
        free(resultat); /* fin renommage */
        inserer_tuer((n->fg->valNode).uString, VARIABLE);
        strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
        sprintf(strTmp,"'%s'",(n->fg->valNode).uString);
        printOut(strTmp);
        free(strTmp);
      } else {
        /* renommage ? */
        sprintf(strTmp2, "RESERVED %s",(n->fd->valNode).uString);
        resultat = malloc(sizeof(recType));
        if (find(strTmp2,resultat) == STATUS_OK) {
          if ((resultat->ptype->num_type == RESERVED_VAR) ||
              (resultat->ptype->num_type == RESERVED)) {
              sprintf(strTmp2, "M2I_%s",(n->fd->valNode).uString);
              strcpy((n->fd->valNode).uString, strTmp2);
          }
        }
        free(resultat); /* fin renommage */
        strTmp = (char*) malloc (MAX_STR + strlen((n->fd->valNode).uString));
        sprintf(strTmp,"'%s'",(n->fd->valNode).uString);
        printOut(strTmp);
        free(strTmp);
        inserer_tuer((n->fd->valNode).uString, VARIABLE);
      }
    }
  }
}

/*+ ecrit la 3eme ligne de commande de traitement des parametres +*/
void parseFuncParam3(Node* n) {
  char* strTmp;

  if (n != NULL) {
    if (n->typeNode == SUITE_PARAM) {
      parseFuncParam3(n->fg);
      parseFuncParam3(n->fd);
    }
    if (n->typeNode == PARAM) {
      if (n->fd == NULL) { /* PAS BESOIN DE RENOMMAGE CAR DEJA FAIT AVANT */
        strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
        sprintf(strTmp,"%s=[]; ",(n->fg->valNode).uString);
        printOut(strTmp);
        free(strTmp);
      } else {
        strTmp = (char*) malloc (MAX_STR + strlen((n->fd->valNode).uString));
        sprintf(strTmp,"%s=[]; ",(n->fd->valNode).uString);
        printOut(strTmp);
        free(strTmp);
      }
    }
  }
}


/**********************************************
** 3 Fonctions de generation d appel de procedure
***********************************************/
/*+ parse les parametres d un appel de procedure et ecrit le recepteur de resultats +*/
int parseProCallParam1(Node* n, int debut) {
/*+ debut vaut 1 si on a encore rien ecrit dans le fichier cible et 0 apres +*/
/*+ cela permet de voir quand il faut ecrire le premier [ +*/
int nextDebut;
char* strTmp;
char strTmp2[256];
recType* resultat;

  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      nextDebut = parseProCallParam1(n->fd, debut);
      nextDebut = parseProCallParam1(n->fg, nextDebut);
      return nextDebut;
    } else if (n->typeNode == IDENTIFIER) {
      if (debut) {
        if (n->fg != NULL) {	/* cas du keyword */
          if (n->fg->typeNode == IDENTIFIER) {
            strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
            /* renommage ? */
            sprintf(strTmp2, "RESERVED %s",(n->fg->valNode).uString);
            resultat = malloc(sizeof(recType));
            if (find(strTmp2,resultat) == STATUS_OK) {
              if ((resultat->ptype->num_type == RESERVED_VAR) ||
                  (resultat->ptype->num_type == RESERVED)) {
                  sprintf(strTmp2, "M2I_%s",(n->fg->valNode).uString);
                  strcpy((n->fg->valNode).uString, strTmp2);
              }
            }
            free(resultat); /* fin renommage */
            sprintf(strTmp2, "REFPARAM %s",(n->fg->valNode).uString);
            inserer_tuer(strTmp2, REFPARAM);
            sprintf(strTmp, "[%s", (n->fg->valNode).uString);
          } else { /* on a pas ecrit en repart avec le meme debut */
            return debut;
          }
        } else {  /* cas passage de variable */
          /* renommage ? */
          sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
          resultat = malloc(sizeof(recType));
          if (find(strTmp2,resultat) == STATUS_OK) {
            if ((resultat->ptype->num_type == RESERVED_VAR) ||
                (resultat->ptype->num_type == RESERVED)) {
                sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
                strcpy((n->valNode).uString, strTmp2);
            }
          }
          free(resultat); /* fin renommage */
          sprintf(strTmp2, "REFPARAM %s",(n->valNode).uString);
          inserer_tuer(strTmp2, REFPARAM);
          strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
          sprintf(strTmp, "[%s", (n->valNode).uString);
        }
        printOut(strTmp);
        free(strTmp);
        return 0;
      } else { /* cas ou on a deja ecris qq chose */
        if (n->fg != NULL) {	/* cas du keyword */
          if (n->fg->typeNode == IDENTIFIER) {
            strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
            /* renommage ? */
            sprintf(strTmp2, "RESERVED %s",(n->fg->valNode).uString);
            resultat = malloc(sizeof(recType));
            if (find(strTmp2,resultat) == STATUS_OK) {
              if ((resultat->ptype->num_type == RESERVED_VAR) ||
                  (resultat->ptype->num_type == RESERVED)) {
                  sprintf(strTmp2, "M2I_%s",(n->fg->valNode).uString);
                  strcpy((n->fg->valNode).uString, strTmp2);
              }
            }
            sprintf(strTmp2, "REFPARAM %s",(n->fg->valNode).uString);
            if (find(strTmp2,resultat) != STATUS_OK) {
              inserer_tuer(strTmp2, REFPARAM);
              sprintf(strTmp, ", %s", (n->fg->valNode).uString);
            } else {
              free(resultat); /* fin renommage */
              free(strTmp);
              return debut;
            }
            free(resultat); /* fin renommage */
          } else {
            return debut;
          }
        } else {
          /* renommage ? */
          sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
          resultat = malloc(sizeof(recType));
          if (find(strTmp2,resultat) == STATUS_OK) {
            if ((resultat->ptype->num_type == RESERVED_VAR) ||
                (resultat->ptype->num_type == RESERVED)) {
                sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
                strcpy((n->valNode).uString, strTmp2);
            }
          }
          strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
          sprintf(strTmp2, "REFPARAM %s",(n->valNode).uString);
          if (find(strTmp2,resultat) != STATUS_OK) {
            inserer_tuer(strTmp2, REFPARAM);
            sprintf(strTmp, ", %s", (n->valNode).uString);
          } else {
            free(resultat); /* fin renommage */
            free(strTmp);
            return debut;
          }
          free(resultat); /* fin renommage */
        }
        printOut(strTmp);
        free(strTmp);
        return 0;
      }
    }
  }
  return debut;
}

/*+ parse les parametres d un appel de procedure et ecrit les parametres formelles de l appel
 debut indique si c est le premier parametre =0 - dans ce cas pas de , +*/
int parseProCallParam2(Node* n, int debut) {
char* strTmp;
recType *scriptRec;  	/* decrit un script dans la table des symboles */
recType* tableSymbNode;
int nextDebut;
char strTmp2[256];
recType* resultat;
int oldNumVar;
unsigned char c;

  nextDebut = debut;

  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      nextDebut = parseProCallParam2(n->fg, nextDebut);
      nextDebut = parseProCallParam2(n->fd, nextDebut);
      return nextDebut;
    } else {
      if (nextDebut == 1) { /* un premier parametre a ete ecrit */
        printOut(", ");
      }
      if (n->typeNode == IDENTIFIER) {
        if (n->fg == NULL) {  /* cas du passage de variable */
          strTmp = (char*) malloc (10);
          c = 'a' + numVar / 10;
          sprintf(strTmp, "'I2M_%c%d', ",c, numVar % 10);
          printOut(strTmp);
          free(strTmp);
          /* renommage ? */
          if ( inRef_struct_list == 0 ) {
          sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
          resultat = malloc(sizeof(recType));
          if (find(strTmp2,resultat) == STATUS_OK) {
            if ((resultat->ptype->num_type == RESERVED_VAR) ||
                (resultat->ptype->num_type == RESERVED)) {
                sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
                strcpy((n->valNode).uString, strTmp2);
            }
          }
          free(resultat); /* fin renommage */
          }
          strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
          strcpy(strTmp, (n->valNode).uString);
          tableSymbNode = malloc(sizeof(recType));
          if (find(strTmp,tableSymbNode) != STATUS_OK) {  /* on regarde si la var est declaree */
            if (printOnOff == 0) {
              strcat(undeclaredVariables,(n->valNode).uString);
              strcat(undeclaredVariables,"=1; ");
            }
            printOut(strTmp);
            inserer_tuer((n->valNode).uString, VARIABLE); /* on met l identifier dans la TS */
          } else {
            sprintf(strTmp, "%s",(n->valNode).uString);
            printOut(strTmp);
          }
          free(strTmp);
          numVar++;
          if (numVar % 10 == 0) {
            numVar++;
          }
          nextDebut = 1; /* on a ecrit quelque chose */
        } else {    /* cas du passage par keyword */
          strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
          sprintf(strTmp, "'%s', ",(n->valNode).uString);
          printOut(strTmp);
          free(strTmp);
          if (n->fg->typeNode == IDENTIFIER) {
            /* renommage ? */
            sprintf(strTmp2, "RESERVED %s",(n->fg->valNode).uString);
            resultat = malloc(sizeof(recType));
            if (find(strTmp2,resultat) == STATUS_OK) {
              if ((resultat->ptype->num_type == RESERVED_VAR) ||
                  (resultat->ptype->num_type == RESERVED)) {
                  sprintf(strTmp2, "M2I_%s",(n->fg->valNode).uString);
                  strcpy((n->fg->valNode).uString, strTmp2);
              }
            }
            free(resultat); /* fin renommage */
            strTmp = (char*) malloc (MAX_STR + strlen((n->fg->valNode).uString));
            sprintf(strTmp, "%s",(n->fg->valNode).uString);
            tableSymbNode = malloc(sizeof(recType));
            if (find(strTmp,tableSymbNode) != STATUS_OK) {  /* on regarde si la var est declaree */
              if (printOnOff == 0) {
                strcat(undeclaredVariables,(n->fg->valNode).uString);
                strcat(undeclaredVariables,"=1; ");
              }
              inserer_tuer((n->fg->valNode).uString, VARIABLE); /* on met l identifier dans la TS */
              printOut(strTmp);
            } else {
              printOut(strTmp);
            }
            free(tableSymbNode);
            free(strTmp);
          } else {
            parseTree(n->fg); /* on ecrit l'expression */
          }
          nextDebut = 1; /* on a ecrit quelque chose */
        }
      } else if (n->typeNode == SUITE_CALL) { /* cas du /keyword */
        strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
        sprintf(strTmp, "'%s', 1",(n->valNode).uString);
        printOut(strTmp);
        free(strTmp);
        nextDebut = 1;
      } else { /* on a autre chose comme une expression */
        strTmp = (char*) malloc (10);
        c = 'a' + numVar / 10;
        sprintf(strTmp, "'I2M_%c%d', ",c, numVar % 10);
        printOut(strTmp);
        free(strTmp);
        oldNumVar = numVar;
        parseTree(n); /* on ecrit l'expression */
        numVar = oldNumVar;
        nextDebut = 1;
        numVar++;
        if (numVar % 10 == 0) {
          numVar++;
        }
      }
    }
  }
  return nextDebut;
}

/* renvoie le nombre de parametres de la procedure a traduire */
int nbParamPro(Node* n)
{
  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      return nbParamPro(n->fg) + nbParamPro(n->fd);
    } else {
      return 1;
    }
  }
  return 0;
}

/*+ parse les parametres d un appel de procedure et termine la liste de param avec I2M_pos +*/
int parseProCallParam3(Node* n, int debut) {
char* strTmp;
char strTmp2[256];
int nextDebut;
recType* resultat;

nextDebut = debut;

  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      nextDebut = parseProCallParam3(n->fd,nextDebut);
      nextDebut = parseProCallParam3(n->fg,nextDebut);
      return nextDebut;
    } else if (n->typeNode == IDENTIFIER) {
      if (n->fg == NULL) { /* on a une variable par adresse */
        sprintf(strTmp2, "REFPARAM %s",(n->valNode).uString);
        resultat = (recType*) malloc (sizeof(recType));
        if (find(strTmp2,resultat) != STATUS_OK) {
          inserer_tuer(strTmp2, REFPARAM);
        } else {
          free(resultat);
          numVar++;
          return nextDebut;
        }
        free(resultat);
        if (nextDebut) {
          nextDebut=0;
        } else {
          printOut(", ");
        }
        strTmp = (char*) malloc (10);
        sprintf(strTmp, "%d",numParamInCurProc - numVar + 1);
        printOut(strTmp);
        free(strTmp);
      } else {
        if (n->fg->typeNode == IDENTIFIER) { /* on a un Keyword par adresse */
          sprintf(strTmp2, "REFPARAM %s",(n->fg->valNode).uString);
          resultat = (recType*) malloc (sizeof(recType));
          if (find(strTmp2,resultat) != STATUS_OK) {
            inserer_tuer(strTmp2, REFPARAM);
            free(resultat);
          } else {
            free(resultat);
            numVar++;
            return nextDebut;
          }
          if (nextDebut) {
            nextDebut=0;
          } else {
            printOut(", ");
          }
          strTmp = (char*) malloc (10);
          sprintf(strTmp, "%d",numParamInCurProc - numVar + 1);
          printOut(strTmp);
          free(strTmp);
        }
      }
    }
  }
  numVar++;
  return nextDebut;
}


/*+ parse les parametres d un appel de procedure et verifie la presence de keywords
 renvoie 1 si l'appel de fonction contient un keyword +*/
int containsKeywords(Node* n) {
  int result;

  result=0;
  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      result = containsKeywords(n->fg);
      if (result == 0) {
      	result = containsKeywords(n->fd);
      }
      return result;
    } else if (n->typeNode == IDENTIFIER) {
      if (n->fg == NULL) { /* on a une variable */
      	result=0;
      } else {	/* on a un Keyword  */
      	result = 1;
      }
    } else if (n->typeNode == SUITE_CALL) { /* cas du /keyword */
      	result = 1;
    }
  }
  return result;
}

/*+ parse les parametres d un appel de procedure et verifie la presence de variables
 renvoie 1 si l'appel de fonction contient une variable passee par adresse +*/
int containsVariable(Node* n) {
  int result;

  result=0;
  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      result = containsVariable(n->fg);
      if (result == 0) {
      	result = containsVariable(n->fd);
      }
      return result;
    } else if (n->typeNode == IDENTIFIER) {
      if (n->fg == NULL) { /* on a une variable */
      	result = 1;
      } else {	/* on a un Keyword  */
        if (n->fg->typeNode == IDENTIFIER) { /* on a un Keyword par adresse */
      	  result = 1;
      	}
      }
    }
  }
  return result;
}


/*+ renvoie 0 si les parametres sont simples et 1 sinon
 c est a dire pas de keyword et pas de passage par adresse +*/
int parseProCallParam4(Node* n, int result) {
char* strTmp;
int nextResult;

  nextResult = result;
  if (n != NULL) {
    if (n->typeNode == SUITE_CALL_LIST) { /* si c est un noeud suite call on sonde les fils */
      nextResult = parseProCallParam4(n->fg,nextResult);
      nextResult = parseProCallParam4(n->fd,nextResult);
    } else if (n->typeNode == IDENTIFIER) { /* cas du keyword et de la variable */
      nextResult = 1;
    } else if (n->typeNode == SUITE_CALL) { /* cas du /keyword */
      nextResult = 1;
    }
  }
  return nextResult;
}


/*+ parse le noeud a la recherche de variables systemes
 le parametre indique le nombre de var globales +*/
int parseForVarSystem(Node* n, int nbVar) {
  int result = nbVar;
  recType *resultat, *rec;
  char* strTmp;

  if (n != NULL) {
    if (n->typeNode == VAR_SYSTEM) {
      if (n->fg != NULL) { /* on a une structure */
        strTmp = (n->fg->fg->valNode).uString;
      } else {
        strTmp = (n->valNode).uString;
      }
      if (strcmp(strTmp,"stime") != 0) {
        if (result == 0) {
          printTab();
          printOut("global");
        }
        resultat = malloc(sizeof(recType));
        if (find(strTmp,resultat) != STATUS_OK) {
          rec = malloc(sizeof(recType));
          rec->name = malloc(strlen(strTmp)+1);
          rec->ptype = malloc(sizeof(type));
          strcpy(rec->name, strTmp);
          rec->ptype->num_type = SYSVAR;
          insert(rec);
          if (strcmp(strTmp,"err_string") == 0) {
             /* On traite !ERR_STRING */
             if (find("error",resultat) == STATUS_OK) {
                /* On a deja rencontre la VS !ERROR donc i2mvs_error_state est deja declaree en global */
             } else {
                printOut(" i2mvs_error_state");
             }
          } else {
             if (strcmp(strTmp,"error") == 0) {
                /* On traite !ERROR */
                if (find("err_string",resultat) == STATUS_OK) {
                   /* On a deja rencontre la VS !ERR_STRING donc i2mvs_error_state est deja declaree en global */
                } else {
                   printOut(" i2mvs_error_state");
                }
             } else {
                /* Cas normal */
                printOut(" i2mvs_");
                printOut(strTmp);
             }
          }
          result++;
        }
      }
    }
    result = parseForVarSystem(n->fg, result);
    result = parseForVarSystem(n->fd, result);
  }
  return result;
}

/*+ Parse le noeud pour detecter tous les appels de variable systeme +*/
void printVarSystemCalled(Node* n) {
  deleteObjetOfType(SYSVAR);
  if (parseForVarSystem(n,0) > 0) {
    printOut(" % SYSTEM VARIABLES\n\n");
    numCurrentLine += 2;
    deleteObjetOfType(SYSVAR);
  }
}

/*+ Parse le noeud pour detecter tous les appels de variable non declaree +*/
void printUndeclaredVariables(Node* n) {
  printOnOff = 0;
  strcpy(undeclaredVariables,"");
  numVar=1;
  catchCount = 0;
  if (n->typeNode == DECLARATION_PROC) {
    if (n->fg != NULL) { /* la procedure a des parametres */
      inProcWithParam = 1; /* on est dans une procedure avec des parametres*/
    } else {
      inProcWithParam = 0;
    }
  } else {
    inFunction = 1;
  }
  if (n->fg != NULL) { /* la procedure a des parametres */
    parseFuncParam1(n->fg);
    parseFuncParam2(n->fg);
    parseFuncParam3(n->fg);
  }
  parseTree(n->fd);
  printOnOff = 1;
}


/*****************************************
** Fonctions de test sur les noeud
*******************************************/

/*+ renvoie 1 si le node est un refmatrix et pas un fonctioncall +*/
int isVariable(Node* n) {
  int result;
  recType* tableSymbNode;

  tableSymbNode = malloc(sizeof(recType));
  result = 0;
  if (n != NULL) {
    if (n->typeNode == FUNCTION_CALL_OU_REF_MATRIX) {
      if (find((n->valNode).uString,tableSymbNode) == STATUS_OK) {
        if (tableSymbNode->ptype->num_type == VARIABLE) {
          result = 1;
        }
      }
    }
  }
  if (tableSymbNode != NULL) {free(tableSymbNode);}
  return result;
}



/*+ renvoie 1 si le node est un refmatrix et pas un fonctioncall +*/
int isMatrix(Node* n) {
  int result;
  recType* tableSymbNode;

  tableSymbNode = malloc(sizeof(recType));
  result = 0;
  if (n != NULL) {
    if (n->typeNode == FUNCTION_CALL_OU_REF_MATRIX) {
      if (find((n->valNode).uString,tableSymbNode) == STATUS_OK) {
          result = 1;
      }
    }
  }
  if (tableSymbNode != NULL) {free(tableSymbNode);}
  return result;
}

/* renvoie 1 si le noeud est un "functionCall" +*/
int isNodeFunctionCall(Node* n) {
  recType *resultat;

  int result;

  result = 0;
  if (n != NULL) {
    if (n->typeNode == FUNCTION_CALL) {
      result = 1;
    } else if (n->typeNode == FUNCTION_CALL_OU_REF_MATRIX) {
      if (n->fd == NULL) {
        resultat = malloc(sizeof(recType));
        erreurHashTable = find((n->valNode).uString, resultat);
        /* On regarde si le type du bloc est une matrice ou un appel de fonction */
        if (erreurHashTable == STATUS_OK && ((resultat->ptype)->num_type == VARIABLE)) {
          result = 0;
        } else {
          result = 1;
        }
        free(resultat);
      } else {
        result=0;
      }
    }
  }
  return result;
}


/* renvoie 1 si le node contient dans ses fils un fonctioncall et 0 sinon*/
int containsFunctionCall(Node* n) {
int result = 0;
  if (n != NULL) {
    if (n->typeNode == FUNCTION_CALL) {
      return 1;
    }
    if (n->typeNode == FUNCTION_CALL_OU_REF_MATRIX) {
      if (isMatrix(n)){
      } else {
        return 1;
      }
    }
    result = containsFunctionCall(n->fg);
    if (result == 1) {return 1;}
    return containsFunctionCall(n->fd);
  }
  return 0;
}


/**********************************************/
/**  FONCTIONS TRAVAILLANTS SUR LES CHAINES ***/
/**********************************************/

/*+ remplace toute les apostrophes de strIn en double apostrophe et stocke le resultat dans strOut +*/
void doubleApostrophe (char* strIn, char* strOut) {
int i;
char* strTmp;

  strcpy(strOut,"");
  i =0;
  while (*strIn != '\0') {
    if (*strIn == '\'') {
      strOut[i] = '\'';
      i++;
      strOut[i] = '\'';
    } else {
      strOut[i] = *strIn;
    }
    strIn++;i++;
  }
  strOut[i] = '\0';
  strTmp = (char*) malloc (strlen(strOut)+3);
  sprintf(strTmp, "'%s'", strOut);
  strcpy(strOut, strTmp);
  free(strTmp);
}


/*+ Traitement du Catch +*/
void gestionCatch (int oldCatchCount, Node* n) {
int oldCatchCount2;
char strTmp2[256];

  while (oldCatchCount < catchCount) { /* on catch le try */
    decTab();
    printOut("\n");
    printTab();
    printOut("catch,\n");
    incTab();
    incTab();
    numCurrentLine+=2;
    if (catchVar[catchCount] != NULL) {
      oldCatchCount2 = catchCount;
      parseTree(catchExecNode[catchCount]);
      if (oldCatchCount2 < catchCount) {
        gestionCatch(oldCatchCount2, n);
      }
    } else {
      sprintf(strTmp2, "Line %d => Label <%s> not found at the same level than on_ioerror at line %d in %s\n", n->lineInSource, (catchExecNode[catchCount]->valNode).uString ,numCurrentLine, outFile);
      printToLog(strTmp2);
    }
    catchCount--;
    decTab();
    decTab();
    printTab();
    if (inScilabTranslation==0) {
		    printOut("end; %catch\n");
	}
	else {
			printOut("end; //catch\n");
	}
	numCurrentLine ++;
  } /* fin de traitement du catch */
}


/*+ retourne 1 si le if etait celui d un catch et le traite par la meme occasion +*/
int testIfCatch(Node* n) {
char strTmp2[256];

  if (n->fg != NULL) { /* test des cas  catchVar EQ 0 et catchVar NE 0 */
    if ((n->fg->fg != NULL) && (catchVar[catchCount] != NULL)) { /* on regarde si le if est le if du CATCH */
      if ((n->fg->fg->typeNode == IDENTIFIER) &&
          (strcmp((n->fg->fg->valNode).uString,(catchVar[catchCount]->valNode).uString)==0)) {
        if ((n->fg->typeNode == NE) && (n->fg->fd->typeNode == INTEGER)) {
          if ((n->fg->fd->valNode).uInt == 0) { /* Cas du CatchVar NE 0 */
            catchExecNode[catchCount] = n->fd->fg; /* on stocke le traitement du catch pour plus tard */
            if (n->fd->fd != NULL) { /* On a un else */
              printOut("\n");
              numCurrentLine++;
              parseTree(n->fd->fd->fg);
            }
            return 1;
          }
        } else if ((n->fg->typeNode == EQ) && (n->fg->fd->typeNode == INTEGER)) {
          if ((n->fg->fd->valNode).uInt == 0) { /* Cas du CatchVar EQ 0 */
            printOut("\n");
            numCurrentLine++;
            parseTree(n->fd->fg); /* On parse le then */
            if (n->fd->fd != NULL) { /* On a un else */
              catchExecNode[catchCount] = n->fd->fd->fg;
            }
            return 1;
          }
        } else if (n->fg->typeNode == Not) {
          /* Cas du Not CatchVar */
          printOut("\n");
          numCurrentLine++;
          parseTree(n->fd->fg); /* On parse le then */
          if (n->fd->fd != NULL) { /* On a un else */
            catchExecNode[catchCount] = n->fd->fd->fg;
          }
          return 1;
        } else {
          sprintf(strTmp2, "Line %d => Unknown translation of catch statement - only X EQ/NE 0 or X/notX  are handled -  at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
          printToLog(strTmp2);
          return 0;
        }
      }
    } else if (catchVar[catchCount] != NULL) {
      if ((n->fg->typeNode == IDENTIFIER) &&
          (strcmp((n->fg->valNode).uString,(catchVar[catchCount]->valNode).uString)==0)) {
        catchExecNode[catchCount] = n->fd->fg; /* on stocke le traitement du catch pour plus tard */
        if (n->fd->fd != NULL) { /* On a un else */
          printOut("\n");
          numCurrentLine++;
          parseTree(n->fd->fd->fg);
        }
        return 1;
      }
    }
  }
  return 0;
}

char strTmp2[256];

/*+ fonction recursive traduisant un noeud precis
 ( traduction des cas de grammaire ) +*/
void parseTree(Node *n) {
 Node* nodeTmp;
 Node* ioErrNode = NULL;
 char* strTmp;
 char* strTmp3;
 int i, nbdim;
 statusEnum err;
 recType* tableSymbNode;
 recType* cherch;
 recType* resultat;
 recType *scriptRec;  	/*+ decrit un script dans la table des symboles +*/
 recType *rec;
 int appelFonctionTmp = 0;	      	/*+ vaut 1 ou 0 selon la position de l'appel +*/
 unsigned char oldCatchCount;
 unsigned char oldIoErrCount;

 if (n!=NULL) {
  /* printf("node %d - ",n->typeNode );  printf("line = %d\n",n->lineInSource);/* */
  switch(n->typeNode){
    case BLOCK:
      parseTree(n->fg);
      parseTree(n->fd);
      break;
    case DECLARATION_LIST:
      parseTree(n->fg);
      parseTree(n->fd);
      break;
    case  DECLARATION_PROC:
      if (oneFunctionTranslation == 1) { /* on ne traduit qu une fonction */
        if (strcmp(oneFunctionName, (n->valNode).uString) != 0) {
          return;
        }
      }
      deleteObjetOfType(VARIABLE); /* on clean la table des symboles */
      if (printOnOff == 1) {
        printUndeclaredVariables(n);
      }
      deleteObjetOfType(VARIABLE); /* on clean la table des symboles */
      fileTranslationError = 0; /* aucune erreur detectee */
      nbWarningFile = 0;
      sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
      resultat = malloc(sizeof(recType));
      if (find(strTmp2,resultat) == STATUS_OK) {
        if ((resultat->ptype->num_type == RESERVED_FUNCTION) ||
            (resultat->ptype->num_type == RESERVED)) {
            /* la nom de la procedure est reserve => renommage */
          sprintf(strTmp2, "Line %d => Procedure <%s> renamed in <M2I_%s>\n", n->lineInSource, (n->valNode).uString, (n->valNode).uString);
          printToLog(strTmp2);
          sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
          strcpy((n->valNode).uString, strTmp2);
        }
      }
      free(resultat);
			if (inScilabTranslation==0) { /* traduction en matlab */
				    sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, (n->valNode).uString);
			}
			else { /* traduction en scilab */
					sprintf(outFile,"%s%c%s.sci",outDir, PATHSEP, (n->valNode).uString);
			}
      /* sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, (n->valNode).uString);*/
      if (displayMessage == 1) {
        if (strlen(outFile) <= 30) {
          fprintf (stderr,"Creating %-30s", outFile) ;
        } else {
          strncpy(strTmp2, outFile, 27);
          strTmp2[27] = '\0';
          fprintf (stderr,"Creating %27s...", strTmp2) ;
        }
        if (nb_lines > 0) {
          fprintf(stderr, ">%3d %s%s", (n->lineInSource *100 / nb_lines),commentaire,commentaire);
        }
      }
      if (pOutputFile != NULL) {
        fclose(pOutputFile);
      }
      pOutputFile = fopen(outFile, "w"); /* on ecrase le fichier cible */
      if (pOutputFile == NULL) {
        if (displayMessage == 1) {
	        fprintf (stderr,"Unable to write in the output file %s\n", outFile) ;
	      }
        exit(1);
      }
      /* on insere une entete en debut de fichier cible */
      catchCount = 0;
			if (inScilabTranslation==0) { /* traduction en matlab */
	          		printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
      				sprintf(strTmp2, "%%%% File generated by IDL2Matlab %s %%%%\n", I2M_VERSION_2);
     				printOut(strTmp2);
      				printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
			}
			else { /* traduction en scilab */
					printOut("////////////////////////////////////////////////\n");
      				sprintf(strTmp2, "//// File generated by IDL2Matlab %s////\n", I2M_VERSION_2);
     				printOut(strTmp2);
      				printOut("////////////////////////////////////////////////\n");
			}
      printOut("\n");
      numCurrentLine = 5;
      /* commentaires */
      if (n->fg != NULL) {
        insertComment(realLineInSource(n->fg));
      } else if (n->fd != NULL) {
        insertComment(realLineInSource(n->fd));
      } else {
        insertComment(n->lineInSource);
      }
      numVar=1;
      tab=0;
      printTab();
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      if (n->fg != NULL) { /* la procedure a des parametres */
        sprintf(strTmp, "function [varargout] = %s(varargin)\n", (n->valNode).uString);
        inProcWithParam = 1; /* on est dans une procedure avec des parametres*/
      } else {
        sprintf(strTmp, "function %s()\n", (n->valNode).uString);
        inProcWithParam = 0;
      }
      printOut(strTmp);
      numCurrentLine++;
      free(strTmp);
      incTab(); /* on tabule les statements */

      if (n->fg != NULL) { /* la procedure a des parametres */
        /* l'entete est ecrite => on ecrit la construction des tables */

       	printOut("\n");
        numCurrentLine++;
        printOut(commentaire);
        printOut(commentaire);
        printOut("Initialization of parameters\n");
        numCurrentLine++;
        printTab();
        /* choix de la structure suivant le langage de traduction*/
        if (inScilabTranslation==0) {
                printOut("I2Mkwn=char(");
                parseFuncParam1(n->fg);
        		printOut(", 'I2M_pos');\n");
		}
		else {
				printOut("I2Mkwn=[");
				parseFuncParam1(n->fg);
        		printOut(", 'I2M_pos'];\n");
		}
        numCurrentLine++;
        printTab();
        if (inScilabTranslation==0) {
		        printOut("I2Mkwv={");
		        parseFuncParam2(n->fg);
   				printOut(", 'I2M_pos'};\n");
		}
		else {
				printOut("I2Mkwv=[");
				parseFuncParam2(n->fg);
       			printOut(", 'I2M_pos'];\n");
		}
        numCurrentLine++;
        printTab();
        parseFuncParam3(n->fg);
        printOut("I2M_pos=[];\n");
        printOut(strIdl2Matlab);
		printOut("\n");
		printOut(commentaire);
        printOut(commentaire);
        printOut("End of parameters initialization\n\n");
        numCurrentLine += 8;
      } else {
        printOut("\n");
        numCurrentLine++;
      }
      printVarSystemCalled(n->fd);
      if (strlen(undeclaredVariables) > 0) {
        printTab();
        printOut(commentaire);
        printOut(" Creation of undeclared variables of functions parameters\n");
        printTab();
        printOut(undeclaredVariables);
        printOut("\n\n");
        numCurrentLine+=3;
      }
      parseTree(n->fd);
      gestionCatch(0, n);
      decTab();
      printOut("\n");
      numCurrentLine ++;
      printTab();
      if (n->fg != NULL) { /* la procedure a des parametres */
      if (inScilabTranslation==0) {
            printOut("if ~isempty(I2M_out),eval(I2M_out);end;\n ");
            numCurrentLine ++;
      } else {
           printOut("if ~(isempty(I2M_out)); execstr([I2M_out]); else varargout(1)=[]; end \n");
           numCurrentLine ++;
      }
      printTab();
      printOut("return;\n");
      numCurrentLine ++;
      }
      else{
      	printOut("return;\n");
        numCurrentLine ++;
      }
      /*numCurrentLine += 2;*/
      insertComment(n->lineInSource);
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      sprintf(strTmp, "%s%s end of function %s\n",commentaire,commentaire, (n->valNode).uString);
      /*numCurrentLine += 3; /* ?? pourquoi 3  ?? */
      printOut(strTmp);
      numCurrentLine ++;
      free(strTmp);
      if (displayMessage == 1) {
        fprintf(stderr, "%c%c%c%c%c%c", 8,8,8,8,8,8);

        fprintf (stderr,"==> OK - %4d lines", numCurrentLine) ;
        if (fileTranslationError) {
          fprintf (stderr," - %2d WARNING", nbWarningFile);
        }
        fprintf (stderr,"\n");
      }
      nbLinesTotal += numCurrentLine;
      nbGeneratedFile++;
      nbWarning += nbWarningFile;
      inProcWithParam = 0;
			if (inScilabTranslation==0) { /* traduction en matlab */
					          sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, "main");
						}
						else { /* traduction en scilab */
										sprintf(outFile,"%s%c%s.sci",outDir, PATHSEP, "main");
						}
      if (pOutputFile != NULL) {
        fclose(pOutputFile);
      }
      pOutputFile = fopen(outFile, "w+"); /* on ecrase le fichier cible */
      break;
    case  DECLARATION_FUNC:
      if (oneFunctionTranslation == 1) { /* on ne traduit qu une fonction */
        if (strcmp(oneFunctionName, (n->valNode).uString) != 0) {
          return;
        }
      }
      deleteObjetOfType(VARIABLE); /* on clean la table des symboles */
      if (printOnOff == 1) {
        printUndeclaredVariables(n);
      }
      deleteObjetOfType(VARIABLE); /* on clean la table des symboles */
      fileTranslationError = 0; /* aucune erreur detecte */
      nbWarningFile = 0;
      /* Test du nom de la fonction */
      sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
      resultat = malloc(sizeof(recType));
      if (find(strTmp2,resultat) == STATUS_OK) {
        if ((resultat->ptype->num_type == RESERVED_FUNCTION) ||
            (resultat->ptype->num_type == RESERVED)) {
            /* la nom de la procedure est reserve => renommage */
          sprintf(strTmp2, "Line %d => Function <%s> renamed in <M2I_%s>\n", n->lineInSource, (n->valNode).uString, (n->valNode).uString);
          printToLog(strTmp2);
          sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
          strcpy((n->valNode).uString, strTmp2);
        }
      }
      free(resultat);
			if (inScilabTranslation==0) { /* traduction en matlab */
					          sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, (n->valNode).uString);
						}
						else { /* traduction en scilab */
										sprintf(outFile,"%s%c%s.sci",outDir, PATHSEP, (n->valNode).uString);
						}
      /*sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, (n->valNode).uString);*/
      if (displayMessage == 1) {
        if (strlen(outFile) <= 30) {
          fprintf (stderr,"Creating %-30s", outFile) ;
        } else {
          strncpy(strTmp2, outFile, 27);
          strTmp2[27] = '\0';
          fprintf (stderr,"Creating %27s...", strTmp2) ;
        }
        if (nb_lines > 0) {
          fprintf(stderr, ">%3d %s%s", (n->lineInSource *100 / nb_lines),commentaire,commentaire);
        }
      }
      if (pOutputFile != NULL) {
        fclose(pOutputFile);
      }
      pOutputFile = fopen(outFile, "w"); /* on ecrase le fichier cible */
      if (pOutputFile == NULL) {
        if (displayMessage == 1) {
          fprintf (stderr,"Unable to write in the output file %s\n", outFile) ;
        }
        exit(1);
      }
      /* on insere une entete en debut de fichier cible */
      catchCount = 0;
			if (inScilabTranslation==0) { /* traduction en matlab */
	          printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
      			sprintf(strTmp2, "%%%% File generated by IDL2Matlab %s %%%%\n", I2M_VERSION_2);
     				printOut(strTmp2);
      			printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
			}
			else { /* traduction en scilab */
						printOut("////////////////////////////////////////////////\n");
      			sprintf(strTmp2, "/// File generated by IDL2Matlab %s ////\n", I2M_VERSION_2);
     				printOut(strTmp2);
      			printOut("////////////////////////////////////////////////\n");
			}
      printOut("\n");
      numCurrentLine = 5;
      /* commentaires */
      if (n->fg != NULL) {
        insertComment(realLineInSource(n->fg));
      } else if (n->fd != NULL) {
        insertComment(realLineInSource(n->fd));
      } else {
        insertComment(n->lineInSource);
      }
      numVar=1;
      tab=0;
      inFunction = 1;	/* on est dans une fonction */
      printTab();
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      if (n->fg != NULL) { /* la fonction a des parametres */
        sprintf(strTmp, "function [I2Mfout, varargout] = %s(varargin)\n", (n->valNode).uString);
        inFunctionWithParam = 1;
      } else {
        sprintf(strTmp, "function [I2Mfout] = %s()\n", (n->valNode).uString);
        inFunctionWithParam = 0;
      }
      printOut(strTmp);
      numCurrentLine++;
      free(strTmp);
      incTab(); /* on tabule les statements */

      if (n->fg != NULL) { /* la fonction a des parametres */
      /* l'entete est ecrite => on ecrit la construction des tables */
	    printOut("\n");
        numCurrentLine++;
	    printOut(commentaire);
	    printOut(commentaire);
	    printOut("Initialization of parameters\n");
        numCurrentLine++;
        printTab();

        /* choix de la structure suivant le langage de traduction */
		        if (inScilabTranslation==0) {
		                printOut("I2Mkwn=char(");
		                parseFuncParam1(n->fg);
		        		printOut(", 'I2M_pos');\n");
				}
				else {
						printOut("I2Mkwn=[");
						parseFuncParam1(n->fg);
		        		printOut(", 'I2M_pos'];\n");
				}
		        numCurrentLine++;
		        printTab();
		        if (inScilabTranslation==0) {
				        printOut("I2Mkwv={");
				        parseFuncParam2(n->fg);
		   				printOut(", 'I2M_pos'};\n");
				}
				else {
						printOut("I2Mkwv=[");
						parseFuncParam2(n->fg);
		       			printOut(", 'I2M_pos'];\n");
		}
        numCurrentLine++;
        printTab();
        parseFuncParam3(n->fg);
        printOut("I2M_pos=[];\n");
        printOut(strIdl2Matlab);
        printOut("\n");
        printOut(commentaire);
        printOut(commentaire);
        printOut(" End of parameters initialization\n\n");
        numCurrentLine += 8;
      } else {
        printOut("\n");
        numCurrentLine++;
      }
      printVarSystemCalled(n->fd);
      if (strlen(undeclaredVariables) > 0) {
        printTab();
        printOut(commentaire);
        printOut(" Creation of undeclared variables of functions parameters\n");
        numCurrentLine ++;
        printTab();
        printOut(undeclaredVariables);
        printOut("\n\n");
        numCurrentLine+=2;
      }
      parseTree(n->fd);
      gestionCatch(0,n);
      decTab();
      insertComment(n->lineInSource);
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      sprintf(strTmp, "%s%s end of function %s\n",commentaire, commentaire, (n->valNode).uString);
      numCurrentLine++;
      printOut(strTmp);
      free(strTmp);
      if (displayMessage == 1) {
        fprintf(stderr, "%c%c%c%c%c%c", 8,8,8,8,8,8);
        fprintf (stderr,"==> OK - %4d lines", numCurrentLine) ;
        if (fileTranslationError) {
          fprintf (stderr," - %2d WARNING", nbWarningFile);
        }
        fprintf (stderr,"\n");
      }
      nbLinesTotal += numCurrentLine;
      nbGeneratedFile++;
      nbWarning += nbWarningFile;
      inFunction = 0;	/* on est plus dans une fonction */
      inFunctionWithParam = 0;
      if (inScilabTranslation==0) { /* traduction en matlab */
         sprintf(outFile,"%s%c%s.m",outDir, PATHSEP, "main");
      }
      else { /* traduction en scilab */
           sprintf(outFile,"%s%c%s.sci",outDir, PATHSEP, "main");
      }
      if (pOutputFile != NULL) {
        fclose(pOutputFile);
      }
      pOutputFile = fopen(outFile, "w+"); /* on ecrase le fichier cible */
      break;
    case FORWARD_FUNCTION :
      printOut(commentaire);
      printOut("  IDL Forward_function : ");
      parseTree(n->fd);
      break;
    case COMPILE_OPT :
      printOut(commentaire);
      printOut(" IDL Compile_opt : ");
      parseTree(n->fd);
      break;
    case  COMMON_CASE :
      printOut("global ");
      /* Les identificateurs d'un common doivent etre presents dans la table des symboles */
      /* Si common == 1 alors les identificateurs rencontres sont mis dans la table des symboles */
      common = 1;
      if (n->fg != NULL) {
        parseTree(n->fg);
      } else {
        inNamedCommon =1;
        if (n->fd->typeNode == IDENTIFIER) {
          /* Il n'y a que le nom du \"common\" */
          resultat = malloc(sizeof(recType));
          sprintf(strTmp2, "COMMON %s",(n->fd->valNode).uString);
          erreurHashTable = find(strTmp2, resultat);
          if (erreurHashTable == STATUS_OK) {
            Pas_tab = 1; /* ??? */
            parseTree((resultat->ptype)->suite_comp);
            Pas_tab = 0; /* ??? */
            strTmp = (char*) malloc (MAX_STR + strlen((n->fd->valNode).uString));
            sprintf(strTmp," %s%s %s",commentaire,commentaire,(n->fd->valNode).uString);
            printOut(strTmp);
            free(strTmp);
            free(resultat);
          }
        } else {
          /* Il n'y a un nom de \"common\" puis les variables */
          /* On insere le "common" avec les variables */
          parseTree(n->fd->fd);
          rec = malloc(sizeof(recType));
          rec->name = malloc(8 + strlen((n->fd->fg->valNode).uString));
          rec->ptype = malloc(sizeof(type));
          sprintf(rec->name, "COMMON %s", (n->fd->fg->valNode).uString);
          rec->ptype->num_type = COMMON_T;
          rec->ptype->suite_comp = n->fd->fd;
          insert(rec);
          strTmp = (char*) malloc (MAX_STR + strlen((n->fd->fg->valNode).uString));
          sprintf(strTmp," %s%s %s",commentaire,commentaire,(n->fd->fg->valNode).uString);
          printOut(strTmp);
          free(strTmp);
        }
        inNamedCommon =0;
      }
      /* On sort du "common", la variable common doit donc etre mise a 0 */
      common = 0;
      printOut("\n");
      numCurrentLine++;
      break;
    case  SUITE_IDENT:
      parseTree(n->fg);
      printOut(" ");
      parseTree(n->fd);
      if (n->fd->typeNode == IDENTIFIER) {
      	printOut(" ");
      }
      break;
    case  SUITE_PARAM:
      /* Affiche la liste des parametres d'une declaration de procedure ou de fonction */
      parseTree(n->fg);
      printOut(", ");
      parseTree(n->fd);
      break;
    case  PARAM	:
      if (n->fd == NULL) {
        parseTree(n->fg);
      } else {
        parseTree(n->fg);
      }
      break;
    case  PARAM_EXTRA:
      sprintf(strTmp2, "Warning => unknown translation of paramextra at line %d in %s\n", numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case  PARAM_REF_EXTRA:
      sprintf(strTmp2, "Warning => unknown translation of paramrefextra at line %d in %s\n", numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case  STATEMENT:
      break;
    case  STATEMENT_LIST:
    /* -------- debug ------------------------
    printOut("fg");
    if(n->fg != NULL) {
        strTmp = (char *) malloc(sizeof(int));
        sprintf(strTmp,"%d",n->fg->typeNode);
        printOut(strTmp);
        free(strTmp);
    } */


      /*if (n->fg != NULL) {
        if ((n->fg->typeNode == If) || (n->fg->typeNode == REPEAT_STATEMENT)
        || (n->fg->typeNode == WHILE) || (n->fg->typeNode == FOR) || (n->fg->typeNode == Catch)
        || (n->fg->typeNode == GESTION_ERREUR)) {
          printOut("\n");
          numCurrentLine++;
          printTab();
          /*printOut("1SL");*/
        /*}
      }   */

      parseTree(n->fg);
      if (n->fd == NULL) {
        /*printTab();
        printOut("2SL");*/
        return;
      }

      if((n->fd->typeNode != STATEMENT_LIST) && (n->fd->typeNode != COMMENTSTATEMENT)) {
      printTab();
      /*printOut("3SL");*/
      /*printOut("ICI");*/                          }

      if (  n->fd->typeNode != If &&  n->fd->typeNode != WHILE
         && n->fd->typeNode != FOR &&  n->fd->typeNode != REPEAT_STATEMENT
         &&  n->fd->typeNode != CASE &&  n->fd->typeNode != CASE_STATEMENT
         && n->fd->typeNode != RETURN ) {
          insertComment(realLineInSource(n->fd));
      }
    /* -------- debug ------------------------
    printOut("fd");
        if(n->fd != NULL) {
        strTmp = (char *) malloc(sizeof(int));
        sprintf(strTmp,"%d",n->fd->typeNode);
        printOut(strTmp);
        free(strTmp);
    } */
    /*  if ((n->fd->typeNode == If) || (n->fd->typeNode == REPEAT_STATEMENT)
        || (n->fd->typeNode == WHILE) || (n->fd->typeNode == FOR) || (n->fd->typeNode == Catch)
        || (n->fd->typeNode == GESTION_ERREUR)) {
          printOut("\n");
          numCurrentLine++;
          printTab();
          /*printOut("4SL");*/
      /*}*/
      parseTree(n->fd);
      if (n->fd->typeNode == STATEMENT_LIST) {
          break;
      }

      if (n->fd->typeNode == ASSIGNMENT ||
          n->fd->typeNode == FUNCTION_CALL_OU_REF_MATRIX ||
          n->fd->typeNode == FUNCTION_CALL || n->fd->typeNode == RETURN) {
           printOut(";");
      }


        /*if (n->fd->typeNode != ACOMMENT ||
            n->fd->typeNode != COMMENTSTATEMENT) {
            printOut("\n");
            numCurrentLine++;
        } */
      break;
    case  PARENTHESE:
      if (n->fd == NULL) {
        printOut("(");
        parseTree(n->fg);
        printOut(")");
      } else {
        printOut("i2m_index(");
        parseTree(n->fg);
        printOut(",'");
        appel_matrice_struct = 1;
        parseTree(n->fd->fg);
        if (n->fd->fg->typeNode != PARAM_MATRIX_ETOILE && n->fd->fg->typeNode != INTERVALLE
            && n->fd->fg->typeNode != MATRIX && n->fd->fg->typeNode != SUITE_CALL_LIST) {
          printOut("+1");
        }
        printOut("')");
        appel_matrice_struct=0;
      }
      break;
    case  AROBASE:
      break;
    case  AROBASE_POINT	:
      break;
    case  RETURN:
      if (inFunctionWithParam || inProcWithParam ) {
        if (inScilabTranslation==0) {
					/* On traduit vers Matlab */
					printOut("if ~isempty(I2M_out),eval(I2M_out);end;");
				}
				else {
					/* On traduit vers Scilab */
					printOut("if ~(isempty(I2M_out)); execstr([I2M_out]); else varargout(1)=[]; end \n");
                    numCurrentLine++;
				}
      }
      if (n->fg == NULL) {
        printOut("return");
      } else { /* on retourne quelque chose */
        printOut("I2Mfout = ");
      	parseTree(n->fg);
      	printOut(";\n");
        numCurrentLine++;
      	printTab();
        printOut("return");
      }
      break;
    case  PLUS	:
      /* Genere l'addition */
      parseTree(n->fg);
      printOut(" + ");
      parseTree(n->fd);
      break;
    case  MINUS	:
      /* Genere la soustraction */
      parseTree(n->fg);
      printOut(" - ");
      parseTree(n->fd);
      break;
    case  PUISS	:
      /* Genere la puissance */
      parseTree(n->fg);
      printOut(".^");
      parseTree(n->fd);
      break;
    case  PROCEDURE_CALL:
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      tableSymbNode = malloc(sizeof(recType));
      sprintf(strTmp, "PRO %s",(n->valNode).uString);
      if (find(strTmp, tableSymbNode) == STATUS_OK) {
      /* on regarde si la procedure est dans la table des symboles */
        if (tableSymbNode->ptype->num_type == PROCEDURE) {
	      /* si elle y est et que c'est bien une procedure on garde le bon nom */
	      /* LA PROCEDURE EST DEFINIE PAR L UTILISATEUR */

          /* Test du nom de la fonction */
          sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
          resultat = malloc(sizeof(recType));
          if (find(strTmp2,resultat) == STATUS_OK) {
            if ((resultat->ptype->num_type == RESERVED_FUNCTION) ||
                (resultat->ptype->num_type == RESERVED)) {
                /* la nom de la procedure est reserve => renommage */
              sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
              strcpy((n->valNode).uString, strTmp2);
            }
          }
          free(resultat);

          strcpy(strTmp,(n->valNode).uString);
          if (parseProCallParam4(n->fg,0) ==1) { /* cas du passage de parametres complexe */
            if (parseProCallParam1(n->fg,1) == 0) { /* on a ecrit quelque chose */
              deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              printOut("] = ");
            }
          }
          sprintf(strTmp, "%s", strTmp);
          printOut(strTmp);
          free(strTmp);
          if (tableSymbNode != NULL) {
            free(tableSymbNode);
          }
      	  if (n->fg != NULL) {
            if (parseProCallParam4(n->fg,0) ==1) { /* cas du passage de parametres complexe */
	            printOut("(");
              numVar=1;  /* pour la numerotation des variables retournees */
              parseProCallParam2(n->fg,0); /* on passe les parametres avec leur tags */
      	      if (containsVariable(n->fg) == 1) { /* on a des variables par adresse */
              	numVar=1;  /* numerotation des variables */
              	printOut(", 'I2M_pos', [");
                numParamInCurProc = nbParamPro(n->fg);
              	parseProCallParam3(n->fg,1); /* on ecrit le dernier parametre indiquant les variables */
                deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              	printOut("])");
              } else {	/* pas de i2m_pos */
                printOut(")");
              }
            } else { /* cas du passage de parametre simple */
              printOut("(");
              parseTree(n->fg);
              printOut(")");
            }
          }
	        printOut(";");
          return;
      	}
      }
      /* Sinon on regarde si c est un cas particulier traiter specialement */
      /*** Cas du IO_ERROR ****/
      if (strcmp((n->valNode).uString, "on_ioerror") == 0) {
        sprintf(strTmp2, "ONIOERROR %s",(n->fg->valNode).uString);
        resultat = malloc(sizeof(recType));
        if (find(strTmp2,resultat) != STATUS_OK) {
          inserer_tuer(strTmp2, VARIABLE);
          printOut("try,");
          incTab();
          catchCount++;
          catchVar[catchCount] = NULL;
          n->fg->lineInSource = tab; /* on bidouille pour reperer le niveau de bloc */
          catchExecNode[catchCount] = n->fg;
        }
        free(resultat);
        return;
      }

      /*** Cas du CALL_PROCEDURE ****/
      if (strcmp((n->valNode).uString, "call_procedure") == 0) {
        /* on construit l'arbre tmp correspondant a l appel de fonction IDL */
        /* on a 2 cas selon le nombre de param de la fonction */
      	printOut("eval(i2m_tr(");
        if (n->fg->typeNode != SUITE_CALL_LIST) { /* on a qu'un param */
      	  parseTree(n->fg);
        } else { /* on a plusieurs param */
      	  parseTree(n->fg->fg);
          printOut("+','+");
          if (stringTranslation == 1) {
            strTmp = (char*) malloc (strlen(stringTranslationResult)+1);
            strcpy(strTmp,stringTranslationResult);
            strcpy(stringTranslationResult, "");
            parseTree(n->fg->fd);
            strTmp3 = (char*) malloc (4096);
            strcpy(strTmp3, stringTranslationResult);
            strcpy(stringTranslationResult, strTmp);
            doubleApostrophe(strTmp3,strTmp); /* on adapte ce petit bout */
            free(strTmp3);
          } else {
            stringTranslation = 1;
            stringTranslationResult = (char*) malloc (4096);
            strcpy(stringTranslationResult, "");
            strTmp = (char*) malloc (4096);
            parseTree(n->fg->fd);
            doubleApostrophe(stringTranslationResult,strTmp); /* on adapte ce petit bout */
            free(stringTranslationResult);
            stringTranslation = 0;
          }
          printOut(strTmp);
          free(strTmp);
        }
        printOut("), '');");
      	return;
      }
      /********  FIN DES CAS DES TRAITEMENTS PARTICULIERS ******/

      /* Sinon on regarde dans la table des symboles voir s il y a une traduction avancee */
      strTmp = (char*) malloc (4096);
      sprintf(strTmp, "TRANSLATION %s",(n->valNode).uString);
      tableSymbNode = malloc(sizeof(recType));
      if (find(strTmp,tableSymbNode) == STATUS_OK) {
      	/* elle est defini dans le fichier de traduction avance donc on peut traduire  */
        /* recuperation des valeurs des parametres apres execution */
        strcpy(strTmp, (tableSymbNode->ptype->suite_comp->valNode).uString);
        strTmp3 = (char*) malloc (4096);
      	strcpy(strTmp, getLibFileWord(strTmp,strTmp3)); /* on recupere le nom Matlab de la proc */
      	sprintf(strTmp2, "%s.m", strTmp3);
      	strcpy(strTmp, getLibFileWord(strTmp,strTmp2)); /* on recupere le mode de passage de parametres*/
        i = atoi(strTmp2);
        if (i == 0) { /* passage simple */
      	  printOut(strTmp3); /*on ecrit le nom de la fonction */
          if (n->fg != NULL) { /* on a des parametres */
          if (containsKeywords(n->fg) == 1) {
             strcpy(strTmp,(n->valNode).uString);
                sprintf(strTmp2, "Line %d => procedure <%s> (-> %s) doesn't accept keywords at line %d in %s\n",n->lineInSource, strTmp, strTmp3,numCurrentLine, outFile);
             printToLog(strTmp2);
             /*free(strTmp);*/
          }
      	    printOut("(");
	          parseTree(n->fg);
            printOut(")");
          }
          free(strTmp3);
        } else {  /* passage de parametres complexe */
          if (parseProCallParam4(n->fg,0) ==1) { /* cas du passage de parametres complexe */
            if (parseProCallParam1(n->fg,1) == 0) { /* on a ecrit quelque chose */
              deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              printOut("] = ");
            }
          }
          printOut(strTmp3); /*on ecrit le nom de la fonction */
          free(strTmp3);
          if (n->fg != NULL) { /* on a des parametres */
      	    if (parseProCallParam4(n->fg,0) ==1) { /* cas du passage de parametres complexe */
      	      printOut("(");
              numVar=1;  /* pour la numerotation des variables retournees */
              parseProCallParam2(n->fg,0); /* on passe les parametres avec leur tags */
      	      if (containsVariable(n->fg) == 1) { /* on a des variables par adresse */
              	numVar=1;  /* numerotation des variables */
              	printOut(", 'I2M_pos', [");
                numParamInCurProc = nbParamPro(n->fg);
              	parseProCallParam3(n->fg,1); /* on ecrit le dernier parametre indiquant les variables */
                deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              	printOut("])");
              } else { /* pas de i2m_pos */
                printOut(")");
              }
            } else { /* on passe les parametre simplement */
              printOut("(");
              parseTree(n->fg);
              printOut(")");
            }
          }
        }
      	printOut(";");
        free(strTmp);
        return;
      }

      /* En dernier recours, on laisse le meme nom de fonction est on leve un warning*/
      strcpy(strTmp,(n->valNode).uString);
      sprintf(strTmp2, "Line %d => unknown procedure <%s> at line %d in %s\n", n->lineInSource, strTmp,numCurrentLine, outFile);
      printToLog(strTmp2);
      sprintf(strTmp, "%s", strTmp);
      printOut(strTmp);
      free(strTmp);
      if (n->fg != NULL) {
        printOut("(");
      	parseTree(n->fg);
      	printOut(")");
      }
      printOut(";");
      break;
    case FUNCTION_CALL_OU_REF_MATRIX:
      /* On peut avoir 3 cas : un appel de fonction, une ref_matrix(appel de matrice) */
      /* ou une ref_struct(appel de structure) */
      isFunctionCall = 0;
      /* On regarde les cas d'appels de fonctions et de ref_matrix */
      if (n->fd == NULL) {
        /* On cherche a savoir si c'est un appel de fonction ou une ref_matrix */
        /* On cherche dans la table des symboles le bloc correspondant au nom  */
        sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
        resultat = malloc(sizeof(recType));
        if (find(strTmp2,resultat) == STATUS_OK) {
          if ((resultat->ptype->num_type == RESERVED_FUNCTION) ||
              (resultat->ptype->num_type == RESERVED)) {
            sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
          } else {
            sprintf(strTmp2, "%s",(n->valNode).uString);
          }
        } else {
          sprintf(strTmp2, "%s",(n->valNode).uString);
        }
        if (find(strTmp2, resultat) == STATUS_OK) {
          /* On est dans le cas d'une ref_matrix. On met a jour la variable correspondante */
          strcpy((n->valNode).uString, strTmp2);
          appel_matrice_struct = 1;
          /* On affiche le nom de la matrice*/
          printOut((n->valNode).uString);
          printOut("(");
          /* IDL commence a 0 et matlab a 1 dans les matrices */
          /* Il faut donc rajouter 1 dans les appels de matrice */
          if (n->fg->typeNode != PARAM_MATRIX_ETOILE && n->fg->typeNode != INTERVALLE
          && n->fg->typeNode != MATRIX && n->fg->typeNode != SUITE_CALL_LIST) {
            /* On rejoute 1 pour le cas d'un appel simple avec un seul parametre */
            parseTree(n->fg);
            printOut(" +1");
          } else {
            /* Si le parametre de ref_matrix est une matrice alors on fait appel */
            /* a la fonction afficher_matrix_crochet */
            if (n->fg->typeNode == MATRIX)
            {
              afficher_matrix_crochet(n->fg->fg);
            } else {
              /* sinon, pour les cas plus complexes, on affiche 1 plus tard */
              parseTree(n->fg);
            }
          }
          printOut(")");
          /* On sort d'une ref_matrix. On met a jour la variable */
          appel_matrice_struct = 0;
          isFunctionCall = 0;
        } else {
          /* On est dans le cas d'un appel de fonction. On met a jour la variable correspondante */
          isFunctionCall = 1;
        }
        free(resultat);
      }
      if (isFunctionCall==0) {
        /* On n'etait pas dans un appel de fonction precedemment alors on sort du case */
        break;
      }
      /* On etait dans un appel de fonction precedemment, on rentre alors dans le case FUNCTION_CALL */
    case  FUNCTION_CALL	:
      appelFonctionTmp = appelFonction; /* on stock la valeur de maniere locale*/
      appelFonction = 0; /* Indique qu on aura deja traiter le cas */
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      sprintf(strTmp, "FUNC %s", (n->valNode).uString);
      tableSymbNode = malloc(sizeof(recType));
      if (find(strTmp, tableSymbNode) == STATUS_OK) {
      /* on regarde si la fonction est dans la table des symboles */
        if (tableSymbNode->ptype->num_type == FONCTION) {
        /* LA FONCTION EST DEFINIE PAR L UTILISATEUR */
          /* Test du nom de la fonction */
          sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
          resultat = malloc(sizeof(recType));
          if (find(strTmp2,resultat) == STATUS_OK) {
            if ((resultat->ptype->num_type == RESERVED_FUNCTION) ||
                (resultat->ptype->num_type == RESERVED)) {
                /* la nom de la procedure est reserve => renommage */
              sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
              strcpy((n->valNode).uString, strTmp2);
            }
          }
          free(resultat);

          if (appelFonctionTmp == 1) { /* si on est dans un assignement */
            if (containsVariable(n->fg) == 1) { /* on a aussi des variables de retour */
              printOut("[");
              parseTree(nameAssignement);
              if (nameAssignement != NULL) {
                if (nameAssignement->typeNode == IDENTIFIER) {
                  sprintf(strTmp2, "REFPARAM %s",(nameAssignement->valNode).uString);
                  inserer_tuer(strTmp2, REFPARAM);
                } /* on insere le retour de fonction pour eviter les pb de param identiques */
              }
              parseProCallParam1(n->fg,0);
              deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              strTmpOp = (char *) malloc(10);
              sprintf(strTmpOp,"] %s ",opAssignment);
              printOut(strTmpOp);
              /* Affichage d'un warning si opAssignment <> "=" ?? */
              free(strTmpOp);
              /* printOut("] = ");*/
            } else {
              parseTree(nameAssignement);
              strTmpOp = (char *) malloc(10);
              sprintf(strTmpOp," %s ",opAssignment);
              printOut(strTmpOp);
              /* Affichage d'un warning si opAssignment <> "=" ?? */
              free(strTmpOp);
              /*printOut(" = ");*/
            }
          }
          strcpy(strTmp,(n->valNode).uString);
          sprintf(strTmp, "%s", strTmp);
          printOut(strTmp);
          free(strTmp);
          if (n->fg != NULL) { /* on a des parametres */
            if ((containsKeywords(n->fg) == 1) || ((containsVariable(n->fg) == 1) && (appelFonctionTmp == 1))) {/* cas du passage de parametre complexe */
              printOut("(");
              numVar=1;  /* pour la numerotation des variables retournees */
              parseProCallParam2(n->fg, 0); /* on passe les parametres avec leur tags */
              if (appelFonctionTmp == 1) {
                if (containsVariable(n->fg) == 1) { /* on a des variables par adresse */
                  numVar=1;  /* numerotation des variables */
                  printOut(", 'I2M_pos', [");
                  if (nameAssignement != NULL) {
                    if (nameAssignement->typeNode == IDENTIFIER) {
                      sprintf(strTmp2, "REFPARAM %s",(nameAssignement->valNode).uString);
                      inserer_tuer(strTmp2, REFPARAM);
                    } /* on insere le retour de fonction pour eviter les pb de param identiques */
                  }
                  numParamInCurProc = nbParamPro(n->fg);
                  parseProCallParam3(n->fg,1); /* on ecrit le dernier parametre indiquant les variables */
                  deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
                  printOut("])");
                } else { /* pas de i2m_pos */
                  printOut(")");
                }
              } else {
                printOut(")");
              }
            } else { /* cas du passage de parametre simple */
              printOut("(");
              parseTree(n->fg);
              printOut(")");
            }
          }
          return;
        }
      }
      if (tableSymbNode != NULL) {free(tableSymbNode);}

      /* Sinon on regarde si c est un cas particulier traiter specialement */
      /*** Cas du EXECUTE ****/
      if (strcmp((n->valNode).uString, "execute") == 0) {
        if (appelFonctionTmp == 1) { /* si on est dans un assignement */
          parseTree(nameAssignement);
          strTmpOp = (char *) malloc(10);
          sprintf(strTmpOp," %s ",opAssignment);
          printOut(strTmpOp);
          /* Affichage d'un warning si opAssignment <> "=" ?? */
          free(strTmpOp);
          /*printOut(" = ");*/
          printOut("1; eval(i2m_tr");
          if (n->fg != NULL) {
            printOut("(");
            parseTree(n->fg);
            printOut(")");
          }
          if (nameAssignement != NULL) { /* on retourne 0 si le premier truc foire */
            printOut(", '");
            parseTree(nameAssignement);
            printOut("=0;'");
          }
        } else {
          printOut("eval(i2m_tr");
          if (n->fg != NULL) {
            printOut("(");
            parseTree(n->fg);
            printOut(")");
          }
          if (inCondStruct == 1) {
            sprintf(strTmp2, "Line %d => Problem with EXECUTE (correct it to res=execute(...)) at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
            printToLog(strTmp2);
          }
        }
        printOut(")");
        free(strTmp);
        return;
      }

      /*** Cas du N_ELEMENTS ****/
      if (strcmp((n->valNode).uString, "n_elements") == 0) {
        if (appelFonctionTmp == 1) { /* si on est dans un assignement */
          parseTree(nameAssignement);
          strTmpOp = (char *) malloc(10);
          sprintf(strTmpOp," %s ",opAssignment);
          printOut(strTmpOp);
          /* Affichage d'un warning si opAssignment <> "=" ?? */
          free(strTmpOp);
          /*printOut(" = ");*/
        }
        printOut("eval('n_elements(");
        if (n->fg != NULL) {
          parseTree(n->fg);
        }
        printOut(")','0')");
        return;
      }

      /*** Cas du N_PARAMS ****/
      if (strcmp((n->valNode).uString, "n_params") == 0) {
        if (appelFonctionTmp == 1) { /* si on est dans un assignement */
          parseTree(nameAssignement);
          strTmpOp = (char *) malloc(10);
          sprintf(strTmpOp," %s ",opAssignment);
          printOut(strTmpOp);
          /* Affichage d'un warning si opAssignment <> "=" ?? */
          free(strTmpOp);
          /*printOut(" = ");*/
        }
        printOut("n_params(nargin, eval('varargin','{}'))");
        return;
      }

      /*** Cas du SIZE ****/
      if (strcmp((n->valNode).uString, "size") == 0) {
        if (appelFonctionTmp == 1) { /* si on est dans un assignement */
          parseTree(nameAssignement);
          strTmpOp = (char *) malloc(10);
          sprintf(strTmpOp," %s ",opAssignment);
          printOut(strTmpOp);
          /* Affichage d'un warning si opAssignment <> "=" ?? */
          free(strTmpOp);
          /*printOut(" = ");*/
        }
        printOut("eval('sizz(");
        if (n->fg != NULL) {
          parseTree(n->fg);
        }
        printOut(")','[0 0 1]')");
        return;
      }

      /*** Cas du CALL_FUNCTION ****/
      if (strcmp((n->valNode).uString, "call_function") == 0) {
        /* on construit l'arbre tmp correspondant a l appel de fonction IDL */
        /* on a 2 cas selon le nombre de param de la fonction */
        if (appelFonctionTmp == 1) { /* si on est dans un assignement */
          parseTree(nameAssignement);
          strTmpOp = (char *) malloc(10);
          sprintf(strTmpOp," %s ",opAssignment);
          printOut(strTmpOp);
          /* Affichage d'un warning si opAssignment <> "=" ?? */
          free(strTmpOp);
          /*printOut(" = ");*/
        }
        printOut("eval(i2m_tr(");
        if (n->fg->typeNode != SUITE_CALL_LIST) { /* on a qu'un param */
          parseTree(n->fg);
        } else { /* on a plusieurs param */
          parseTree(n->fg->fg);
          printOut("+'('+");
          if (stringTranslation == 1) {
            strTmp = (char*) malloc (strlen(stringTranslationResult)+1);
            strcpy(strTmp,stringTranslationResult);
            strcpy(stringTranslationResult, "");
            parseTree(n->fg->fd);
            strTmp3 = (char*) malloc (4096);
            strcpy(strTmp3, stringTranslationResult);
            strcpy(stringTranslationResult, strTmp);
            doubleApostrophe(strTmp3,strTmp); /* on adapte ce petit bout */
            free(strTmp3);
          } else {
            stringTranslation = 1;
            stringTranslationResult = (char*) malloc (4096);
            strcpy(stringTranslationResult, "");
            strTmp = (char*) malloc (4096);
            parseTree(n->fg->fd);
            doubleApostrophe(stringTranslationResult,strTmp); /* on adapte ce petit bout */
            free(stringTranslationResult);
            stringTranslation = 0;
          }
          printOut(strTmp);
          free(strTmp);
          printOut("+')')");
        }
        printOut(", '3.14')");
        return;
      }
      /********  FIN DES CAS DES TRAITEMENTS PARTICULIERS ******/

      /* Sinon on regarde dans le fichier lib.dat voir s il y a une traduction avancee */
      strTmp = (char*) malloc (4096);
      sprintf(strTmp, "TRANSLATION %s",(n->valNode).uString);
      tableSymbNode = malloc(sizeof(recType));
      if (find(strTmp,tableSymbNode) == STATUS_OK) {
      	/* elle est defini dans le fichier de traduction avance donc on peut traduire  */
        /* recuperation des valeurs des parametres apres execution */
        strcpy(strTmp, (tableSymbNode->ptype->suite_comp->valNode).uString);
        strTmp3 = (char*) malloc (4096);
        strcpy(strTmp, getLibFileWord(strTmp,strTmp3)); /* on recupere le nom Matlab de la fonction */
        sprintf(strTmp2, "%s.m", strTmp3);
        strcpy(strTmp, getLibFileWord(strTmp,strTmp2)); /* on recupere le mode de passage de parametres*/
        i = atoi(strTmp2);
        if (i == 0) { /* passage simple */
          if (appelFonctionTmp == 1) { /* si on est dans un assignement */
            parseTree(nameAssignement);
            strTmpOp = (char *) malloc(10);
            sprintf(strTmpOp," %s ",opAssignment);
            printOut(strTmpOp);
            /* Affichage d'un warning si opAssignment <> "=" ?? */
            free(strTmpOp);
            /*printOut(" = ");*/
          }
          printOut(strTmp3); /*on ecrit le nom de la fonction */
          if (n->fg != NULL) { /* on a des parametres */
            if (containsKeywords(n->fg) == 1) {
                strcpy(strTmp,(n->valNode).uString);
                sprintf(strTmp2, "Line %d => function <%s> (-> %s) doesn't accept keywords at line %d in %s\n",n->lineInSource, strTmp, strTmp3, numCurrentLine, outFile);
                printToLog(strTmp2);
                /*free(strTmp);*/
            }
            printOut("(");
             parseTree(n->fg);
            printOut(")");
          }
          free(strTmp3);
        } else {  /* passage de paramatre complexe */
          if (appelFonctionTmp == 1) { /* si on est dans un assignement */
            if (containsVariable(n->fg) == 1) { /* on a aussi des variables de retour */
              printOut("[");
              parseTree(nameAssignement);
              if (nameAssignement != NULL) {
                if (nameAssignement->typeNode == IDENTIFIER) {
                  sprintf(strTmp2, "REFPARAM %s",(nameAssignement->valNode).uString);
                  inserer_tuer(strTmp2, REFPARAM);
                } /* on insere le retour de fonction pour eviter les pb de param identiques */
              }
              parseProCallParam1(n->fg,0);
              deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
              strTmpOp = (char *) malloc(10);
              sprintf(strTmpOp,"] %s ",opAssignment);
              printOut(strTmpOp);
              /* Affichage d'un warning si opAssignment <> "=" ?? */
              free(strTmpOp);
              /*printOut("] = ");*/
            } else {
              parseTree(nameAssignement);
              strTmpOp = (char *) malloc(10);
              sprintf(strTmpOp," %s ",opAssignment);
              printOut(strTmpOp);
              /* Affichage d'un warning si opAssignment <> "=" ?? */
              free(strTmpOp);
              /*printOut(" = ");*/
            }
          }
          printOut(strTmp3); /*on ecrit le nom de la fonction */
          free(strTmp3);
          if (n->fg != NULL) { /* on a des parametres */
            if ((containsKeywords(n->fg) == 1) || ((appelFonctionTmp ==1) && (containsVariable(n->fg) == 1))){/* cas du passage de parametre complexe */
              printOut("(");
              numVar=1;  /* pour la numerotation des variables retournees */
              parseProCallParam2(n->fg, 0); /* on passe les parametres avec leur tags */
              if (appelFonctionTmp == 1) { /* si on est dans un assignement */
                if (containsVariable(n->fg) == 1) { /* on a des variables par adresse */
                  numVar=1;  /* numerotation des variables */
                  printOut(", 'I2M_pos', [");
                  if (nameAssignement != NULL) {
                    if (nameAssignement->typeNode == IDENTIFIER) {
                      sprintf(strTmp2, "REFPARAM %s",(nameAssignement->valNode).uString);
                      inserer_tuer(strTmp2, REFPARAM);
                    } /* on insere le retour de fonction pour eviter les pb de param identiques */
                  }
                  numParamInCurProc = nbParamPro(n->fg);
                  parseProCallParam3(n->fg,1); /* on ecrit le dernier parametre indiquant les variables */
                  deleteObjetOfType(REFPARAM); /* on clean la table des symboles */
                  printOut("])");
                } else { /* pas d i2m_pos */
                  printOut(")");
                }
              } else { /* pas d i2m_pos */
                printOut(")");
              }
            } else { /* on passe les parametre simplement */
              printOut("(");
              parseTree(n->fg);
              printOut(")");
            }
          }
        }
        if (strTmp != NULL) {
          free(strTmp);
        }
        return;
      }

      /* En dernier recours, on laisse le meme nom de fonction est on leve un warning*/
      strcpy(strTmp,(n->valNode).uString);
      sprintf(strTmp2, "Line %d => unknown function  <%s> at line %d in %s\n", n->lineInSource, strTmp,numCurrentLine, outFile);
      printToLog(strTmp2);
      sprintf(strTmp, "%s", strTmp);
      if (appelFonctionTmp == 1) { /* si on est dans un assignement */
        parseTree(nameAssignement);
        strTmpOp = (char *) malloc(10);
        sprintf(strTmpOp," %s ",opAssignment);
        printOut(strTmpOp);
        /* Affichage d'un warning si opAssignment <> "=" ?? */
        free(strTmpOp);
        /*printOut(" = ");*/
      }
      printOut(strTmp);
      free(strTmp);
      if (n->fg != NULL) {
        printOut("(");
        parseTree(n->fg);
        printOut(")");
      }
      break;
    case  SUITE_CALL_LIST:
    case  PARAM_MATRIX_SUITE:
      /* On regarde 2 cas : on etait dans une ref_matrix ou une ref_struct alors il faut ajouter 1 */
      /* sinon, il ne faut rien faire */
      if (appel_matrice_struct == 1) {
        /* On est dans une ref_matrice : on ajoute 1 dans la partie gauche de SUITE_CALL_LIST */
        if (n->fg->typeNode != PARAM_MATRIX_ETOILE && n->fg->typeNode != INTERVALLE && n->fg->typeNode != MATRIX )
        {/* Pour les cas autres que *, intervalle ou matrice, on rajoute simplement 1 */
          parseTree(n->fg);
          printOut(" +1");
        } else {
          /* Si le parametre de ref_matrix est une matrice alors on fait appel */
          /* a la fonction afficher_matrix_crochet */
          if (n->fg->typeNode == MATRIX) {
            afficher_matrix_crochet(n->fg->fg);
          } else {
            /* sinon, on ajoute 1 plus tard */
            parseTree(n->fg);
          }
        }
        printOut(",");
        /* On est dans une ref_matrice : on ajoute 1 dans la partie gauche de SUITE_CALL_LIST */
        if (n->fd->typeNode != PARAM_MATRIX_ETOILE && n->fd->typeNode != INTERVALLE && n->fd->typeNode != MATRIX
              && n->fd->typeNode != SUITE_CALL_LIST)
        {/* Pour les cas autres que *, intervalle ou matrice, on rajoute simplement 1 */
          parseTree(n->fd);
          printOut(" +1");
        } else {
          /* Si le parametre de ref_matrix est une matrice alors on fait appel */
          /* a la fonction afficher_matrix_crochet */
          if (n->fd->typeNode == MATRIX) {
            afficher_matrix_crochet(n->fd->fg);
          } else {
            /* sinon, on ajoute 1 plus tard */
            parseTree(n->fd);
          }
        }
      } else {
        /* On genere le SUITE_CALL_LIST */
        parseTree(n->fg);
        printOut(",");
        parseTree(n->fd);
      }
      break;
    case  SUITE_CALL:	/* cas du /param */
      printOut("1");
      break;
    case  STRUCTURE:
      /* On regarde 2 cas :  {coucou} (un seul nom de structure) */
      /* et {age : 13, ..., taille : 1.78} (on definit la structure) */
      if (n->fg == NULL && n->fd == NULL) {
        /* On est dans le cas {coucou} */
        /* On fait alors appel a une fonction nommee "struct_vide"  */
        printOut("struct_vide('structure_name','");
        printOut((n->valNode).uString);
        printOut("',");
        /* On cherche l'ensemble des attributs de la structure {coucou} dans la table des symboles */
        resultat = malloc(sizeof(recType));
        strTmp = (char*) malloc (strlen((n->valNode).uString)+5);
        sprintf(strTmp, "REF %s", (n->valNode).uString);
        if (find(strTmp,resultat) == STATUS_OK)	{
          /* On genere l'ensemble des parametres de "struct_vide" */
          genererStruct(resultat->ptype->suite_comp);
        }
        free(resultat);
        printOut(")");
      } else {
        /* On est dans le cas salut = {age : 13, ..., taille : 1.78} */
        printOut("struct(");
        /* On genere l'ensemble des parametres de "struct"*/
        genererStruct(n->fg);
        printOut(")");
      }
      break;
    case NAMED_STRUCTURE :
      /* On est dans le cas salut = {coucou, age : 13, ..., taille : 1.78} */
      printOut("struct('structure_name','");
      printOut((n->valNode).uString);
      printOut("',");
      if (n->fg->typeNode == SUITE_CONS) {
        /* On entre dans la table des symboles "coucou" avec ses attributs "age : 13, ..., taille : 1.78"*/
        creer_ref_tab_symb((n->valNode).uString, n->fg);
        /* On genere l'ensemble des parametres de "struct"*/
        genererStruct(n->fg);
      } else {
        resultat = malloc(sizeof(recType));
        strTmp = (char*) malloc (strlen((n->valNode).uString)+5);
        sprintf(strTmp, "REF %s", (n->valNode).uString);
        if (find(strTmp,resultat) == STATUS_OK)	{
          /* On genere l'ensemble des parametres de "struct_vide" */
          genererNamedStruct(resultat->ptype->suite_comp, n->fg);
        }
        free(resultat);
      }
      printOut(")");
    break;
    case  SUITE_CONS:
      sprintf(strTmp2, "Warning => unknown translation of suitecons at line %d in %s\n", numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case  REF_STRUCT_LIST:
      appel_matrice_struct = 1;
      if (inRef_struct_list == 0) {
      	inRef_struct_list = 1;
      	printOut("[");
        parseTree(n->fg);
        printOut(".");
        parseTree(n->fd);
        printOut("]");
        inRef_struct_list = 0;
    	} else {
      	parseTree(n->fg);
        printOut(".");
        parseTree(n->fd);
      }
      appel_matrice_struct =0;
      break;
    case  REF_STRUCT:
      if (inRef_struct_list == 0) {
        inRef_struct_list = 1;
        if (n->fg->typeNode != IDENTIFIER) printOut("[");
        parseTree(n->fg);
        printOut(".");
        parseTree(n->fd);
        if (n->fg->typeNode != IDENTIFIER) printOut("]");
        inRef_struct_list = 0;
      } else {
        parseTree(n->fg);
        printOut(".");
        parseTree(n->fd);
      }
      break;
    case  IDENTIFIER_PARENTHESE :
      /* On affiche le nom de l'identifier*/
      printOut((n->valNode).uString);
      printOut("(");
      /* IDL commence a 0 et matlab a 1 dans les matrices */
      /* Il faut donc rajouter 1 dans les appels de matrice */
      if (n->fg->typeNode != PARAM_MATRIX_ETOILE && n->fg->typeNode != INTERVALLE
        && n->fg->typeNode != MATRIX && n->fg->typeNode != SUITE_CALL_LIST) {
        /* On rejoute 1 pour le cas d'un appel simple avec un seul parametre */
        parseTree(n->fg);
        printOut(" +1");
      } else {
        /* Si le parametre de ref_matrix est une matrice alors on fait appel */
        /* a la fonction afficher_matrix_crochet */
        if (n->fg->typeNode == MATRIX) {
          afficher_matrix_crochet(n->fg->fg);
        } else {
          /* sinon, pour les cas plus complexes, on affiche 1 plus tard */
          parseTree(n->fg);
        }
      }
      printOut(")");
      break;
    case  MATRIX:
      /* On regarde si on n'est pas deja a l'interieur d'une matrice */
      if (inMatrix == 0) {
      /* on entre dans une nouvelle matrice. On met la variable correspondante a jour */
        inMatrix = 1;
        /* On regarde si on est dans une matrice simple(pas de matrice imbriquee) */
        /* ou dans une matrice "imbriquee" */
        if (n->fg->fg != NULL && n->fg->fg->typeNode != MATRIX)
        {
          /* On est dans une matrice simple */
          /* On fait alors appel a une fonction nommee "reshape_array"  */
          sprintf(strTmp2,"d%d_array(", getMatrixDim(n));
          printOut(strTmp2);
          /* On sait que la matrice est de dimension nbdim-by-1. On calcule alors le nbdim */
          nbdim = calculer_dim(n->fg);
          parseTree(n->fg);
          printOut(")");
        } else {
          /* On calcule la dimension de la matrice que l'on rentre dans TabDim */
          TabDim[indDim]=calculer_dim(n->fg);
          indDim ++;
          sprintf(strTmp2,"d%d_array(", getMatrixDim(n));
          printOut(strTmp2);
          parseTree(n->fg);
          /* On affiche les dimensions de la matrice */
          /*afficher_dims();*/
          printOut(")");
        }
        inMatrix = 0;
      } else {
        /* On est deja dans une matrice */
        /* On calcule la dimension de cette matrice que l'on rentre dans TabDim */
        TabDim[indDim]=calculer_dim(n->fg);
        indDim ++;
        sprintf(strTmp2,"d%d_array(", getMatrixDim(n));
        printOut(strTmp2);
        parseTree(n->fg);
        printOut(")");
      }
      break;
   case  SUITE_MATRIX	:
      /* On affiche les elements de la matrice */
      parseTree(n->fg);
      printOut(",");
      parseTree(n->fd);
      break;
    case  PARAM_MATRIX_ETOILE	:
      /* On genere l'* d'IDL */
      printOut(":");
      break;
    case  INTERVALLE	:
      /* On genere les intervalles */
      printOut("(");
      parseTree(n->fg);
      /* On rajoute 1 car on est dans une ref_matrice ou un ref_struct */
      printOut("+1):");
      /* On regarde si le 2eme element est une * ou pas */
      if (n->fd->typeNode == PARAM_MATRIX_ETOILE) {
        /* C'est une etoile : on genere "end" */
        printOut("end");
      }
      else {
        /* Ce n'est pas une etoile, on genere et rajoute 1 */
        printOut("(");
        parseTree(n->fd);
        printOut("+1)");
      }
      break;
    case  EXPRESSION_FAC:
      sprintf(strTmp2, "Warning => unknown translation of exprefac at line %d in %s\n", numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case GESTION_ERREUR :
      if (catchCount > 0) { /* on essaye de catcher une io_error */
        /* on ferme les catch qui ne seraient pas encore fermes */
        i=0;
        while ((catchCount > 0) && (catchVar[catchCount]!=NULL) && (i == 0)) {
          if (catchVar[catchCount]->lineInSource >= tab) {
            gestionCatch(catchCount -1, n);
            i=0;
          } else {
            i=1;
          }
        }
        if (catchCount > 0) {
          if (catchVar[catchCount]==NULL) { /* cas du on_ioerr */
            if (strcmp((n->valNode).uString, (catchExecNode[catchCount]->valNode).uString) == 0) {
            /* Les labels correspondent */
              if (catchExecNode[catchCount]->lineInSource == tab) {
              /* le niveau d imbrication correspond */
                catchCount--;
                decTab();
                printOut("\n");
                printTab();
                printOut("catch,\n");
                printTab();
                printOut("end; ");
                printOut(commentaire);
                printOut(" on_ioerror\n");
                printTab();
                numCurrentLine += 3;
              }
            }
          }
        }
      }
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      sprintf(strTmp, "%s%s Label %s\n",commentaire,commentaire, (n->valNode).uString);
      printOut(strTmp);
      numCurrentLine++;
      free(strTmp);
      break;
    break;
    case  VAR_SYSTEM:
      if (n->fg == NULL) {
        if (strcmp((n->valNode).uString, "err_string") == 0) {
          printOut("i2mvs_error_state.msg");
          return;
        }
        if (strcmp((n->valNode).uString, "stime") == 0) {
          printOut("systime");
          return;
        }
        if (strcmp((n->valNode).uString, "error") == 0) {
          printOut("i2mvs_error_state.code");
          /*sprintf(strTmp2, "Line %d => Dangerous translation of var system <error> at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
          printToLog(strTmp2); */
          return;
        }
        strTmp = (char*) malloc (256);
        sprintf(strTmp, "i2mvs_%s",(n->valNode).uString);
        printOut(strTmp);
        free(strTmp);
        return;
      } else {
        if (n->fg->typeNode == REF_STRUCT) { /* on a une structure */
          strTmp = (char*) malloc (256);
          sprintf(strTmp, "i2mvs_%s.%s",(n->fg->fg->valNode).uString,(n->fg->fd->valNode).uString);
          printOut(strTmp);
          free(strTmp);
          return;
      	}
      }
      sprintf(strTmp2, "Line %d => unknown translation of var system at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case  CASE_STATEMENT:
      if  (inScilabTranslation==0) {
    	  printOut("switch ");
	  }
      else {
      	  printOut("select ");
	  }
      parseTree(n->fg); /* on ecrit le parametre du switch */
      incTab();
      /*printOut(",\n");
      numCurrentLine++;*/
      parseTree(n->fd); /* on ecrit les cases */
      decTab();
      printTab();
      printOut("end ");
      printOut(commentaire);
      if  (inScilabTranslation==0) {
	      	  printOut(" switch");
	  	  }
	        else {
	        	  printOut(" select");
	  }
      printOut("\n");
      numCurrentLine++;
      break;
    case  CASE_STATEMENT_SUITE:
      if(n->fg->typeNode == COMMENTSTATEMENT) {
      printTab();
      /*printOut("CASE");*/

      }
      parseTree(n->fg); /* on ecrit les cases */
      parseTree(n->fd); /* on ecrit l otherwise */
      break;
    case  CASE	:
      oldCatchCount = catchCount;
      printTab();
      printOut("case ");
      parseTree(n->fg);
      printOut(",\n");
      numCurrentLine++;
      if (n->fd != NULL) {
        if ((n->fd->fd != NULL) || (n->fd->fg != NULL)) {
          incTab();
          parseTree(n->fd);
          gestionCatch(oldCatchCount, n);
          decTab();
        }
      }
      break;
    case  CASE_ELSE:
      if (n->fg != NULL) {
        if ((n->fg->fd != NULL) || (n->fg->fg != NULL)) {
          oldCatchCount = catchCount;
          printTab();
          if  (inScilabTranslation==0) {
		  	      	  printOut("otherwise\n");
		  }
		  else {
		         	  printOut("else\n");
	  	  }

          numCurrentLine++;
          incTab();
          parseTree(n->fg);
          gestionCatch(oldCatchCount, n);
          decTab();
        }
      }
      break;
    case  CASE_SUITE:
     parseTree(n->fd); /* on ecrit les case un par un */
     parseTree(n->fg); /* on suit les suite de case */
     break;
    case  NAME	:
      /* On regarde si NAME est un identificateur ou pas */
      if (n->fg==NULL) {
        /* NAME est un identificateur, on affiche cet identificateur */
        strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
        sprintf(strTmp, "%s", (n->valNode).uString);
        printOut(strTmp);
        free(strTmp);
      } else {
        /* On genere le NAME */
        parseTree(n->fg);
      }
      break;
    case  WHILE	:
      oldCatchCount = catchCount;
      printOut("while (");
      inCondStruct = 1;
      parseTree(n->fg);
      inCondStruct = 0;
      incTab();
      printOut("),\n");
      numCurrentLine++;
      parseTree(n->fd); /* on ecrit les statements */
      gestionCatch(oldCatchCount, n);
      decTab();
      printTab();
      printOut("end");
      printOut(commentaire);
      printOut(" while");
      printOut("\n");
      numCurrentLine++;
      break;
    case  REPEAT_STATEMENT:
      printOut(commentaire);
      printOut(" Traduction du repeat en while\n");
      oldCatchCount = catchCount;
      numCurrentLine++;
      parseTree(n->fg); /* on ecrit les statements */
      printTab();
      printOut("while (~(");
      inCondStruct = 1;
      parseTree(n->fd);
      inCondStruct = 0;
      incTab();
      printOut(")),\n");
      numCurrentLine++;
      parseTree(n->fg); /* on ecrit les statements */
      gestionCatch(oldCatchCount, n);
      decTab();
      printTab();
      printOut("end");
	  printOut(commentaire);
      printOut(" while");
      printOut("\n");
      numCurrentLine++;
      break;
    case FOR:
      printOut("for ");
      oldCatchCount = catchCount;
      if (n->fg != NULL) { /* on verifie qu il est construit correctement */
        if (n->fg->fg != NULL) {
          inCondStruct = 1;
          strTmp = (char*) malloc (MAX_STR + strlen((n->fg->fg->valNode).uString));
          sprintf(strTmp, "%s = ", (n->fg->fg->valNode).uString); /* identifier */
          printOut(strTmp);
          free(strTmp);
          inserer_tuer((n->fg->fg->valNode).uString, VARIABLE); /* on ajoute l index dans la TDS */
          exp_for = 1;
          parseTree(n->fg->fg->fg);
          exp_for = 0;
          if (n->fg->fd != NULL) { /* increment */
            printOut(":");
            exp_for = 1;
            parseTree(n->fg->fd);
            exp_for = 0;
          }
          if (n->fg->fg->fd != NULL) { /* on ecrit la borne sup */
            printOut(":");
            exp_for = 1;
            parseTree(n->fg->fg->fd);
            exp_for = 0;
          }
          inCondStruct = 0;
        }
      }
      incTab();
      printOut(",\n");
      numCurrentLine++;
      parseTree(n->fd); /* on ecrit les statements */
      gestionCatch(oldCatchCount, n);
      decTab();
      printTab();
      printOut("end");
	  printOut(commentaire);
      /*printOut("for");*/
      printOut(" for\n");
      numCurrentLine++;
      break;
    case  ASSIGNMENT:
      if (n->fd == NULL) {
        /* On est dans le cas du ++ ou du --  */
        parseTree(n->fg);
        break;
      }

      nameAssignement = n->fg; /* on stocke ce qu'il y a avant le = */
      strcpy(opAssignment,(n->valNode).uString);
      /* Traitement special de la variable system QUIET et PATH*/
      if ((n->fg->valNode).uString != NULL) {
        if ((strcmp((n->fg->valNode).uString, "quiet") == 0) && (n->fg->typeNode == VAR_SYSTEM)) {
          printOut("i2mvs_quiet =  ");
          if (n->fd->typeNode == INTEGER) {
            if ((n->fg->valNode).uInt == 1) {
              printOut("1; warning on");
              return;
            }
          }
          printOut("0; warning off");
          return;
        }
        if ((strcmp((n->fg->valNode).uString, "path") == 0) && (n->fg->typeNode == VAR_SYSTEM)) {
          printOut("i2mvs_path = i2m_path(");
          parseTree(n->fd);
          printOut(")");
          return;
        }
      }
      /* Fin du Traitement du QUIET et PATH */

      /* Renommage si ce n'est pas une variable systeme ? */
      if(n->fg->typeNode != VAR_SYSTEM) {
      sprintf(strTmp2, "RESERVED %s",(n->fg->valNode).uString);
      resultat = malloc(sizeof(recType));
      if (find(strTmp2,resultat) == STATUS_OK) {
        if ((resultat->ptype->num_type == RESERVED_VAR) ||
            (resultat->ptype->num_type == RESERVED)) {
          sprintf(strTmp2, "M2I_%s",(n->fg->valNode).uString);
          strcpy((n->fg->valNode).uString, strTmp2);
        }
      }
      free(resultat);     }

      if (n->fg->fg == NULL && n->fg->fd == NULL) {
        /* On met l'identificateur (partie gauche de l'ASSIGNEMENT) dans la table des symboles */
        /* Si l'identificateur est deja present, on l'enleve  */
        resultat = malloc(sizeof(recType));
        erreurHashTable = find((n->fg->valNode).uString,resultat);
        if (erreurHashTable == STATUS_OK) {
          delete(resultat);
        }
        free(resultat);

        inserer_tuer((n->fg->valNode).uString, VARIABLE);
      }
      /* on teste si la partie droite est un fonction call */
      if (isNodeFunctionCall(n->fd)) {
      	appelFonction = 1;
        parseTree(n->fd);
        appelFonction = 0;
        return;
      }

      /* On genere le code de l'assignement */
      /* On distingue 2 cas pour generer l'assignement : la partie gauche */
      /* est un appel de structure ou pas */
      if (n->fg->typeNode == REF_STRUCT_LIST || n->fg->typeNode == REF_STRUCT)
      { /* La partie gauche est un appel de structure  */
        /* On fait alors appel a la fonction "generer_ref_struct" */
        generer_ref_struct(n->fg, n->fd);
      } else {/* On genere simplement l'assignement */
        parseTree(n->fg);
        strTmpOp = (char*) malloc (10);
        sprintf(strTmpOp," %s ",(n->valNode).uString);
        printOut(strTmpOp);
        /* On affiche un warning si (n->valNode).uString <> "=" ??  */
        free(strTmpOp);
        parseTree(n->fd);
      }
     break;
    case IDENTIFIER:
      /* Test du nom de l identificateur */
      if (n->fg != NULL) { /* Cas special du suite call = passage keyword */
        parseTree(n->fg);
        return;
      }
      if (inRef_struct_list == 0 ) {
        sprintf(strTmp2, "RESERVED %s",(n->valNode).uString);
        resultat = malloc(sizeof(recType));
        if (find(strTmp2,resultat) == STATUS_OK) {
          if ((resultat->ptype->num_type == RESERVED_VAR) ||
              (resultat->ptype->num_type == RESERVED)) {
            sprintf(strTmp2, "M2I_%s",(n->valNode).uString);
            strcpy((n->valNode).uString, strTmp2);
          }
        }
        free(resultat);
      }

      if (common == 1) {
      /* C'est un identifier dans le common. On doit le rentrer dans la table des symboles */
          inserer_tuer((n->valNode).uString, VARIABLE);
      }
      /* On affiche l'identificateur */

      printOut((n->valNode).uString);
      break;
    case INTEGER	:
      /* On affiche l'entier */
      strTmp = (char*) malloc (MAX_STR);
      sprintf(strTmp, "%d", (n->valNode).uInt);
      printOut(strTmp);
      free(strTmp);
      break;
    case REAL	:
      /* On affiche le reel */
      switch ((n->valNode).uReal[strlen((n->valNode).uReal)-1]) {
      /* on test le dernier caractere */
        case 'd' :
        case 'D' :
        case 'E' :
        case 'e' : (n->valNode).uReal[strlen((n->valNode).uReal)-1] = '\0';
      }
      printOut((n->valNode).uReal);
      break;
    case DECIMAL	:
      /* On affiche le decimal */
      i=0;
      while (i == 0)
      switch ((n->valNode).uString[strlen((n->valNode).uString)-1]) {
      /* on test le dernier caractere */
        case 'b' : case 'B' : case 'l' : case 'L' :
        case 's' : case 'S' : case 'U' : case 'u' :
          (n->valNode).uString[strlen((n->valNode).uString)-1] = '\0';
          break;
        default :
          i=1;
      }
      printOut((n->valNode).uString);
      break;
    case HEXADECIMAL	:
      i=0;
      while (i == 0)
      switch ((n->valNode).uString[strlen((n->valNode).uString)-1]) {
      /* on test le dernier caractere */
        case 'b' : case 'B' : case 'l' : case 'L' : case 'X' :
        case 's' : case 'S' : case 'U' : case 'u' : case 'x' :
          (n->valNode).uString[strlen((n->valNode).uString)-1] = '\0';
          break;
        default :
          i=1;
      }
      sprintf(strTmp2, "hex2dec(%s)", (n->valNode).uString);
      printOut(strTmp2);
      break;
    case 	OCTAL	:
      i=0;
      while (i == 0)
      switch ((n->valNode).uString[strlen((n->valNode).uString)-1]) {
      /* on test le dernier caractere */
        case 'b' : case 'B' : case 'l' : case 'L' : case 'O' :
        case 's' : case 'S' : case 'U' : case 'u' : case 'o' :
          (n->valNode).uString[strlen((n->valNode).uString)-1] = '\0';
          break;
        default :
          i=1;
      }
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      removeCharacter((n->valNode).uString, '\'' , strTmp2);
      removeCharacter(strTmp2, '"' , strTmp);
      sprintf(strTmp2, "oct2dec('%s')", strTmp);
      printOut(strTmp2);
      free(strTmp);
      break;
    case 	STRING	:
      /* On affiche la chaine de caracteres */
      strTmp = (char*) malloc (MAX_STR + strlen((n->valNode).uString));
      adaptString((n->valNode).uString, strTmp);
      printOut(strTmp);
      free(strTmp);
      break;
    case 	Or	:
      /* Genere le "OU" */
      parseTree(n->fg);
      printOut(" | ");
      parseTree(n->fd);
      break;
    case ORSHORTCUT :
      /* genere le || */
      parseTree(n->fg);
      printOut(" || ");
      parseTree(n->fd);
      break;
    case 	And	:
      /* Genere le "ET" */
      if ((n->fg->typeNode == IDENTIFIER) || (n->fg->typeNode == INTEGER) ||
      (n->fd->typeNode == IDENTIFIER) || (n->fd->typeNode == INTEGER))
      {
        printOut("bitand(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" & ");
        parseTree(n->fd);
      }
      break;
    case ANDSHORTCUT :
      /* genere le && */
      parseTree(n->fg);
      printOut(" && ");
      parseTree(n->fd);
      break;
    case 	LT	:
      /* Genere le "<" */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strclt(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" < ");
        parseTree(n->fd);
      }
      break;
    case 	UPLUS	:
      /* Genere le "+" unaire */
      printOut("+");
      parseTree(n->fg);
      break;
    case 	UMINUS	:
      /* Genere le "-" unaire */
      printOut("-");
      parseTree(n->fg);
      break;
    case PlusPlus :
      /* cas du ++ */
      if (n->fg != NULL) {
        /*  cas var++  */
        parseTree(n->fg);
        printOut("++");
      } else {
        /*  cas ++var  */
        printOut("++");
        parseTree(n->fd);
      }
      /* Affichage d'un warning ?? */
      break;
    case MoinsMoins :
      /* cas du -- */
      if (n->fg != NULL) {
        /*  cas var--  */
        parseTree(n->fg);
        printOut("--");
      } else {
        /*  cas --var  */
        printOut("--");
        parseTree(n->fd);
      }
      /* Affichage d'un warning ?? */
      break;
    case 	LE	:
      /* Genere le "<=" */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strcle(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" <= ");
        parseTree(n->fd);
      }
      break;
    case 	GT	:
      /* Genere le ">" */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strcgt(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" > ");
        parseTree(n->fd);
      }
      break;
    case GE	:
      /* Genere le "-" */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strcge(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" >= ");
        parseTree(n->fd);
      }
      break;
    case EQ	:
      /* Genere l'egalite */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strcomp(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" == ");
        parseTree(n->fd);
      }
      break;
    case NE	:
      /* Genere l'inegalite */
      if ((n->fg->typeNode == STRING) || (n->fd->typeNode == STRING)) {
        printOut("strcne(");
        parseTree(n->fg);
        printOut(", ");
        parseTree(n->fd);
        printOut(")");
      } else {
        parseTree(n->fg);
        printOut(" ~= ");
        parseTree(n->fd);
      }
      break;
    case Not	:
      /* Genere le "NOT" */
      printOut(" ~");
      parseTree(n->fg);
      break;
      /*
      printOut("i2m_not(");
      parseTree(n->fg);
      printOut(")");
      break;*/
    case TILDE :
      /* Genere le "~" */
      printOut(" ~");
      parseTree(n->fg);
      break;
    case TIMES	:
      /* Genere la multiplication */
      parseTree(n->fg);
      printOut(" .* ");
      parseTree(n->fd);
      break;
    case SLASH	:
      /* Genere la division */
      parseTree(n->fg);
      printOut(" ./ ");
      parseTree(n->fd);
      break;
    case DIESE :
      /* Genere le "#" */
      parseTree(n->fg);
      printOut(" * ");
      parseTree(n->fd);
      break;
    case DIESE2 :
      /* Genere le "##" */
      parseTree(n->fd);
      printOut(" * ");
      parseTree(n->fg);
       break;
    case Mod	:
      /* Genere le reste de la division */
      if (inScilabTranslation==0) {
            printOut("i2m_mod(");
	  }
      else {
      		printOut("modulo(");
	  }
      parseTree(n->fg);
      printOut(", ");
      parseTree(n->fd);
      printOut(")");
      break;
    case If	:
      if (testIfCatch(n) == 1) {
        return;
      }
      printOut("if (");
      inCondStruct = 1;
      parseTree(n->fg);
      inCondStruct = 0;
      incTab();
      /*printOut("), ");*/
      printOut(")\n");
      numCurrentLine++;
      parseTree(n->fd); /* on ecrit le then */
      /************ Obsolete ********************
      if (n->fd->fd != NULL) {
        if (n->fd->fd->fg != NULL) {
          if (n->fd->fd->fg->typeNode == CASE_ELSE) {
            decTab();
            printTab();
            printOut("end");
            printOut(commentaire);
            printOut("if\n");

            numCurrentLine++;
            decTab();
            parseTree(n->fd->fd->fg);
            incTab();
            return;
          }
        } ********* Fin du code obsolete *********/
        /* On parse le else */
      if (n->fd->fd != NULL) {
        parseTree(n->fd->fd);
      }
      decTab();
      printTab();
      printOut("end");
	  printOut(commentaire);
      printOut("if\n");
      /*printOut(" if");*/
      numCurrentLine++;
      break;
    case Then	:
      oldCatchCount = catchCount; /* on garde le niveau d imbriquement des catch */
      parseTree(n->fg);
      gestionCatch(oldCatchCount, n);
      break;
    case Else	:
      oldCatchCount = catchCount; /* on garde le niveau d imbriquement des catch */
      decTab();
      printTab();
      incTab();
      /*printOut("else");*/
      printOut("\n");
      numCurrentLine++;
      decTab();
      printTab();
      incTab();
      printOut("else\n");
      numCurrentLine++;
      parseTree(n->fg);
      gestionCatch(oldCatchCount, n);
      break;
    case INF	:
      /* Genere le minimum */
      printOut("min(");
      parseTree(n->fg);
      printOut(", ");
      parseTree(n->fd);
      printOut(")");
      break;
    case SUP	:
      /* Genere le maximum */
      printOut("max(");
      parseTree(n->fg);
      printOut(", ");
      parseTree(n->fd);
      printOut(")");
      break;
    case XOR	:
      /* Genere le maximum */
      printOut("xor(");
      parseTree(n->fg);
      printOut(", ");
      parseTree(n->fd);
      printOut(")");
      break;
    case ACOMMENT :
         /*printf("Commentaire : %s\n",(n->valNode).uString);*/
         printOut(commentaire);
         strTmp = (char*) malloc (strlen((n->valNode).uString)+1);
         removeCharacter((n->valNode).uString, ';', strTmp);
         printOut(strTmp);
         free(strTmp);
         /*printOut("\n");*/
         /*printTab();*/
/*         numCurrentLine++;  */
      break;
    case CR :
          printOut("\n");
          numCurrentLine++;
      break;
    case COMMENTSTATEMENT :
         /*printf("commentstatement\n");*/
         if (n->fg != NULL) {
           /*if ((n->fg->typeNode == If) || (n->fg->typeNode == REPEAT_STATEMENT)
              || (n->fg->typeNode == WHILE) || (n->fg->typeNode == FOR) || (n->fg->typeNode == Catch)
              || (n->fg->typeNode == GESTION_ERREUR)) {
                printOut("\n");
                numCurrentLine++;
                printTab();
                /*printOut("1CS");*/
           /*} else { */if ((n->fg->typeNode != CR) && (n->fg->typeNode != COMMENTSTATEMENT)
                         && (n->fg->typeNode != STATEMENT_LIST) /*&& (n->fg->typeNode != ACOMMENT)*/) {
                       printTab();
                       /*printOut("newCS");*/
                    }
           /*}*/
         }
         parseTree(n->fg);
         if (n->fg != NULL) {
            if (n->fg->typeNode == ASSIGNMENT ||
            n->fg->typeNode == FUNCTION_CALL_OU_REF_MATRIX ||
            n->fg->typeNode == FUNCTION_CALL || n->fg->typeNode == RETURN) {
              printOut(";");
            }
         }
         if (n->fd != NULL) {
           /*if (((n->fd->typeNode == If) || (n->fd->typeNode == REPEAT_STATEMENT)
              || (n->fd->typeNode == WHILE) || (n->fd->typeNode == FOR) || (n->fd->typeNode == Catch)
              || (n->fd->typeNode == GESTION_ERREUR)) && ((n->fg == NULL) || (n->fg->typeNode == CR))) {
                printOut("\n");
                printTab();
                /*printOut("2CS");*/
    /*          numCurrentLine++;
           } else {*/ if((n->fd->typeNode != CASE_SUITE) && (n->fd->typeNode != CR) && (n->fd->typeNode != CASE) && (n->fd->typeNode != COMMENTSTATEMENT)
                         && (n->fd->typeNode != STATEMENT_LIST)) {
                      printTab();
                      /*strTmp = (char *) malloc(256);
                      sprintf(strTmp,"3CS%d ",n->fg->*/
                      /*printOut("3CS");*/
                    }
           /*}*/
         }
         parseTree(n->fd);
         if (n->fd != NULL) {
            if (n->fd->typeNode == ASSIGNMENT ||
            n->fd->typeNode == FUNCTION_CALL_OU_REF_MATRIX ||
            n->fd->typeNode == FUNCTION_CALL || n->fd->typeNode == RETURN) {
              printOut(";");
            }
         }
      break;
    case Catch :
      if (n->fg->typeNode == IDENTIFIER) {
        if (catchCount > 0) { /* il faut peut etre fermer les catch ouvert au meme niveau */
          i=0;
          while ((catchCount > 0) && (catchVar[catchCount]!=NULL) && (i == 0)) {
            if (catchVar[catchCount]->lineInSource >= tab) {
              gestionCatch(catchCount -1, n);
              i=0;
              printTab();
            } else {
              i=1;
            }
          }
        }
        printOut("try,");
        incTab();
      	catchCount++;
        catchVar[catchCount] = n->fg;
        catchVar[catchCount]->lineInSource = tab;
        catchExecNode[catchCount] = NULL;
      }
      break;
    case INTERRO :
      printOut("i2m_interro(");
      parseTree(n->fg->fg);
      printOut(", ");
      parseTree(n->fg->fd);
      printOut(", ");
      parseTree(n->fd);
      printOut(")");
      break;
/*******************************************************/
/***        Cas Non traites                          ***/
/*******************************************************/
    case REF_STRUCT_PARENTHESE :
      sprintf(strTmp2, "Line %d => Unknown translation of advanced structure at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
/*******************************************************/
/***        Traitement des Objets IDL                ***/
/*******************************************************/
    case FLECHE :
      sprintf(strTmp2, "Line %d => Unknown translation of fleche statement at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case DECL_PROC_OBJ :
      sprintf(strTmp2, "Line %d => Unknown translation of object procedure declaration at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case DECL_FUNC_OBJ :
      sprintf(strTmp2, "Line %d => Unknown translation of object function declaration at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case METHODE_CALL :
      sprintf(strTmp2, "Line %d => Unknown translation of object methode call at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
    case OBJECT :
      sprintf(strTmp2, "Line %d => Unknown translation of object reference at line %d in %s\n", n->lineInSource, numCurrentLine, outFile);
      printToLog(strTmp2);
      break;
  default:
    sprintf(strTmp2, "Line %d => Translation error at node type %d at line %d in %s\n", n->lineInSource, n->typeNode, numCurrentLine, outFile);
    printToLog(strTmp2);
  } /* end switch */
 }
}


/*+ fonction principale pour generer le code traduit +*/
void genereCode (char *od, char *lfn, char* sfn, char* sdn, char* ofn, char* str,char* onefn, int translation) {
char strTmp[512];
char * strTmpLog;

  /* initialisation des variables */
  tab = 0;
  inMatrix = 0;
  inFunction  = 0;
  inRef_struct_list = 0;
  inNamedCommon = 0;
  catchCount = 0;
  ioErrCount = 0;

  fileTranslationError = 0; /* aucune erreur detecte */
  nbWarningFile = 0;
  nbLinesTotal = 0;
  pOutputFile = NULL;

  if (scriptFileTranslation) { /* idl2matlab est lance en mode script */
    replaceCharacter(ofn, '.', '_', strTmp);
    sprintf(outFile,"%s%c%s",outDir, PATHSEP, strTmp);
    if (displayMessage == 1) {
      fprintf (stderr,"Creating script %s", outFile) ;
    }
    pOutputFile = fopen(outFile, "w"); /* on ecrase le fichier cible */
    if (pOutputFile == NULL) {
      if (displayMessage == 1) {
        fprintf (stderr,"Unable to write in the output file %s\n", outFile) ;
      }
      exit(1);
    }
    /* on insere une entete en debut de fichier cible */
		if (inScilabTranslation==0) { /* traduction en matlab */
		      printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
    			sprintf(strTmp2, "%%%%  Matlab script file generated by IDL2Matlab %s %%%%\n", I2M_VERSION_2);
    			printOut(strTmp2);
    			printOut("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
		}
		else { /* traduction en scilab */
					printOut("/////////////////////////////////////////////////////////////\n");
    			sprintf(strTmp2, "/////  Scilab script file generated by IDL2Matlab %s /////\n", I2M_VERSION_2);
    			printOut(strTmp2);
    			printOut("/////////////////////////////////////////////////////////////\n");
		}

    printOut("\n");
    numCurrentLine = 5;
  }

  stringTranslationResult = str;
  strcpy(oneFunctionName , onefn);
  strcpy(outFileName, ofn);
  strcpy(sourceFileName,sfn);
  strcpy(sourceDirName,sdn);
  strcpy(outDir,od);
  strcpy(logFileName, lfn);
  setString();
  sprintf(strTmp, "%s%cidl2matlab.log", outDir, PATHSEP);
  pLogFile = fopen(strTmp, "w");
  if (pLogFile != NULL) {
    printToLog("********************************************************************************\n");
    strTmpLog = (char *) malloc(strlen(sfn)+70);
    sprintf(strTmpLog,"! Line numbers refer to the file \"%s\" !\n",sfn);
    printToLog(strTmpLog);
    free(strTmpLog);
    printToLog("________________________________________________________________________________\n\n");
      fileTranslationError = 0; /* On remet les compteurs a 0 */
      nbWarningFile = 0;
      translationError = 0;
    parseTree(root);  /* LANCE LA TRADUCTION */
    fclose(pLogFile);
  } else {
    fprintf( stderr, "Unable to open the log file\n");
    exit(0);
  }

  if (scriptFileTranslation) { /* idl2matlab est lance en mode script */

    sprintf(strTmp, "\n%s%s end of script %s\n",commentaire,commentaire, outFile);
    numCurrentLine+=2;
    printOut(strTmp);
    if (displayMessage == 1) {
      fprintf (stderr,"\t==>   %s created\t - %d lines", outFile, numCurrentLine) ;
    }
    if ((fileTranslationError) && (displayMessage == 1)) {
      fprintf (stderr," - %d WARNING(S)", nbWarningFile);
    }
    if (displayMessage == 1) {
      fprintf (stderr,"\n");
    }
    nbGeneratedFile++;
    nbWarning += nbWarningFile;
    fclose(pOutputFile);
  }
}
