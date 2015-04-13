/******************************************************************************
*                                IDL2SCILAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Abstract Tree
*   Auteurs             :       Gardon Lucien
*                               Sylvestre Nadege
*                               Bourtembourg Reynald
*   Date creation       :       11 / 11 / 2001
*   Modification        :       07 / 07 / 2003
*
*****************************************************************************/

#ifndef TYPE_H
 #define TYPE_H

/* Old version - NE PAS MODIFIER LA TAILLE = 10 */
#define I2M_VERSION "Apr03 2003"

/* Current version - NE PAS MODIFIER LA TAILLE = 10 */
#define I2M_VERSION_2 "1.6 130501"
                      

/* OS cible */
#ifdef WIN32
  #define PATHSEP '\\'
#else
  #define PATHSEP '/'
#endif

#ifndef IDL2MATLAB
#ifdef WIN32
#define IDL2MATLAB "C:\\idl2matlab"
#else  /* !WIN32 */
#ifdef MAC
#define IDL2MATLAB ":idl2matlab" /* ToDo: What to put here? */
#else  /* !MAC */
#define IDL2MATLAB "/usr/local/lib/idl2matlab"
#endif /* !MAC */
#endif /* !WIN32 */
#endif


/******************************************************/
/* Definition des principaux types de base du systeme */
/******************************************************/

/*+ valeur d'un node +*/
/*+ Un node peut contenir plusieurs types de valeurs +*/
typedef union {
    char uString[256];	/*+ si idf +*/
    int uInt;			/*+ si denotation entiere +*/
    char uReal[256];   		/*+ si denotation reelle +*/
} leaf ;

/*+ type Node - Brique de base de l'arbre abstrait +*/
typedef struct Node {
  int typeNode ; 		/*+ le type du noeud +*/
  int lineInSource;		/*+ numero de ligne dans le fichier source +*/
  leaf valNode ;		/*+ sa valeur +*/
  struct Node *fg;		/*+ acces fils droit +*/
  struct Node *fd;		/*+ acces fils gauche +*/
} Node ;

typedef Node *PNode;		/*+ Pointeur sur un noeud +*/

typedef struct Comment {
  int lineInSource;		/*+ numero de ligne dans le fichier source +*/
  char *commentString;		/*+ commentaire +*/
  struct Comment *nextComment;	/*+ acces suivant +*/
} Comment ;


/************************************************************/
/* Definition des principales variables globales du systeme */
/************************************************************/

Node *root;			/*+ racine de l arbre abstrait +*/
Comment *commentTable; 	      	/*+ table contenant les caracteres +*/
Comment *lastComment; 	      	/*+ dernierCommentaire de la table +*/
int translationError;		/*+ indicateur d erreur pendant la traduction +*/
int fileTranslationError;	/*+ indicateur d erreur dans un fichier +*/
int numCurrentLine;		/*+ compteur de ligne courante dans le fichier cible +*/
int nbGeneratedFile;		/*+ nb de fichiers generes +*/
int nbWarningFile;		/*+ nombre de warning pour un fichier +*/
int nbWarning;			/*+ nombre total de warning +*/
int nbLinesTotal; /*+ nombre total de lignes +*/

/*+ type de fichier a traduire +*/
/*+ vaut 1 si la source est un script 0 sinon +*/
int scriptFileTranslation;
int displayMessage;/*+ affichage des messages si = 1 sinon 0 +*/
int writeWarning;/*+ ecrit les warning dans le fichier log si = 1 +*/
int writeAbstractTree;/*+ affiche l'arbre abstrait si = 1 +*/
int stringTranslation; 	      	/*+ vaut 1 en cas de traduction de chaine de car+*/
int oneFunctionTranslation;   	/*+ pour traduire 1 seule fonction +*/
int inScilabTranslation;      	/*+ pour traduire en Scilab (1 -> Scilab, 0 -> Matlab) +*/
int tabVal;   	      	      	/*+ nb d'espace pour l'indentation +*/
char* commentaire;				/*+ caractere du commentaire +*/
char i2mDirName[256];          /*+ repertoire d'IDL2MATLAB +*/

#endif
