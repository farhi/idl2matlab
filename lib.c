/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Base de connaissance
*   Auteurs             :       Gardon Lucien
*				Sylvestre Nadege
*   Date creation       :       29 / 04 / 2002
*   Modification        :     	23 / 05 / 2002
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "type.h"
#include "hashtable.h"

/* Fichier contenant les information de traduction avancee */
/* C'est un fichier texte */

   char functionTableFile2 [256];	/* table de conversion de nom de fonctions */


/* Charge le fichier contenant toutes les traductions connues des fonctions IDL */
void loadFunctionsTranlations(void) {
  char strString[256];
  FILE *fichero;  /* pointeur sur le fichier d'informations */
  Node* n;
  int i, j, c;
  recType *rec;
  statusEnum err;
  
  #ifdef OS_UNIX
   strcpy(functionTableFile2,"./lib/lib");	/* table de conversion de nom de fonctions */

#endif
#ifdef OS_WINDOWS
   strcpy(functionTableFile2,".\\lib\\lib");	/* table de conversion de nom de fonctions */
#endif


	if (inScilabTranslation==0) { /* traduction en matlab */
          strcat(functionTableFile2, "matlab.dat");
	}
	else { /* traduction en scilab */
					strcat(functionTableFile2, "scilab.dat");
	}
  /* on ouvre le fichier en lecture */
	fichero = fopen( functionTableFile2, "r" );
  if( fichero == NULL ) {
    /* fichier introuvable */
    fprintf(stderr, "File %s not found !\n", functionTableFile2);
    exit(0);
  } else {
    while (! feof(fichero)) {
      i = 0;
      do {
        c = fgetc(fichero);
        strString[i] = c;
        i++;
      } while ((c != EOF) && (c != '\n'));
      strString[i] = '\0';
      if (strString[0] != '/') {
        i=0;
        while ((strString[i] != ';') && (strString[i] != '\0')) {
          i++;
        }
        if (strString[i] != '\0') {
          strString[i++]='\0';
          rec = malloc(sizeof(recType));
          rec->name = malloc(15 + strlen(strString));
          rec->ptype = malloc(sizeof(type));
          sprintf(rec->name, "TRANSLATION %s", strString);
          rec->ptype->num_type = TRANSLATION;
          j=0;
          n = (Node*) malloc (sizeof(Node));
          while (strString[i] != '\0') {
            (n->valNode).uString[j++] = strString[i++];
          }
          (n->valNode).uString[j] = '\0';
          rec->ptype->suite_comp = n;
          err = insert(rec);
        } else {
          free(rec);
        }
      }
    }
    fclose(fichero);
  }
}

