/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Table des symbole
*   Auteurs             :       Gardon Lucien
*   Date creation       :       08 / 10 / 2002
*   Modification        :     	08 / 10 / 2002
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashtable.h"

/* Fichier contenant les information de traduction simple */
/* C'est un fichier texte */
#ifdef OS_UNIX
  const char* functionTableFile="./lib/rename.dat";
#endif
#ifdef OS_WINDOWS
  const char* functionTableFile=".\\lib\\rename.dat";	/* table de conversion de nom de fonctions */
#endif

/* Charge la table des mots reserve dans la table de hachage */
void loadReservedWord(void) {
  char strString[256];
  FILE *fichero;  /* pointeur sur le fichier d'informations */
  int i, c;
  recType *rec;
  statusEnum err;
  /* on ouvre le fichier en lecture */
  fichero = fopen( functionTableFile, "r" );
  if( fichero == NULL ) {
    /* fichier introuvable */
    fprintf(stderr, "File %s not found !\n", functionTableFile);
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
      rec = malloc(sizeof(recType));
      rec->ptype = malloc(sizeof(type));
      rec->name = (char*) malloc (strlen(strString)+15);
      i=0;
      while ((strString[i] != ';') && (strString[i] != '\0')) {
        i++;
      }
      if (strString[i] != '\0') {
        c = strString[i+1];
        strString[i]='\0';
        sprintf(rec->name, "RESERVED %s", strString);
        if (c == '0') {
          rec->ptype->num_type = RESERVED_VAR;
        } else if (c == '1') {
          rec->ptype->num_type = RESERVED_FUNCTION;
        } else {
          rec->ptype->num_type = RESERVED;
        }
        err = insert(rec);
      } else {
        free(rec);
      }
    }
    fclose(fichero);
  }
}
