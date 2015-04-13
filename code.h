/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Abstract Tree
*   Auteurs             :       Gardon Lucien
*				Sylvestre Nadege
*   Date creation       :       19 / 04 / 2002
*   Modification        :     	23 / 05 / 2002
*
*****************************************************************************/

#ifndef CODE_H
 #define CODE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "tree.h"
#include "idl2matlab.h"
#include "lib.h"
#include "hashtable.h"

/* Fonction principale generant le code source */
/*  od est le repertoire de sortie
    lfn est le mon du fichier log
    sfn est le nom du fichier source
    sdn est le repertoire source
    ofn est le nom de fichier cible
    str est la chaine representant le code traduit (uniquement pour l'option -S
		translation est l'entier precisant le langage cible matlab:0, scilab:1
*/
void genereCode (char *od, char *lfn, char* sfn, char* sdn, char* ofn, char*
str, char* onefn, int translation);

#endif
