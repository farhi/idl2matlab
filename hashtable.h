/******************************************************************************
*                                IDL2SCILAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       table de hachage
*   Auteurs             :       ?
*                               Bourtembourg Reynald
*   Date creation       :       ?
*   Modification        :       07 / 07 / 2003
*
*****************************************************************************/

#ifndef HASHTABLE_H
 #define HASHTABLE_H


#include "type.h"

/* On entre toutes les declarations dans la table des symboles */

/*+ Ensemble des types de declarations +*/
typedef enum {
  FONCTION , MATRICE, PROCEDURE, STRUCT, SCRIPT,
  PROC_CALL, FONC_CALL, VARIABLE, REFERENCE, COMMON_T, RESERVED,
  RESERVED_FUNCTION, RESERVED_VAR, TRANSLATION, LABEL, SYSVAR, REFPARAM
} type_decl;


typedef struct {
	type_decl num_type;    /*+ Type de la declaration +*/
	Node *suite_comp;      /*+ Si num_type == REFERENCE, ensemble des parametres de la structure +*/
} type ;


/*+ user data stored in hash table +*/
typedef struct {
    char *name;       	      	/*+ Nom de la declaration +*/
    type *ptype;                /*+ optional related data +*/
} recType;

/*+ Index de la table des symboles sous forme de "int" +*/
typedef int hashIndexType;      /*+ index into hash table +*/


/*+ Type d'erreur pouvant etre renvoyee par la table des symboles +*/
typedef enum {
    STATUS_OK,
    STATUS_MEM_EXHAUSTED,
    STATUS_KEY_NOT_FOUND
} statusEnum;

/*+ Bloc contenu de la table des symboles +*/
typedef struct nodeTag {
    struct nodeTag *next;       /*+ noeud suivant +*/
    recType rec;                /*+ donnees de l'utilisateur +*/
} nodeType;

/*+ La table des symboles +*/
nodeType **hashTable;

/*+ Longueur de la table de hachage +*/
int hashTableSize;

/*+ Index de la table des symboles sous forme de "unsigned char" +*/
typedef unsigned char HashIndexType;


/******* Fonctions exportees *****/

/*+ Renvoie l'index dans la table des symboles de "str" +*/
HashIndexType hash(char *str);

/*+ Insertion de rec +*/
statusEnum insert(recType *rec);

/*+ Retourne dans rec, le bloc de nom "str" +*/
statusEnum find(char *str, recType *rec);

/*+ Enleve rec de la table des symboles +*/
statusEnum delete(recType *rec);

/*+ Affichage de la table des symboles +*/
void displayTable();

/*+ Libere la memoire utilisee par la table des symboles +*/
void freeHashTable(nodeType *p);

/*+ Supprime les objets de la table de connaissance ayant pour type "deleteType" +*/
void deleteObjetOfType(int deleteType);

#endif
