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

#include <stdio.h>
#include <stdlib.h>
#include "hashtable.h"

/* implementation dependent declarations */

/*+ Renvoie l'index dans la table des symboles de "str" +*/
HashIndexType hash(char *str) {
  HashIndexType h = 0;
  while (*str) h += *str++;
  return h;
}

/*+ Affichage de la table des symboles +*/
void displayTable() {
  nodeType *p;
  int i;
  for ( i=0;i<256;i++) {
    p = hashTable[i];
    if (hashTable[i] != NULL) {

      while (p) {

	printf(" Nom de l'identifier  : %s\n", (p->rec).name);
	printf(" Type de l'identifier : %d\n", ((p->rec).ptype)->num_type);

	if (((p->rec).ptype) != NULL && ((p->rec).ptype)->suite_comp != NULL) {printf("suite_comp n'est pas vide\n");}
	else {printf("suite_comp est vide\n");}


	p = p->next;
      }
    }
  }
}

/*+ Insertion de rec +*/
statusEnum insert(recType *rec) {
  nodeType *p, *p0;
  hashIndexType bucket;

  /* Recherche de l'index ou on va inserer rec */
  bucket = hash(rec->name);

  /* On verifie qu'il reste de la place memoire */
  if ((p = malloc(sizeof(nodeType))) == 0)
    return STATUS_MEM_EXHAUSTED;

  /* On retrouve le debut de liste ou on va inserer rec */
  p0 = hashTable[bucket];
  hashTable[bucket] = p;

  /* On insere rec en debut de liste */
  p->next = p0;
  p->rec = *rec;
  return STATUS_OK;
}

/*+ Enleve rec de la table des symboles +*/
statusEnum delete(recType *rec) {
    nodeType *p0, *p;
    hashIndexType bucket;

    p0 = 0;
    /* Recherche de l'index ou se trouve rec */
    bucket = hash(rec->name);
    p = hashTable[bucket];

    /* Recherche de rec dans la table des symboles */
    while (p && strcmp((p->rec).name, rec->name) != 0) {
        p0 = p;
        p = p->next;
    }
    /* Message renvoye s'il ne trouve pas le rec a enlever */
    if (!p) return STATUS_KEY_NOT_FOUND;

    /* rec est present : on l'enleve */
    if (p0) p0->next = p->next;
    else hashTable[bucket] = p->next;

    free (p);
    return STATUS_OK;
}


/*+ Supprime les objets de la table des symboles ayant pour type "deleteType" +*/
void deleteObjetOfType(int deleteType) {
  nodeType *p, *p0;
  int i;

  /* On parcourt toute la table */
  for (i=0;i<256;i++) {
    p = hashTable[i];
    if (hashTable[i] != NULL) {
      p0 = NULL;
      while (p) {

	/* On regarde si le type est le meme que celui a enlever */
	if (((p->rec).ptype)->num_type == deleteType) {
	  if (p0 != NULL) {
  	      p0->next = p->next;
	      free(p);
	      p = p0->next;
	  } else {
	      hashTable[i] = p->next;
	      free(p);
	      p = hashTable[i];
	  }
	} else {
	  p0 = p;
	  p = p->next;
	}
      }
    }
  }
}


/*+ Retourne dans rec, le bloc de nom "str" +*/
statusEnum find(char *str, recType *rec) {
  nodeType *p;

  /* Recherche de l'index ou se trouve le bloc de nom "str" */
  p = hashTable[hash(str)];

  /* Recherche de "str" dans la table des symboles */
  while (p && strcmp((p->rec).name, str) !=0 ) {
    p = p->next;
  }
  /* Message renvoye quand "str" n'est pas present */
  if (!p) return STATUS_KEY_NOT_FOUND;

  /* On eretourne le "recType" de "str" */
  *rec = p->rec;
  return STATUS_OK;
}

/*+ Libere la memoire utilisee par la table des symboles +*/
void freeHashTable(nodeType *p)
{
  nodeType *l;
  int i;

  /* On parcourt toute la table */
  for ( i=0;i<256;i++) {
    p = hashTable[i];
    if (hashTable[i] != NULL) {
      while (p) {
        l = p;
        p = p->next;
        free(l);
      }
    }
  }
}
