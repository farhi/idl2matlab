/******************************************************************************
*                                IDL2MATLAB Project
*-----------------------------------------------------------------------------
*   ILL (Institut Laue Langevin)
*
*   38000 GRENOBLE Cedex
*-----------------------------------------------------------------------------
*   Module              :       Symbol table
*   Auteurs             :       Gardon Lucien
*                               Sylvestre Nadege
*                               Bourtembourg Reynald
*   Date creation       :       15 / 11 / 2001
*   Modification        :       07 / 07 / 2003
*
*****************************************************************************/



#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "tree.h"
#include "y.tab.h"
#include "hashtable.h"
#include "rename.h"

int nb_lines;
char *bloc_courant;


/*+ fonction recursive traduisant un noeud precis +*/
/*+ cette fonction parcours l'arbre abstrait par recurence sur les noeuds +*/
/*+ Elle ajoute dans la table des symboles le nom des procedures et fonction +*/
/*+ definies par l'utilisateur +*/
/*+ Les autres elements seront ajoutes dynamiquement lors de la generation de code+*/
void parseTreeSymb(Node *n)
{
 char* strTmp;
 recType *rec;

 if (n!=NULL) {
  switch(n->typeNode){
    case BLOCK:
        parseTreeSymb(n->fg);
        break;
    case DECLARATION_LIST:
        parseTreeSymb(n->fg);
        parseTreeSymb(n->fd);
        break;
    case  DECLARATION_PROC:
      /* si l'identificateur est reserve alors => insertion special */

      bloc_courant = (n->valNode).uString;
      rec = malloc(sizeof(recType));
      rec->ptype = malloc(sizeof(type));
      rec->name = (char*) malloc (strlen((n->valNode).uString)+5);
      sprintf(rec->name,"PRO %s",(n->valNode).uString);
      rec->ptype->num_type = PROCEDURE;
      insert(rec);
      break;
    case  DECLARATION_FUNC:
      bloc_courant = (n->valNode).uString;
      rec = malloc(sizeof(recType));
      rec->ptype = malloc(sizeof(type));
      rec->name = (char*) malloc (strlen((n->valNode).uString)+6);
      sprintf(rec->name,"FUNC %s",(n->valNode).uString);
      rec->ptype->num_type = FONCTION;
      insert(rec);
      break;
    case  COMMON_CASE :
      break;
    case  SUITE_IDENT:
      break;
    case  SUITE_PARAM:
      break;
    case  PARAM :
      break;
    case  PARAM_EXTRA:
      break;
    case  PARAM_REF_EXTRA:
      break;
    case  STATEMENT:
        break;
    case  STATEMENT_LIST:
      parseTreeSymb(n->fg);
      parseTreeSymb(n->fd);
        break;
    case COMMENTSTATEMENT:
      parseTreeSymb(n->fg);
      parseTreeSymb(n->fd);
        break;
    case ACOMMENT:
        break;
    case CR:
         break;
    case  PARENTHESE:
        break;
    case GESTION_ERREUR :
      break;
    case  AROBASE:
      break;
    case  AROBASE_POINT :
      break;
    case  RETURN:
      break;
    case Or :
    case And :
    case LT :
    case GT :
    case LE :
    case GE :
    case EQ :
    case NE :
    case PLUS :
    case MINUS  :
    case PUISS  :
    case TIMES  :
    case SLASH  :
    case Mod :
    case DIESE :
    case INF :
    case SUP :
    case XOR :
      break;
    case Not :
    case UMINUS :
    case UPLUS :
      break ;
    case  PROCEDURE_CALL:
    case  FUNCTION_CALL :
      break;
    case  SUITE_CALL_LIST:
    case  SUITE_CALL:
      break;
    case  STRUCTURE:
      break;
    case  SUITE_CONS:
      break;
    case  REF_STRUCT:
      break;
    case  MATRIX:
      break;
    case  SUITE_MATRIX  :
      break;
    case  REF_MATRIX  :
      break;
    case  PARAM_MATRIX_SUITE:
      break;
    case  PARAM_MATRIX_ETOILE :
      break;
    case  INTERVALLE  :
      break;
    case  CROCHET :
      break;
    case  SUITE_EXPRESSION:
      break;
    case  FIN_IF  :
      break;
    case  VAR_SYSTEM:
      break;
    case  FIN_ELSE  :
      break;
    case  CASE_STATEMENT:
      break;
    case  CASE_STATEMENT_SUITE:
      break;
    case  FIN_CASE:
      break;
    case  CASE  :
      parseTreeSymb(n->fd);
      break;
    case  CASE_ELSE:
      break;
    case  CASE_SUITE:
      break;
    case  NAME  :
      break;
    case  WHILE :
      break;
    case  FIN_WHILE:
      break;
    case  REPEAT_STATEMENT:
      break;
    case  FIN_REPEAT:
      break;
    case  FOR:
      break;
    case  FIN_FOR:
      break;
    case  FOR_COND:
      break;
    case  EXPRESSION_FAC:
      break;
    case FUNCTION_CALL_OU_REF_MATRIX:
        break;
    case  ASSIGNMENT:
      break;
    case IDENTIFIER   :
      break;
    case INTEGER  :
    case REAL   :
    case DECIMAL  :
    case HEXADECIMAL  :
    case OCTAL    :
    case STRING   :
      break;
    case If :
      break;
    case Then :
      break;
    case Else :
      break;
  default: break;

  } /* en switch */
 }
}


/*+ fonction principale pour generer le code traduit +*/
void genererTableSymb ()
{
  parseTreeSymb(root);
}
