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
*   Date creation       :       11 / 11 / 2001
*   Modification        :     	23 / 05 / 2002   
*                               
*****************************************************************************/

#include <stdio.h>
#include "tree.h"
#include "y.tab.h" /* on inclut les constantes definies dans le resultat de yacc */

/* Construction de l'AA (Analyse) */

extern int nb_lines;  /* variable globale indiquant le numero de la ligne courante dans le fichier IDL source */

/* ecrit a l ecran le type du noeud n */
void printNodeType(Node *n) {
  /* Selon le type du noeud, on ecrit la bonne information */
  switch(n->typeNode){
    case BLOCK:
      printf("BLOCK               ");	break;
    case DECLARATION_LIST:
      printf("DECLARATION_LIST    "); break;
    case  DECLARATION_PROC:
      printf("DECL_PROC: %s",(n->valNode).uString); break;
    case  DECLARATION_FUNC:
      printf("DECL_FUNC: %s",(n->valNode).uString); break;
    case  COMMON_CASE :
      printf("COMMON_CASE         "); break;
    case  SUITE_IDENT:
      printf("SUITE_IDENT         "); break;
    case  SUITE_PARAM:
      printf("SUITE_PARAM         "); break;
    case  Catch:
      printf("CATCH               "); break;
    case  PARAM	:		
      printf("PARAM               "); break;
    case  PARAM_EXTRA:		
      printf("PARAM_EXTRA         "); break;
    case  PARAM_REF_EXTRA:		
      printf("PARAM_REF_EXTRA     "); break; 
    case  STATEMENT:		
      printf("STATEMENT           "); break; 
    case  STATEMENT_LIST:		
      printf("STATEMENT_LIST      "); break; 
    case  PARENTHESE:		
      printf("PARENTHESE          "); break; 
    case  AROBASE:			
      printf("AROBASE             "); break; 
    case  AROBASE_POINT	:	
      printf("AROBASE_POINT       "); break; 
    case  RETURN:			
      printf("RETURN              "); break; 
    case  PLUS	:		
      printf("PLUS                "); break;
    case  MINUS	:		
      printf("MINUS               "); break; 
    case  PUISS	:		
      printf("PUISS               "); break; 
    case  PROCEDURE_CALL:
      printf("PROCEDURE_CALL      "); break;
    case  FUNCTION_CALL	:	
      printf("FUNCTION_CALL       "); break; 
    case  SUITE_CALL_LIST:		
      printf("SUITE_CALL_LIST     "); break; 
    case  SUITE_CALL:		
      printf("SUITE_CALL          "); break; 
    case  STRUCTURE:		
      printf("STRUCTURE           "); break; 
    case  SUITE_CONS:
      printf("SUITE_CONS          "); break;
    case  REF_STRUCT:
      printf("REF_STRUCT          "); break; 
    case  MATRIX:			
      printf("MATRIX              "); break; 
    case  SUITE_MATRIX	:	
      printf("SUITE_MATRIX        "); break; 
    case  REF_MATRIX	:	
      printf("REF_MATRIX          "); break; 
    case  PARAM_MATRIX_SUITE:	
      printf("PARAM_MATRIX_SUITE  "); break; 
    case  PARAM_MATRIX_ETOILE	:
      printf("PARAM_MATRIX_ETOILE "); break; 
    case  INTERVALLE	:	
      printf("INTERVALLE          "); break;
    case  CROCHET	:		
      printf("CROCHET             "); break; 
    case  SUITE_EXPRESSION:	
      printf("SUITE_EXPRESSION    "); break; 
    case  FIN_IF	:
      printf("FIN_IF              "); break;
    case  VAR_SYSTEM:
      printf("VAR_SYSTEM          "); break; 
    case  FIN_ELSE	:	
      printf("FIN_ELSE            "); break;
    case  CASE_STATEMENT:
      printf("CASE_STATEMENT      "); break; 
    case  CASE_STATEMENT_SUITE:	
      printf("CASE_STATEMENT_SUITE"); break; 
    case  FIN_CASE:		
      printf("FIN_CASE            "); break; 
    case  CASE	:		
      printf("CASE                "); break; 
    case  CASE_ELSE:			
      printf("CASE_ELSE           "); break;
    case  CASE_SUITE:		
      printf("CASE_SUITE          "); break; 
    case FUNCTION_CALL_OU_REF_MATRIX:
      printf("FUNCorMATR %s", (n->valNode).uString); break;
    case  NAME	: /* on ecrit ici l'identificateur contenu dans le noeud */
      if ((n->fd==NULL) && (n->fg==NULL)) {
  	printf("NAME : %s      ",(n->valNode).uString);
      } else {
  	printf("NAME                ");
      }
      break; 
    case  WHILE	:		
      printf("WHILE               "); break; 
    case  FIN_WHILE:		
      printf("FIN_WHILE           "); break; 
    case  REPEAT_STATEMENT:	
      printf("REPEAT_STATEMENT    "); break; 
    case  FIN_REPEAT:		
      printf("FIN_REPEAT          "); break; 
    case  FOR:
      printf("FOR                 "); break; 
    case  FIN_FOR:			
      printf("FIN_FOR             "); break; 
    case  FOR_COND:
      printf("FOR_COND            "); break;
    case  EXPRESSION_FAC:
      printf("EXPRESSION_FAC      "); break; 
    case EXP_OU_FUNCALL:
      printf("EXP_OU_FUNCALL      "); break; 
    case  ASSIGNMENT:		  
      printf("ASSIGNMENT          "); break;	
    case 	IDENTIFIER:
      printf("IDENT : %s",(n->valNode).uString); break;
    case 	INTEGER	:
      printf("INTEGER : %d   ",(n->valNode).uInt); break;
    case 	REAL	:
      printf("REAL                "); break; 
    case 	DECIMAL	:
      printf("DECIMAL             "); break; 
    case 	HEXADECIMAL	:
      printf("HEXADECIMAL         "); break; 
    case 	OCTAL	:
      printf("OCTAL               "); break; 
    case 	STRING	:
      printf("STRING              "); break;
    case 	End	:
      printf("End                 "); break; 
    case 	Begin	:
      printf("Begin               "); break; 
    case 	If	:
      printf("If                  "); break; 
    case 	Then	:
      printf("Then                "); break; 
    case 	Of	:
      printf("Of                  "); break;
    case 	Or	: 
      printf("Or                  "); break; 
    case 	 Case	:
      printf("Case                "); break; 
    case 	And	:
      printf("And                 "); break;
    case 	LT	:
      printf("LT                  "); break;
    case 	LE	:
      printf("LE                  "); break;
    case 	UMINUS	:
      printf("UMINUS              "); break; 
    case 	UPLUS	:
      printf("UPLUS               "); break; 
    case 	GT	:
      printf("GT                  "); break; 
    case GE	:
      printf("GE                  "); break; \
    case EQ	:
      printf("EQ                  "); break; 
    case NE	:
      printf("NE                  "); break; 
    case Pro	:
      printf("Pro                 "); break; 
    case Not	:
      printf("Not                 "); break; 
    case TIMES	:
      printf("TIMES               "); break;
    case SLASH	:
      printf("SLASH               "); break; 
    case Mod	:
      printf("Mod                 "); break; 
    case While	:
      printf("While               "); break; 
    case Repeat	:
      printf("Repeat              "); break; 
    case For	:
      printf("For                 "); break; 
    case Endrep	:
      printf("Endrep              "); break; 
    case Endwhile:
      printf("Endwhile            "); break; 
    case Endfor	:
      printf("Endfor              "); break; 
    case Do	:
      printf("Do                  "); break; 
    case Until	:
      printf("Until               "); break; 
    case Assign	:
      printf("Assign              "); break;
    case Else	:
      printf("Else                "); break; 
    case Endelse	:
      printf("Endelse             "); break; 
    case Endif	:
      printf("Endif               "); break; 
    case Function:
      printf("Function            "); break;
    case Common	:
      printf("Common              "); break;
    case Extra	:
      printf("Extra               "); break;
    case Ref_Extra	:
      printf("Ref_Extra           "); break;
    case Endcase	:
      printf("Endcase             "); break;
    case Return	:
      printf("Return              "); break;
    case INF	:
      printf("INF                 "); break;
    case SUP	:
      printf("SUP                 "); break;
    case OBJECT	:
      printf("OBJECT              "); break;
    case REF_STRUCT_PARENTHESE:
      printf("REF_STRUCT_PARENT   "); break;
    case INTERRO:
      printf("INTERRO             "); break;
    case DECL_PROC_OBJ:
      printf("DECL_PROC_OBJ       "); break;
    case DECL_FUNC_OBJ:
      printf("DECL_FUNC_OBJ       "); break;
    case METHODE_CALL:
      printf("METHODE_CALL        "); break;
    case IDENTIFIER_PARENTHESE:
      printf("IDENTIFIER_PARENTHESE"); break;
    case FORWARD_FUNCTION:
      printf("FORWARD_FUNCTION    "); break;
    case FLECHE:
      printf("FLECHE              "); break;
    case GESTION_ERREUR:
      printf("GESTION_ERREUR      "); break;
    case NAMED_STRUCTURE:
      printf("NAMED_STRUCTURE     "); break;
    case DIESE :
      printf("DIESE               "); break;
    case DIESE2:
      printf("DIESE2              "); break;
    case COMPILE_OPT:
      printf("COMPILE_OPT         "); break;
  default:
    printf(" CAS NON TRAITE = %d", n->typeNode);
  }
  printf(" l:%d ", n->lineInSource);
}

/* affiche le contenu d'un noeud */
void printNode (Node *n, int tab, int bfg) {
/* tab indique le niveau de tabulation courant */
/* bfg est un boolean indiquant si le node est un fils gauche de son pere */
/* il sert pour la bonne marche de l'affichage */
  int i;
  /* si c'est un fils gauche, on devra ecrire la tabulation pour l'aligne avec le reste */
  if (bfg==1) { /* on aligne le node */
    printf("  \t");
    for(i=1;i<3*tab;i++) {
      printf("\t");
    }
  }
  if (n != NULL) {
    printf("->");
    printNodeType(n); /* on ecrit le noeud */
    printf("\t");
    /* on affiche les fils par recurence */
    printNode(n->fg, tab + 1, 0); /* on affiche le fils gauche */
    printNode(n->fd, tab + 1, 1); /* on affiche le fils droit */
  } else {
    printf("->NULL\n");
  }
}

/* lance la l affichage de l arbre abstrait */
void printTree(Node *n) {
  if (n == NULL) {
    printf("L'arbre abstrait est vide\n");
  } else {
    printNode(n, 0, 0); /* on affiche la racine avec les bons parametres */
    printf("\n");
  }
}

/* Cree un noeud basique */
Node *creerNode(int t1, Node *n1, Node *n2) {
  Node *n;
  n = (Node *) calloc (1,sizeof(Node));
  n->typeNode=t1;
  n->fg=n1;
  n->fd=n2;
  (n->lineInSource) = nb_lines;
  return(n);
}

/* cree un noeud avec une valeur entiere */
Node *creerNodeInt(int t1, Node *n1, Node *n2, int x) {
  Node *n;
  n = creerNode(t1,n1,n2);
  (n->valNode).uInt=x;
  (n->lineInSource) = nb_lines;
  return(n);
}

/* cree un noeud avec une valeur reelle */
Node *creerNodeDouble(int t1, Node *n1, Node *n2, char* x) {
  return creerNodeNormalString(t1,n1,n2,x);
}

/* cree un noeud avec une valeur de chaine de carateres */
Node *creerNodeString(int t1, Node *n1, Node *n2, char *str) {
/* on met tout les identifier en minuscules pour ne pas creer d erreur */

  int i;
  Node* n;

  n = creerNode(t1,n1,n2);
  i = 0;
  if (*str == '_') {
    (n->valNode).uString[i++] = 'I';
    (n->valNode).uString[i++] = '2';
    (n->valNode).uString[i++] = 'M';
    (n->valNode).uString[i++] = 'U';
  }
  while (*str != '\0') {
    (n->valNode).uString[i++] = tolower(*str);
    str++;
  }
  (n->valNode).uString[i]='\0';
  (n->lineInSource) = nb_lines;
  return(n);
}

/* cree un noeud avec une valeur de chaine de carateres */
Node *creerNodeNormalString(int t1, Node *n1, Node *n2, char *str) {
  Node* n;

  n = creerNode(t1,n1,n2);
  strcpy((n->valNode).uString, str);
  (n->lineInSource) = nb_lines;
  return(n);
}


/* indique si le noeud n est une feuille */
int isLeaf(Node *n) {
  return ((n->fg == NULL) && (n->fd == NULL));
}

/* renvoie NULL */
Node *getNull() {
  return NULL;
}
