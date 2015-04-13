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
*   Date creation       :       10 / 04 / 2002
*   Modification        :     	23 / 05 / 2002   
*                               
*****************************************************************************/
#ifndef TREE_H
#define TREE_H

#include "type.h"

/* Definition des types des noeuds de l'arbre abstrait */
/* Ce type pernet d'identidier chaque instruction ou construction
    du language IDL */
    
#define BLOCK			11000
#define DECLARATION_LIST	11001
#define DECLARATION_PROC	11002
#define DECLARATION_FUNC	11003
#define COMMON_CASE 		11004
#define SUITE_IDENT		11006
#define SUITE_PARAM		11007
#define PARAM			11008
#define PARAM_EXTRA		11009
#define PARAM_REF_EXTRA		11010
#define STATEMENT		11011
#define STATEMENT_LIST		11012
#define PARENTHESE		11013
#define AROBASE			11014
#define AROBASE_POINT		11015
#define RETURN			11016
#define PLUS			11020
#define MINUS			11021
#define PROCEDURE_CALL		11022
#define FUNCTION_CALL		11023
#define SUITE_CALL_LIST		11024
#define SUITE_CALL		11025
#define STRUCTURE		11026
#define SUITE_CONS		11027
#define REF_STRUCT		11028
#define MATRIX			11029
#define SUITE_MATRIX		11030
#define REF_MATRIX		11031
#define PARAM_MATRIX_SUITE	11032
#define PARAM_MATRIX_ETOILE	11033
#define INTERVALLE		11034
#define CROCHET			11035
#define SUITE_EXPRESSION	11036
#define FIN_IF			11037
#define VAR_SYSTEM		11038
#define FIN_ELSE		11040
#define CASE_STATEMENT		11041
#define CASE_STATEMENT_SUITE	11042
#define FIN_CASE		11043
#define CASE			11044
#define CASE_ELSE		11045	
#define CASE_SUITE		11046
#define NAME			11047
#define WHILE			11048
#define FIN_WHILE		11049
#define REPEAT_STATEMENT	11050
#define FIN_REPEAT		11051
#define FOR			11052
#define FIN_FOR			11053
#define FOR_COND		11054
#define EXPRESSION_FAC		11055
#define ASSIGNMENT		11056
#define INF			11057
#define SUP			11058
#define PUISS			11059
#define GESTION_ERREUR		11060
#define FUNCTION_CALL_OU_REF_MATRIX 11061
#define PROC_NAME		11062
#define EXP_OU_FUNCALL		11063
#define UMINUS			11064
#define UPLUS			11065
#define NAMED_STRUCTURE		11066
#define REF_STRUCT_LIST		11067
#define IDENTIFIER_PARENTHESE	11068
#define FORWARD_FUNCTION      	11069
#define REF_STRUCT_PARENTHESE   11070
#define COMMENT       	        11071
#define TIMES       	        11072
#define INTERRO       	        11073
#define DECL_PROC_OBJ	        11074
#define DECL_FUNC_OBJ	        11075 
#define METHODE_CALL	        11076



/**********************/
/* Fonction exportees */
/**********************/

/*  Affiche l'arbre abstrait representer par la racine n */
void printTree(Node *n);

/*  Creer un noeud de type t1 definie plus haut
    n1 sera le fils gauche et n2 le fils droit */  
Node *creerNode(int t1, Node *n1, Node *n2);

/*  Creer un noeud de type t1 definie plus haut
    le noeud contiendra l'entier passe en parametre
    n1 sera le fils gauche et n2 le fils droit */  
Node *creerNodeInt(int t1, Node *n1, Node *n2, int x);

/*  Creer un noeud de type t1 definie plus haut
    le noeud contiendra le reel passe en parametre
    n1 sera le fils gauche et n2 le fils droit */  
Node *creerNodeDouble(int t1, Node *n1, Node *n2, char* x);

/*  Creer un noeud de type t1 definie plus haut
    le noeud contiendra la chaine passee en parametre en minuscule
    n1 sera le fils gauche et n2 le fils droit */  
Node *creerNodeString(int t1, Node *n1, Node *n2, char *str);

/*  Creer un noeud de type t1 definie plus haut
    le noeud contiendra la chaine passee en parametre
    n1 sera le fils gauche et n2 le fils droit */  
Node *creerNodeNormalString(int t1, Node *n1, Node *n2, char *str);

/*  Renvoie 1 si n est une feuille */  
int isLeaf(Node *n);

/*  Creer un noeud VIDE */  
Node *getNull();

#endif
