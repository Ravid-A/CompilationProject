/*
program: function
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();
void yyerror(const char* s);

typedef struct node
{
	char *token;
	struct node *left;
	struct node *right;
} node;

node *mknode(char *token, node *left, node *right);
void printtree(node *tree);

typedef enum { false, true } bool;

node *root = NULL;
%}

%union {
    char* sval;
    int ival;
    float rval;
    char cval;
    bool bval;
}

%token <sval> IDENTIFIER LIT_STRING
%token <ival> LIT_INT
%token <rval> LIT_FLOAT LIT_DOUBLE
%token <cval> LIT_CHAR
%token <bval> LIT_BOOL

%token COMMENT_OPEN COMMENT_END
%token REF DEREF
%token SEMICOL COMMA STRLEN VAR
%token ARGS PUBLIC PRIVATE STATIC RETURN 
%token AND EQ GRTR GRTR_EQ LESS LESS_EQ NOT NOT_EQ OR 
%token BLOCK_OPEN BLOCK_CLOSE PAREN_OPEN PAREN_CLOSE INDEX_OPEN INDEX_CLOSE
%token BOOL CHAR STRING INT FLOAT DOUBLE VOID NULL_TOKEN
%token PTR_INT PTR_FLOAT PTR_DOUBLE PTR_CHAR
%token WHILE DO FOR
%token IF ELSE

%left COMMA
%right ASS
%left OR
%left AND
%left EQ NOT_EQ
%left LESS LESS_EQ GRTR GRTR_EQ
%left ADD SUB
%left MUL DIV
%right NOT
%right DEREF
%right REF
%right INDEX_OPEN

%%



s: s function_tester
   | function_tester; 

function_tester : function

function: privacy_of_function return_type IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE is_static { //crete node }

privacy_of_function: PUBLIC | PRIVATE;

is_static: ':' STATIC 
            | /* empty */
            ;

return_type: variable_type | VOID;

variable_type: INT |
               CHAR |
               FLOAT |
               DOUBLE |
               STRING |
               PTR_INT |
               PTR_DOUBLE |
               PTR_FLOAT |
               PTR_CHAR;

arguments: ARGS arguments_variables 
               | /* empty */
               ;

arguments_variables: arguments_variables SEMICOL variable_type ':' variable_declaration
            | variable_type ':' variable_declaration;

variable_declaration: variable_declaration COMMA IDENTIFIER
                      | IDENTIFIER;



%%


#include "lex.yy.c"

int main()
{
    int ret = yyparse();
    printtree(root);
    return ret;
}

void yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
    fprintf(stderr, "Error at line %d\n", yylineno);
    exit(1);
}

node *mknode(char *token, node *left, node *right)
{
    node *newnode = (node*)malloc(sizeof(node));
    char *newstr = (char*)malloc(sizeof(token) + 1);
    strcpy(newstr,token);
    newnode->left = left;
    newnode->right = right;
    newnode->token = newstr;
    return newnode;
}

void printtree(node *tree)
{
    printf("%s\n", tree->token);
    if(tree->left)
        printtree(tree->left);
    if(tree->right)
        printtree(tree->right);
}