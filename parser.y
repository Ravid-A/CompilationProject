/*
program: function
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();
void yyerror(const char* s);

typedef struct args
{
    char **args;
    int count;
} args;

typedef struct node
{
	char *token;
	struct node **children;
    int children_count;
} node;

node *mknode(char *token);

void add_child(node *parent, node *child);
void add_args(args *a, char *arg);
void add_args_to_node(node *parent, char *type, args *a);

void printtree(node *tree, int indent);

void printParen(int indent);

typedef enum { false, true } bool;
%}

%union {
    char* sval;
    int ival;
    float rval;
    char cval;
    bool bval;
    struct args *args;
    struct node *node;
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
%left MOD
%right NOT
%right DEREF
%right REF
%right INDEX_OPEN

%type <node> s function return_type arguments arguments_variables body statment
%type <sval> variable_type privacy_of_function is_static 
%type <args> argument_declaration

%%

start: s { printf("ACCEPT\n\n"); printtree($1, 0); }

s: s function {
        add_child($$, $2);
    };
   | function {
        $$ = mknode("CODE");
        add_child($$, $1);
   }

function: privacy_of_function return_type IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE is_static BLOCK_OPEN body BLOCK_CLOSE { 
                                                                                                                                $$ = mknode("FUNC");
                                                                                                                                add_child($$, mknode($3));
                                                                                                                                add_child($$, mknode($7));
                                                                                                                                add_child($$, mknode($1));
                                                                                                                                add_child($$, $5);
                                                                                                                                add_child($$, $2);
                                                                                                                                add_child($$, $9);
                                                                                                                            };

privacy_of_function: PUBLIC { $$ = "PUBLIC"; } 
                     | PRIVATE { $$ = "PRIVATE"; }
                     | { yyerror("Privacy of a function must be provided: \"public\" or \"private\""); };

is_static: ':' STATIC { $$ = "STATIC"; }
            | ':' { yyerror("Static keyword must be provided if \":\" is written");}
            | { $$ = "NON_STATIC"; }
            ;

return_type: variable_type  { 
                                $$ = mknode("RET");
                                add_child($$, mknode($1)); 
                            };
            | VOID { 
                    $$ = mknode("RETURN");
                    add_child($$, mknode("VOID")); 
                };

variable_type: INT { $$ = "INT"; } |
               CHAR { $$ = "CHAR"; } |
               FLOAT { $$ = "FLOAT"; } |
               DOUBLE { $$ = "DOUBLE"; } |
               STRING { $$ = "STRING"; } |
                BOOL { $$ = "BOOL"; } |
               PTR_INT { $$ = "PTR_INT"; } |
               PTR_DOUBLE { $$ = "PTR_DOUBLE"; } |
               PTR_FLOAT { $$ = "PTR_FLOAT"; } |
               PTR_CHAR { $$ = "PTR_CHAR"; } ;

arguments: ARGS arguments_variables { $$ = $2; }
               | ARGS { yyerror("Arguments of a function must be provided if \"args>>\" is written");}
               | { $$ = mknode("ARGS"); add_child($$, mknode("NONE")); }
               ;

arguments_variables: arguments_variables SEMICOL variable_type ':' argument_declaration {  add_args_to_node($$, $3, $5); }
            | arguments_variables SEMICOL { yyerror("Arguments of a function will be separated by a \";\" but no argument was provided after the semicolon"); }
            | variable_type ':' argument_declaration { 
                                                        $$ = mknode("ARGS");
                                                        add_args_to_node($$, $1, $3);
                                                     }
            | variable_type argument_declaration { yyerror("Arguments of a certain type must be separated by a ':' like \"int: x\""); }   

argument_declaration: argument_declaration COMMA IDENTIFIER { add_args($$, $3); }
                      | IDENTIFIER { add_args($$, $1); }

body : statment { $$ = mknode("BODY"); add_child($$, $1); }
        | { yyerror("Body of a function cannot be empty"); }

statment: variable_type IDENTIFIER SEMICOL { $$ = mknode("VAR_DECL"); add_child($$, mknode($1)); add_child($$, mknode($2)); }

%%


#include "lex.yy.c"

int main()
{
    return yyparse();
}

void yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
    fprintf(stderr, "Error at line %d\n", yylineno);
    exit(1);
}

node *mknode(char *token)
{
    node *newnode = (node*)malloc(sizeof(node));
    char *newstr = (char*)malloc(sizeof(token) + 1);
    strcpy(newstr,token);
    newnode->token = newstr;
    newnode->children = NULL;
    newnode->children_count = 0;
    return newnode;
}

void add_child(node *parent, node *child)
{
    if(parent->children_count == 0)
    {
        parent->children = (node**)malloc(sizeof(node*));
    }
    else
    {
        parent->children = (node**)realloc(parent->children, sizeof(node*) * (parent->children_count + 1));
    }

    parent->children[parent->children_count] = child;
    parent->children_count++;
}

void add_args(args *a, char *arg)
{
    char* newstr = (char*)malloc(sizeof(arg) + 1);
    strcpy(newstr, arg);

    if(a->count == 0)
    {
        a->args = (char**)malloc(sizeof(char*));
    }
    else
    {
        a->args = (char**)realloc(a->args, sizeof(char*) * (a->count + 1));
    }

    a->args[a->count] = (char*)malloc(sizeof(newstr) + 1);
    strcpy(a->args[a->count], newstr);
    a->count++;
}

void add_args_to_node(node *parent, char *type, args *a)
{
    node *newnode = mknode(type);
    add_child(parent, newnode);

    for(int i = 0; i < a->count; i++)
    {
        node *arg = mknode(a->args[i]);
        add_child(newnode, arg);
    }
}

void printtree(node *tree, int indent)
{
    if(!tree)
        return;

    bool print_paren = false;

    printParen(indent);

    print_paren = tree->children_count > 0;

    if(print_paren)
        printf("(");        

    printf("%s\n", tree->token);
    
    for(int i = 0; i < tree->children_count; i++)
    {
        printtree(tree->children[i], indent + 1);
    }

    if(print_paren)
    {
        printParen(indent);
        printf(")\n");
    }
}

void printParen(int indent)
{
    for(int i = 0; i < indent; i++)
    {
        printf(" \t");
    }
}