/*
program: function
*/
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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
void add_nodes_to_node(node *parent, node *child);

void printtree(node *tree, int indent);

void printParen(int indent);

char* ConcatString(char *s1, char *s2);

char* stoc(char c);
char* stoi(int i);
char* stof(float f);

bool current_function_has_return = false;

int yycolumnno = 0;
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

%type <node> s function return_type arguments arguments_variables function_body function_call declaration declarations
%type <node> privacy_of_function is_static variable_assignment statements expression values if_statement
%type <node> return_statement variable_declaration possible_statements block binary_expression loop_statement
%type <sval> variable_type 
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

function: privacy_of_function return_type IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE is_static BLOCK_OPEN function_body BLOCK_CLOSE { 
                                                                                                            $$ = mknode("FUNC");
                                                                                                            add_child($$, mknode($3));
                                                                                                            add_child($$, $7);
                                                                                                            add_child($$, $1);
                                                                                                            add_child($$, $5);
                                                                                                            add_child($$, $2);
                                                                                                            add_child($$, $9);
                                                                                                        };

privacy_of_function: PUBLIC { $$ = mknode("PUBLIC"); }
                    | PRIVATE { $$ = mknode("PRIVATE"); }
                    | { yyerror("Privacy of a function must be specified"); }

is_static: ':' STATIC { $$ = mknode("STATIC"); }
               | { $$ = mknode("NON_STATIC"); }

return_type: variable_type  { 
                                current_function_has_return = true;
                                $$ = mknode("RET");
                                add_child($$, mknode($1)); 
                            };
            | VOID { 
                    current_function_has_return = false;
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
               PTR_FLOAT { $$ = "PTR_FLOAT"; } |
               PTR_DOUBLE { $$ = "PTR_DOUBLE"; } |
               PTR_CHAR { $$ = "PTR_CHAR"; } ;

arguments: ARGS arguments_variables { $$ = $2; }
               | { $$ = mknode("ARGS"); add_child($$, mknode("NONE")); }
               ;

arguments_variables: arguments_variables SEMICOL variable_type ':' argument_declaration{  add_args_to_node($$, $3, $5); }
            | arguments_variables SEMICOL { yyerror("Arguments of a function will be separated by a \";\" but no argument was provided after the semicolon"); }
            | variable_type ':' argument_declaration { 
                                                        $$ = mknode("ARGS");
                                                        add_args_to_node($$, $1, $3);
                                                     }
            | variable_type argument_declaration { yyerror("Arguments of a certain type must be separated by a ':' like \"int: x\""); }   

argument_declaration: argument_declaration COMMA IDENTIFIER { add_args($$, $3); }
                      | IDENTIFIER { add_args($$, $1); }

variable_assignment: IDENTIFIER ASS expression { $$ = mknode(ConcatString("ASS ", $1)); add_child($$, $3); };
                     | IDENTIFIER ASS { yyerror("missing value for assainment."); }
                     | ASS expression { yyerror("missing variable indetifier"); } ;

variable_declaration: VAR variable_type ':' argument_declaration { $$ = mknode("VARDEC"); add_args_to_node($$, $2, $4); }

expression: expression ADD expression { $$ = mknode("+"); add_child($$, $1); add_child($$, $3); } |
            expression SUB expression { $$ = mknode("-"); add_child($$, $1); add_child($$, $3); } |
            expression MUL expression { $$ = mknode("*"); add_child($$, $1); add_child($$, $3); } |
            expression DIV expression { $$ = mknode("/"); add_child($$, $1); add_child($$, $3); } |
            expression MOD expression { $$ = mknode("%"); add_child($$, $1); add_child($$, $3); } |
            values { $$ = $1; } 

values: LIT_BOOL { $$ = mknode($1? "True":"False"); } |
        LIT_CHAR { $$ = mknode(stoc($1)); } |
        LIT_INT { $$ = mknode(stoi($1)); } |
        LIT_DOUBLE { $$ = mknode(stof($1)); } |
        LIT_FLOAT { $$ = mknode(stof($1)); } |
        LIT_STRING { $$ = mknode($1); } |
        NULL_TOKEN { $$ = mknode("NULL"); } |
        STRLEN IDENTIFIER STRLEN { $$ = mknode("STRLEN"); add_child($$, mknode($2)); } |
        STRLEN LIT_STRING STRLEN { $$ = mknode("STRLEN"); add_child($$, mknode($2)); } |
        IDENTIFIER { $$ = mknode($1); } |
        PAREN_OPEN expression PAREN_CLOSE { $$ = $2; } |
        REF IDENTIFIER { $$ = mknode(ConcatString("REF ", $2)); } |
        DEREF IDENTIFIER { $$ = mknode(ConcatString("DEREF ", $2)); } |
        IDENTIFIER INDEX_OPEN expression INDEX_CLOSE { $$ = mknode(ConcatString("INDEX ", $1)); add_child($$, $3); } |
        function_call { $$ = $1; } ;

function_call: IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE { $$ = mknode("FUNC_CALL"); add_child($$, mknode($1)); add_child($$, $3); }

function_body: declarations statements return_statement { $$ = $1; $$->token="BODY"; add_nodes_to_node($$, $2); add_child($$, $3);}
       | declarations return_statement { $$ = $1; $$->token="BODY"; add_child($$, $2); }
       | declarations statements { if(current_function_has_return) yyerror("Return value is required for this function"); $$ = $1; $$->token="BODY"; add_nodes_to_node($$, $2); }
       | statements return_statement { $$ = $1; $$->token="BODY"; add_child($$, $2); }
       | declarations { if(current_function_has_return) yyerror("Return value is required for this function");  $$ = $1; $$->token="BODY"; }
       | statements { if(current_function_has_return) yyerror("Return value is required for this function");  $$ = $1; $$->token="BODY"; }
       | return_statement { $$ = mknode("BODY"); add_child($$, $1); } 
       | statements declarations { yyerror("Declarations must be before statements"); }
       | return_statement declarations { yyerror("Declarations must be before return statement"); }
       | { if(current_function_has_return) yyerror("Return value is required for this function");  $$ = mknode("BODY"); add_child($$, mknode("EMPTY")); }

block: declarations statements return_statement { $$ = $1; $$->token="BLOCK"; add_nodes_to_node($$, $2); add_child($$, $3);}
       | declarations return_statement { $$ = $1; $$->token="BLOCK"; add_child($$, $2); }
       | declarations statements { $$ = $1; $$->token="BLOCK"; add_nodes_to_node($$, $2); }
       | statements return_statement { $$ = $1; $$->token="BLOCK"; add_child($$, $2); }
       | declarations { $$ = $1; $$->token="BLOCK"; }
       | statements { $$ = $1; $$->token="BLOCK"; }
       | return_statement { $$ = mknode("BLOCK"); add_child($$, $1); } 
       | statements declarations { yyerror("Declarations must be before statements"); }
       | return_statement declarations { yyerror("Declarations must be before return statement"); }
       | { if(current_function_has_return) yyerror("Return value is required for this function");  $$ = mknode("BLOCK"); add_child($$, mknode("EMPTY")); }

loop_statement: WHILE PAREN_OPEN binary_expression PAREN_CLOSE BLOCK_OPEN block BLOCK_CLOSE { $$ = mknode("WHILE"); add_child($$, $3); add_child($$, $6); }
               | DO BLOCK_OPEN block BLOCK_CLOSE WHILE PAREN_OPEN binary_expression PAREN_CLOSE SEMICOL { $$ = mknode("DO"); add_child($$, $3); add_child($$, $7); }
               | FOR PAREN_OPEN variable_declaration SEMICOL binary_expression SEMICOL variable_assignment PAREN_CLOSE BLOCK_OPEN block BLOCK_CLOSE { $$ = mknode("FOR"); add_child($$, $3); add_child($$, $5); add_child($$, $7); add_child($$, $10); }

if_statement: IF PAREN_OPEN binary_expression PAREN_CLOSE BLOCK_OPEN block BLOCK_CLOSE { $$ = mknode("IF"); add_child($$, $3); add_child($$, $6); }
        | IF PAREN_OPEN binary_expression PAREN_CLOSE BLOCK_OPEN block BLOCK_CLOSE ELSE BLOCK_OPEN block BLOCK_CLOSE { $$ = mknode("IF_ELSE"); add_child($$, $3); add_child($$, $6); add_child($$, $10); }

binary_expression: expression EQ expression { $$ = mknode("=="); add_child($$, $1); add_child($$, $3); }
                 | expression GRTR expression { $$ = mknode(">"); add_child($$, $1); add_child($$, $3); }
                 | expression GRTR_EQ expression { $$ = mknode(">="); add_child($$, $1); add_child($$, $3); }
                 | expression LESS expression { $$ = mknode("<"); add_child($$, $1); add_child($$, $3); }
                 | expression LESS_EQ expression { $$ = mknode("<="); add_child($$, $1); add_child($$, $3); }
                 | expression NOT_EQ expression { $$ = mknode("!="); add_child($$, $1); add_child($$, $3); }
                 | expression AND expression { $$ = mknode("&&"); add_child($$, $1); add_child($$, $3); }
                 | expression OR expression { $$ = mknode("||"); add_child($$, $1); add_child($$, $3); }
                 | NOT expression { $$ = mknode("!"); add_child($$, $2); }
                 | LIT_BOOL { $$ = mknode($1? "True":"False"); } ;

statements: possible_statements { $$ = mknode("STATEMENTS"); add_child($$, $1); } |
            statements possible_statements { add_child($$, $2); } ;

declarations: declaration { $$ = mknode("DECLARATIONS"); add_child($$, $1); } |
              declarations declaration { add_child($$, $2); } ;

declaration: variable_declaration SEMICOL { $$ = $1; } |
             function { $$ = $1; } ;

possible_statements: variable_assignment SEMICOL { $$ = $1; } |
                     loop_statement { $$ = $1; } |
                     if_statement { $$ = $1; } ;

return_statement: RETURN expression SEMICOL { if(!current_function_has_return) yyerror("Return value is not allowed for this function"); $$ = mknode("RETURN"); add_child($$, $2); }
                  | RETURN SEMICOL { $$ = mknode("RETURN"); } ;

%%


#include "lex.yy.c"

int main()
{
    return yyparse();
}

void yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
    fprintf(stderr, "Error at line %d, column: %d\n", yylineno, yycolumnno);
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
    if(!parent || !child)
        return;

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

void add_nodes_to_node(node *parent, node *child)
{
    for(int i = 0; i < child->children_count; i++)
    {
        add_child(parent, child->children[i]);
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

char* ConcatString(char *s1, char *s2)
{
    char *result = (char*)malloc(strlen(s1) + strlen(s2) + 1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

char* stoc(char c)
{
    char *result = (char*)malloc(2);
    result[0] = c;
    result[1] = '\0';
    return result;
}

char* stoi(int i)
{
    char *result = (char*)malloc(11);
    sprintf(result, "%d", i);
    return result;
}

char* stof(float f)
{
    char *result = (char*)malloc(11);
    sprintf(result, "%f", f);
    return result;
}