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

typedef enum Type 
{
    TYPE_INT,
    TYPE_CHAR,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_BOOL,
    TYPE_PTR_INT,
    TYPE_PTR_FLOAT,
    TYPE_PTR_DOUBLE,
    TYPE_PTR_CHAR
} Type;

typedef struct node
{
	char *token;
	struct node **children;
    int children_count;
    Type type;
    
} node;

typedef struct stack {
    bool *data;
    int size;
    int capacity;
} stack;

void push(stack stack, bool value);
bool pop(stack stack);

node *mknode(char *token);

bool return_required = false;
stack return_required_stack;

void add_child(node *parent, node *child);
void add_nodes_to_node(node *parent, node *child);

void printtree(node *tree, int indent);

void printParen(int indent);

char* ConcatString(char *s1, char *s2);

char* ctos(char c);
char* itos(int i);
char* ftos(float f);

int yycolumnno = 0;
%}

%union {
    char* sval;
    int ival;
    float rval;
    char cval;
    bool bval;
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
%left COLON
%right ASS
%left OR
%left AND
%left EQ NOT_EQ
%left LESS LESS_EQ GRTR GRTR_EQ
%left '+' '-' '*'
%left DIV
%left MOD
%right NOT
%right REF
%right INDEX_OPEN

%type <node> s function return_type arguments arguments_variables function_body function_call declarations functions_declarations variable_declarations
%type <node> privacy_of_function is_static variable_assignment statements expression values if_statement call_arguments statement_block
%type <node> return_statement variable_declaration possible_statements block binary_expression loop_statement argument_declaration
%type <node> variable_declaration_value variable_id_declaration code_block_statements code_block_statement code_block_declarations
%type <sval> variable_type

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc DEREF
%nonassoc UPLUS UMINUS

%%

start: s { printf("ACCEPT\n\n"); printtree($1, 0); }

s: s function {
        add_child($$, $2);
    };
   | function {
        $$ = mknode("CODE");
        add_child($$, $1);
   }

function: privacy_of_function return_type IDENTIFIER { push(return_required_stack, return_required); } PAREN_OPEN arguments PAREN_CLOSE is_static BLOCK_OPEN function_body BLOCK_CLOSE { 
                                                                                                            $$ = mknode("FUNC");
                                                                                                            add_child($$, mknode($3));
                                                                                                            add_child($$, $8);
                                                                                                            add_child($$, $1);
                                                                                                            add_child($$, $6);
                                                                                                            add_child($$, $2);
                                                                                                            add_child($$, $10);

                                                                                                            return_required = pop(return_required_stack);
                                                                                                        };

privacy_of_function: PUBLIC { $$ = mknode("PUBLIC"); }
                    | PRIVATE { $$ = mknode("PRIVATE"); }
                    | { yyerror("Privacy of a function must be specified"); }

is_static: COLON STATIC { $$ = mknode("STATIC"); }
               | { $$ = mknode("NON_STATIC"); }

return_type: variable_type  { 
                                return_required = true;
                                $$ = mknode("RET");
                                add_child($$, mknode($1)); 
                            };
            | VOID { 
                    return_required = false;
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

arguments_variables: arguments_variables SEMICOL variable_type COLON argument_declaration{ node* typenode = mknode($3); add_nodes_to_node(typenode, $5); add_child($$, typenode); }
            | arguments_variables SEMICOL { yyerror("Arguments of a function will be separated by a \";\" but no argument was provided after the semicolon"); }
            | variable_type COLON argument_declaration { 
                                                        $$ = mknode("ARGS");
                                                        node* typenode = mknode($1); 
                                                        add_nodes_to_node(typenode, $3); 
                                                        add_child($$, typenode);
                                                     }
            | variable_type argument_declaration { yyerror("Arguments of a certain type must be separated by a ':' like \"int: x\""); }   

argument_declaration: argument_declaration COMMA IDENTIFIER { add_child($$, mknode($3)); }
                      | IDENTIFIER { $$ = mknode("ARGS"); add_child($$, mknode($1)); }



variable_assignment: IDENTIFIER ASS expression { $$ = mknode(ConcatString("ASS ", $1)); add_child($$, $3); };
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS expression { $$ = mknode(ConcatString("INDEX ", $1)); add_child($$, $3); node* ass = mknode("ASS"); add_child(ass, $6); add_child($$, ass); };
                     | '*' %prec DEREF IDENTIFIER ASS expression { $$ = mknode("DEREF"); add_child($$, mknode($2)); node* ass = mknode("ASS"); add_child(ass, $4); add_child($$, ass); }
                     | IDENTIFIER ASS { yyerror("missing value for assainment."); }
                     | '*' %prec DEREF IDENTIFIER ASS { yyerror("missing value for assainment."); }
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS { yyerror("missing value for assainment."); }
                     | IDENTIFIER INDEX_OPEN INDEX_CLOSE ASS expression { yyerror("Index must be provided"); }
                     | ASS expression { yyerror("missing variable indetifier"); } ;

variable_declaration: VAR variable_type COLON variable_id_declaration { $$ = mknode("VARDEC"); node* typenode = mknode($2); add_nodes_to_node(typenode, $4); add_child($$, typenode); }
                      | VAR COLON variable_id_declaration { yyerror("Missing variable type"); }
                      | VAR variable_id_declaration { yyerror("Missing variable type");  }

variable_id_declaration: variable_id_declaration COMMA IDENTIFIER variable_declaration_value { node* varnode = mknode($3); add_child(varnode, $4); add_child($$, varnode); }
                      | IDENTIFIER variable_declaration_value { $$ = mknode("ARGS"); node* varnode = mknode($1); add_child(varnode, $2); add_child($$, varnode); }

variable_declaration_value: ASS expression { $$ = $2; }
                            | {$$ = NULL; };

expression: expression '+' expression { $$ = mknode("+"); add_child($$, $1); add_child($$, $3); } |
            expression '-' expression { $$ = mknode("-"); add_child($$, $1); add_child($$, $3); } |
            expression '*' expression { $$ = mknode("*"); add_child($$, $1); add_child($$, $3); } |
            expression DIV expression { $$ = mknode("/"); add_child($$, $1); add_child($$, $3); } |
            expression MOD expression { $$ = mknode("%"); add_child($$, $1); add_child($$, $3); } |
            values { $$ = $1; } 

values: LIT_BOOL { $$ = mknode($1? "True":"False"); } |
        LIT_CHAR { $$ = mknode(ctos($1)); } |
        LIT_INT { $$ = mknode(itos($1)); } |
        LIT_DOUBLE { $$ = mknode(ftos($1)); } |
        LIT_FLOAT { $$ = mknode(ftos($1)); } |
        LIT_STRING { $$ = mknode($1); } |
        NULL_TOKEN { $$ = mknode("NULL"); } |
        STRLEN IDENTIFIER STRLEN { $$ = mknode("STRLEN"); add_child($$, mknode($2)); } |
        STRLEN LIT_STRING STRLEN { $$ = mknode("STRLEN"); add_child($$, mknode($2)); } |
        IDENTIFIER { $$ = mknode($1); } |
        PAREN_OPEN expression PAREN_CLOSE { $$ = $2; } |
        REF expression { $$ = mknode("REF"); add_child($$, $2); } |
        '*' expression %prec DEREF { $$ = mknode("DEREF"); add_child($$, $2); } |
        '+' expression %prec UPLUS { $$ = $2; } |
        '-' expression %prec UMINUS { $$ = mknode("-"); add_child($$, $2); } |
        IDENTIFIER INDEX_OPEN expression INDEX_CLOSE { $$ = mknode(ConcatString("INDEX ", $1)); add_child($$, $3); } |
        function_call { $$ = $1; } ;

function_call: IDENTIFIER PAREN_OPEN call_arguments PAREN_CLOSE { $$ = mknode("FUNC_CALL"); add_child($$, mknode($1)); add_child($$, $3); }

call_arguments: call_arguments COMMA expression { add_child($$, $3); }
               | expression { $$ = mknode("ARGS"); add_child($$, $1); }
               | { $$ = mknode("ARGS"); add_child($$, mknode("NONE")); };

function_body: declarations statements return_statement { $$ = mknode("BODY"); add_nodes_to_node($$, $1); add_nodes_to_node($$, $2); add_child($$, $3);}
       | declarations return_statement { $$ = mknode("BODY"); add_nodes_to_node($$, $1); add_child($$, $2); }
       | declarations statements { if(return_required) yyerror("Return value is required for this function"); $$ = mknode("BODY"); add_nodes_to_node($$, $1); add_nodes_to_node($$, $2); }
       | statements return_statement { $$ = mknode("BODY"); add_nodes_to_node($$, $1); add_child($$, $2); }
       | declarations { if(return_required) yyerror("Return value is required for this function");  $$ = mknode("BODY"); add_nodes_to_node($$, $1); }
       | statements { if(return_required) yyerror("Return value is required for this function");  $$ = mknode("BODY"); add_nodes_to_node($$, $1); }
       | return_statement { $$ = mknode("BODY"); add_child($$, $1); } 
       | statements declarations { yyerror("Declarations must be before statements"); }
       | return_statement declarations { yyerror("Declarations must be before return statement"); }
       | { if(return_required) yyerror("Return value is required for this function");  $$ = mknode("BODY"); add_child($$, mknode("EMPTY")); }

block: code_block_declarations code_block_statements {$$ = mknode("BLOCK"); add_nodes_to_node($$, $1); add_nodes_to_node($$, $2); }
       | code_block_statements { $$ = mknode("BLOCK"); add_nodes_to_node($$, $1); }
       | code_block_declarations { $$ = mknode("BLOCK"); add_nodes_to_node($$, $1); }
       | code_block_statements code_block_declarations { yyerror("Declarations must be before statements"); }
       | { $$ = mknode("BLOCK"); add_child($$, mknode("EMPTY")); }

code_block_statements: code_block_statements code_block_statement  { add_child($$, $2); }
                    | code_block_statement { $$ = mknode("STATEMENTS"); add_child($$, $1); }

code_block_statement: BLOCK_OPEN block BLOCK_CLOSE { $$ = $2; }
                    | possible_statements { $$ = $1; }
                    | return_statement { $$ = $1; }

code_block_declarations: variable_declarations { $$ = $1; }
                         | function { yyerror("Function declaration cannot be inside a code block"); }

loop_statement: WHILE PAREN_OPEN binary_expression PAREN_CLOSE statement_block { $$ = mknode("WHILE"); add_child($$, $3); add_child($$, $5); }
               | DO statement_block WHILE PAREN_OPEN binary_expression PAREN_CLOSE SEMICOL { $$ = mknode("DO"); add_child($$, $2); add_child($$, $5); }
               | FOR PAREN_OPEN variable_declaration SEMICOL binary_expression SEMICOL variable_assignment PAREN_CLOSE statement_block{ $$ = mknode("FOR"); add_child($$, $3); add_child($$, $5); add_child($$, $7); add_child($$, $9); }

if_statement: IF PAREN_OPEN binary_expression PAREN_CLOSE statement_block %prec LOWER_THAN_ELSE { $$ = mknode("IF"); add_child($$, $3); add_child($$, $5); }
        | IF PAREN_OPEN binary_expression PAREN_CLOSE statement_block ELSE statement_block { $$ = mknode("IF_ELSE"); add_child($$, $3); add_child($$, $5); add_child($$, $7); }

statement_block: BLOCK_OPEN block BLOCK_CLOSE { $$ = $2; }
                | possible_statements { $$ = $1; } 
                | return_statement { $$ = $1; } ;

binary_expression: binary_expression EQ binary_expression { $$ = mknode("=="); add_child($$, $1); add_child($$, $3); }
                 | binary_expression GRTR binary_expression { $$ = mknode(">"); add_child($$, $1); add_child($$, $3); }
                 | binary_expression GRTR_EQ binary_expression { $$ = mknode(">="); add_child($$, $1); add_child($$, $3); }
                 | binary_expression LESS binary_expression { $$ = mknode("<"); add_child($$, $1); add_child($$, $3); }
                 | binary_expression LESS_EQ binary_expression { $$ = mknode("<="); add_child($$, $1); add_child($$, $3); }
                 | binary_expression NOT_EQ binary_expression { $$ = mknode("!="); add_child($$, $1); add_child($$, $3); }
                 | binary_expression AND binary_expression { $$ = mknode("&&"); add_child($$, $1); add_child($$, $3); }
                 | binary_expression OR binary_expression { $$ = mknode("||"); add_child($$, $1); add_child($$, $3); }
                 | NOT expression { $$ = mknode("NOT"); add_child($$, $2); }
                 | expression { $$ = $1; };

statements: possible_statements { $$ = mknode("STATEMENTS"); add_child($$, $1); } |
            statements possible_statements { add_child($$, $2); } ;

declarations: variable_declarations functions_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); add_nodes_to_node($$,$2); }|
              variable_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); } |
              functions_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); } |
              functions_declarations variable_declarations { yyerror("Function declarations must be before variable declarations"); } ;

variable_declarations: variable_declaration SEMICOL { $$ = mknode("VAR_DECS"); add_child($$, $1); } |
                        variable_declarations variable_declaration SEMICOL { add_child($$, $2); } ;

functions_declarations: function { $$ = mknode("FUNC_DECS"); add_child($$, $1); } |
                        functions_declarations function { add_child($$, $2); } ;

possible_statements: variable_assignment SEMICOL { $$ = $1; } |
                     loop_statement { $$ = $1; } |
                     if_statement { $$ = $1; } |
                     function_call SEMICOL { $$ = $1; };

return_statement: RETURN expression SEMICOL {if(!return_required) yyerror("Return value is not allowed for this function"); $$ = mknode("RETURN"); add_child($$, $2); }
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

char* ctos(char c)
{
    char *result = (char*)malloc(2);
    result[0] = c;
    result[1] = '\0';
    return result; 
}

char* itos(int i)
{
    char *result = (char*)malloc(11);
    sprintf(result, "%d", i);
    return result;
}

char* ftos(float f)
{
    char *result = (char*)malloc(11);
    sprintf(result, "%f", f);
    return result;
}

void push(stack stack, bool value)
{
    if(stack.size == stack.capacity)
    {
        stack.capacity *= 2;
        stack.data = (bool*)realloc(stack.data, sizeof(bool) * stack.capacity);
    }

    stack.data[stack.size++] = value;
}

bool pop(stack stack)
{
    if(stack.size == 0)
        return false;

    return stack.data[--stack.size];
}