/*
program: function
*/
%{
#include "parser.h"
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

%token REF
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
%type <node> variable_declaration_value variable_id_declaration code_block_statements code_block_statement code_block_declarations variable_type

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc PTR
%nonassoc UPLUS UMINUS

%%

start: s { if(!check_main_exists(current_scope)) yyerror("There is no main function");  printf("ACCEPT"); print_tree_to_file($1); kill_scope(current_scope); }

s: s function {
        add_child($$, $2);
    };
   | function {
        $$ = mknode("CODE");
        add_child($$, $1);
   }

function:   privacy_of_function return_type IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE is_static  {
                                                            if(check_symbol(current_scope, $3, SYMBOL_FUNCTION)) {
                                                                yyerror("Function already exists");
                                                            }

                                                            add_function(current_scope, $3, $2->type, $5, $1, $7);
                                                            make_scope();
                                                            add_arguments_to_scope(current_scope, $5);
                                                        } 
            BLOCK_OPEN function_body BLOCK_CLOSE { 
                                                    $$ = mknode("FUNC");
                                                    add_child($$, mknode($3));
                                                    add_child($$, $7);
                                                    add_child($$, $1);
                                                    add_child($$, $5);
                                                    add_child($$, $2);
                                                    add_child($$, $10);

                                                    exit_scope();
                                                };

privacy_of_function: PUBLIC { $$ = mknode("PUBLIC"); }
                    | PRIVATE { $$ = mknode("PRIVATE"); }
                    | { yyerror("Privacy of a function must be specified"); }

is_static: COLON STATIC { $$ = mknode("STATIC"); }
               | { $$ = mknode("NON_STATIC"); }

return_type: variable_type  { 
                                $$ = mknode("RET");
                                add_child($$, $1); 
                                $$->type = $1->type;
                            };
            | VOID { 
                    $$ = mknode("RETURN");
                    add_child($$, mknode("VOID")); 
                    $$->type = TYPE_VOID;
                };

variable_type: INT { $$ = mknode("INT"); $$->type = TYPE_INT; } |
               CHAR { $$ = mknode("CHAR"); $$->type = TYPE_CHAR; } |
               FLOAT { $$ = mknode("FLOAT"); $$->type = TYPE_FLOAT; } |
               DOUBLE { $$ = mknode("DOUBLE"); $$->type = TYPE_DOUBLE; } |
               STRING { $$ = mknode("STRING"); $$->type = TYPE_STRING; } |
               BOOL { $$ = mknode("BOOL"); $$->type = TYPE_BOOL; } |
               PTR_INT { $$ = mknode("PTR_INT"); $$->type = TYPE_PTR_INT; } |
               PTR_FLOAT { $$ = mknode("PTR_FLOAT"); $$->type = TYPE_PTR_FLOAT; } |
               PTR_DOUBLE { $$ = mknode("PTR_DOUBLE"); $$->type = TYPE_PTR_DOUBLE; } |
               PTR_CHAR { $$ = mknode("PTR_CHAR"); $$->type = TYPE_PTR_CHAR; } ;

arguments: ARGS arguments_variables { $$ = $2; }
               | { $$ = mknode("ARGS"); add_child($$, mknode("NONE")); }
               ;

arguments_variables: arguments_variables SEMICOL variable_type COLON argument_declaration{ node* typenode = $3; add_nodes_to_node(typenode, $5); add_child($$, typenode); }
            | arguments_variables SEMICOL { yyerror("Arguments of a function will be separated by a \";\" but no argument was provided after the semicolon"); }
            | variable_type COLON argument_declaration { 
                                                        $$ = mknode("ARGS");
                                                        node* typenode = $1; 
                                                        add_nodes_to_node(typenode, $3); 
                                                        add_child($$, typenode);
                                                     }
            | variable_type argument_declaration { yyerror("Arguments of a certain type must be separated by a ':' like \"int: x\""); }   

argument_declaration: argument_declaration COMMA IDENTIFIER { add_child($$, mknode($3)); }
                      | IDENTIFIER { $$ = mknode("ARGS"); add_child($$, mknode($1)); }



variable_assignment: IDENTIFIER ASS expression { 
                                                    if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                                                        yyerror("Variable must be declared before the assignment statement"); 
                                                    $$ = mknode(ConcatString("ASS ", $1)); 
                                                    add_child($$, $3); 
                                                };
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS expression { 
                                                                                        if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                                                                                            yyerror("Variable must be declared before the assignment statement");

                                                                                        Symbol *sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);
                                                                                        if(sym->return_type != TYPE_STRING)
                                                                                            yyerror("Index in array can only be used on a string variable.");
                                                                                            

                                                                                        $$ = mknode(ConcatString("INDEX ", $1)); 
                                                                                        add_child($$, $3); 
                                                                                        node* ass = mknode("ASS"); 
                                                                                        add_child(ass, $6); 
                                                                                        add_child($$, ass); 
                                                                                    };
                     | '*' %prec PTR IDENTIFIER ASS expression    { 
                                                                        if(!check_symbol_recursive(current_scope, $2, SYMBOL_VARIABLE)) 
                                                                            yyerror("Variable must be declared before the assignment statement");

                                                                        Symbol *sym = get_symbol(current_scope, $2, SYMBOL_VARIABLE);
                                                                        if(!is_pointer(sym))
                                                                            yyerror("Deref is only available for pointers");


                                                                        $$ = mknode("DEREF"); 
                                                                        add_child($$, mknode($2)); 
                                                                        node* ass = mknode("ASS"); 
                                                                        add_child(ass, $4); 
                                                                        add_child($$, ass); 
                                                                    }
                     | IDENTIFIER ASS { yyerror("missing value for assainment."); }
                     | '*' %prec PTR IDENTIFIER ASS { yyerror("missing value for assainment."); }
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS { yyerror("missing value for assainment."); }
                     | IDENTIFIER INDEX_OPEN INDEX_CLOSE ASS expression { yyerror("Index must be provided"); }
                     | ASS expression { yyerror("missing variable indetifier"); } ;

variable_declaration: VAR variable_type COLON variable_id_declaration   { 
                                                                            add_variables(current_scope, $4, $2->type);

                                                                            $$ = mknode("VARDEC"); 
                                                                            node* typenode = $2; 
                                                                            add_nodes_to_node(typenode, $4); 
                                                                            add_child($$, typenode); 
                                                                        }
                      | VAR COLON variable_id_declaration { yyerror("Missing variable type"); }
                      | VAR variable_id_declaration { yyerror("Missing variable type");  }

variable_id_declaration: variable_id_declaration COMMA IDENTIFIER variable_declaration_value { 
                                                                                                if(check_symbol(current_scope, $3, SYMBOL_VARIABLE))
                                                                                                {
                                                                                                    yyerror("Symbol is already in use");
                                                                                                }

                                                                                                node* varnode = mknode($3); 
                                                                                                add_child(varnode, $4); 
                                                                                                add_child($$, varnode); 
                                                                                             }
                      | IDENTIFIER variable_declaration_value { 
                                                                if(check_symbol(current_scope, $1, SYMBOL_VARIABLE))
                                                                {
                                                                    yyerror("Symbol is already in use");
                                                                }

                                                                $$ = mknode("ARGS"); 
                                                                node* varnode = mknode($1); 
                                                                add_child(varnode, $2); 
                                                                add_child($$, varnode); 
                                                              }

variable_declaration_value: ASS expression { $$ = $2; }
                            | {$$ = NULL; };

expression: expression '+' expression { $$ = mknode("+"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3);  } |
            expression '-' expression { $$ = mknode("-"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression '*' expression { $$ = mknode("*"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression DIV expression { $$ = mknode("/"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression MOD expression { $$ = mknode("%"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            values { $$ = $1; } 

values: LIT_BOOL { $$ = mknode($1? "True":"False"); $$->type = TYPE_BOOL; } |
        LIT_CHAR { $$ = mknode(ctos($1)); $$->type = TYPE_CHAR; } |
        LIT_INT { $$ = mknode(itos($1)); $$->type = TYPE_INT; } |
        LIT_DOUBLE { $$ = mknode(ftos($1)); $$->type = TYPE_DOUBLE; } |
        LIT_FLOAT { $$ = mknode(ftos($1)); $$->type = TYPE_FLOAT; } |
        LIT_STRING { $$ = mknode($1); $$->type = TYPE_STRING; } |
        NULL_TOKEN { $$ = mknode("NULL"); $$->type = TYPE_NULL; } |
        STRLEN IDENTIFIER STRLEN    { 
                                        if(!check_symbol_recursive(current_scope, $2, SYMBOL_VARIABLE)) 
                                            yyerror("Variable must be declared before the use statement");

                                        Symbol* sym = get_symbol(current_scope, $2, SYMBOL_VARIABLE);
                                        if(sym->return_type != TYPE_STRING)
                                            yyerror("Strlen can only be used on a string variable.");

                                        $$ = mknode("STRLEN"); 
                                        add_child($$, mknode($2)); 
                                        $$->type = TYPE_INT; 
                                    } |
        STRLEN LIT_STRING STRLEN { $$ = mknode("STRLEN"); add_child($$, mknode($2)); $$->type = TYPE_INT; } |
        IDENTIFIER  { 
                        if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                            yyerror("Variable must be declared before the use statement");
                            
                        Symbol* sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);

                        $$ = mknode($1); 
                        $$->type = sym->return_type;
                    } |
        PAREN_OPEN expression PAREN_CLOSE { $$ = $2; } |
        REF IDENTIFIER  { 
                            if(!check_symbol_recursive(current_scope, $2, SYMBOL_VARIABLE)) 
                            yyerror("Variable must be declared before the use statement");
                            
                            Symbol* sym = get_symbol(current_scope, $2, SYMBOL_VARIABLE);
                            if(!is_referenceable(sym))
                                yyerror("Variable is not referenceable");

                            $$ = mknode("REF"); 
                            add_child($$, mknode($2));
                            $$->type = type_to_pointer(sym->return_type);
                        } |
        REF IDENTIFIER INDEX_OPEN expression INDEX_CLOSE { 
                                                            if(!check_symbol_recursive(current_scope, $2, SYMBOL_VARIABLE)) 
                                                                yyerror("Variable must be declared before the use statement");

                                                            Symbol* sym = get_symbol(current_scope, $2, SYMBOL_VARIABLE);
                                                            if(sym->return_type != TYPE_STRING)
                                                                yyerror("Index in array can only be used on a string variable.");

                                                            $$ = mknode("REF"); 
                                                            add_child($$, mknode(ConcatString("INDEX ", $2))); 
                                                            add_child($$, $4); 

                                                            $$->type = TYPE_PTR_CHAR;
                                                        } |
        '*' IDENTIFIER %prec PTR    { 
                                        if(!check_symbol_recursive(current_scope, $2, SYMBOL_VARIABLE)) 
                                            yyerror("Variable must be declared before the use statement");

                                        Symbol* sym = get_symbol(current_scope, $2, SYMBOL_VARIABLE);
                                        if(!is_pointer(sym))
                                            yyerror("Deref is only available for pointers");

                                        $$ = mknode("DEREF"); 
                                        add_child($$, mknode($2)); 
                                        $$->type = pointer_to_type(sym->return_type);
                                    } |
        '+' expression %prec UPLUS { $$ = $2;} |
        '-' expression %prec UMINUS { $$ = mknode("-"); add_child($$, $2); $$->type = $2->type; } |
        IDENTIFIER INDEX_OPEN expression INDEX_CLOSE    { 
                                                            if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                                                                yyerror("Variable must be declared before the use statement");

                                                            Symbol* sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);
                                                            if(sym->return_type != TYPE_STRING)
                                                                yyerror("Index in array can only be used on a string variable.");

                                                            $$ = mknode(ConcatString("INDEX ", $1)); 
                                                            add_child($$, $3); 
                                                            $$->type = TYPE_CHAR;
                                                        } |
        function_call { $$ = $1; } ;

function_call: IDENTIFIER PAREN_OPEN call_arguments PAREN_CLOSE { 
                                                                    if(!check_symbol_recursive(current_scope, $1, SYMBOL_FUNCTION)) 
                                                                        yyerror("Function must be declared before the call statement");

                                                                    Symbol* sym = get_symbol(current_scope, $1, SYMBOL_FUNCTION);

                                                                    if(sym->args_count != $3->children_count)
                                                                        yyerror("Function arguments count does not match the function declaration");

                                                                    $$ = mknode("FUNC_CALL"); 
                                                                    add_child($$, mknode($1)); 
                                                                    add_child($$, $3); 

                                                                    $$->type = sym->return_type;
                                                                }

call_arguments: call_arguments COMMA expression { add_child($$, $3); }
               | expression { $$ = mknode("ARGS"); add_child($$, $1); }
               | { $$ = mknode("ARGS"); add_child($$, mknode("NONE")); };

function_body: declarations statements return_statement { 
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1); 
    add_nodes_to_node($$, $2); 
    add_child($$, $3);
}
| declarations return_statement { 
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1); 
    add_child($$, $2);
}
| declarations statements { 
    if(check_return_required(current_scope)) yyerror("Return value is required for this function"); 
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1); 
    add_nodes_to_node($$, $2);
}
| statements return_statement { 
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1); 
    add_child($$, $2);
}
| declarations { 
    if(check_return_required(current_scope)) yyerror("Return value is required for this function");  
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1);
}
| statements { 
    if(check_return_required(current_scope)) yyerror("Return value is required for this function");  
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1);
}
| return_statement { 
    $$ = mknode("BODY"); 
    add_child($$, $1);
} 
| statements declarations { 
    yyerror("Declarations must be before statements");
}
| return_statement declarations { 
    yyerror("Declarations must be before return statement");
}
| { 
    if(check_return_required(current_scope)) yyerror("Return value is required for this function");  
    $$ = mknode("BODY"); 
    add_child($$, mknode("EMPTY"));
}

block: code_block_declarations code_block_statements {
    $$ = mknode("BLOCK"); 
    add_nodes_to_node($$, $1); 
    add_nodes_to_node($$, $2);
}
| code_block_statements { 
    $$ = mknode("BLOCK"); 
    add_nodes_to_node($$, $1);
}
| code_block_declarations { 
    $$ = mknode("BLOCK"); 
    add_nodes_to_node($$, $1);
}
| code_block_statements code_block_declarations { 
    yyerror("Declarations must be before statements");
}
| { 
    $$ = mknode("BLOCK"); 
    add_child($$, mknode("EMPTY"));
}

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

return_statement: RETURN expression SEMICOL { 
                                                if(!check_return_required(current_scope)) 
                                                    yyerror("Return value is not allowed for this function"); 

                                                // if(get_return_type(current_scope) != $2->type) 
                                                //     yyerror("Return type does not match the function return type");

                                                $$ = mknode("RETURN"); 
                                                add_child($$, $2); 
                                            }
                  | RETURN SEMICOL { $$ = mknode("RETURN"); } ;

%%

#include "lex.yy.c"

int main()
{
    make_scope();
    return yyparse();
}

void yyerror(const char* s)
{
    fprintf(stderr, "%s\n", s);
    fprintf(stderr, "Error at line %d, column: %d\n", yylineno, yycolumnno);
    exit(1);
}