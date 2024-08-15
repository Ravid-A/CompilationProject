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
%left '+' '-' 
%left DIV '*'
%right NOT
%right REF
%right INDEX_OPEN

%type <node> s function return_type arguments arguments_variables function_body function_call declarations functions_declarations variable_declarations code_block
%type <node> privacy_of_function is_static variable_assignment statements expression values if_statement call_arguments statement_block string_declaration function_data
%type <node> return_statement variable_declaration possible_statements block loop_statement argument_declaration for_init string_id_declaration string_declaration_value
%type <node> variable_declaration_value variable_id_declaration code_block_statements code_block_statement code_block_declarations variable_type possible_variable_declaration

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc PTR
%nonassoc UPLUS UMINUS

%%

start: s { if(!check_main_exists(current_scope)) yyerror("There is no main function");  printf("ACCEPT\n"); print_tree_to_file($1); kill_scope(current_scope); }

s: s function {
        add_child($$, $2);
    };
   | function {
        $$ = mknode("CODE");
        add_child($$, $1);
   }

function:   function_data IDENTIFIER PAREN_OPEN arguments PAREN_CLOSE is_static  {
                                                            if(check_symbol(current_scope, $2, SYMBOL_FUNCTION)) {
                                                                yyerror("Function already exists");
                                                            }

                                                            node* privacynode = $1->children[0];
                                                            node* returntypenode = $1->children[1];

                                                            add_function(current_scope, $2, returntypenode->type, $4, privacynode, $6);
                                                            make_scope();
                                                            add_arguments_to_scope(current_scope, $4);
                                                        } 
            BLOCK_OPEN function_body BLOCK_CLOSE { 
                                                    node* privacynode = $1->children[0];
                                                    node* returntypenode = $1->children[1];

                                                    $$ = mknode("FUNC");
                                                    add_child($$, mknode($2));
                                                    add_child($$, $6);
                                                    add_child($$, privacynode);
                                                    add_child($$, $4);
                                                    add_child($$, returntypenode);
                                                    add_child($$, $9);

                                                    exit_scope();
                                                };

function_data: privacy_of_function return_type { $$ = mknode("FUNC_DATA"); add_child($$, $1); add_child($$, $2); }
               | privacy_of_function { yyerror("Missing return type for function"); };
               | privacy_of_function STRING { yyerror("String cannot be a return type for a function"); };
               | return_type { yyerror("Missing privacy of function"); };

privacy_of_function: PUBLIC { $$ = mknode("PUBLIC"); }
                    | PRIVATE { $$ = mknode("PRIVATE"); };

is_static:  COLON STATIC { $$ = mknode("STATIC"); }
            | COLON { yyerror("Missing static keyword"); }
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

                                                    Symbol *sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);
                                                    if(sym->return_type != $3->type)
                                                    {
                                                        if(is_pointer(sym))
                                                        {
                                                            if($3->type != TYPE_NULL)
                                                                yyerror("The value in the assignment must be in the type of the variable");
                                                        } 
                                                        else
                                                            yyerror("The value in the assignment must be in the type of the variable");
                                                    }
                                                    

                                                    $$ = mknode(ConcatString("ASS ", $1)); 
                                                    add_child($$, $3); 
                                                };
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS expression { 
                                                                                        if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                                                                                            yyerror("Variable must be declared before the assignment statement");

                                                                                        Symbol *sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);
                                                                                        if(sym->return_type != TYPE_STRING)
                                                                                            yyerror("Index in array can only be used on a string variable.");
                                                                                        
                                                                                        if($3->type != TYPE_INT)
                                                                                            yyerror("Index must be an integer");

                                                                                        if($6->type != TYPE_CHAR)
                                                                                            yyerror("The value in the assignment must be in the type of the variable");

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

                                                                        if(pointer_to_type(sym->return_type) != $4->type)
                                                                            yyerror("The value in the assignment must be in the type of the dereferenced variable");

                                                                        $$ = mknode("DEREF"); 
                                                                        add_child($$, mknode($2)); 
                                                                        node* ass = mknode("ASS"); 
                                                                        add_child(ass, $4); 
                                                                        add_child($$, ass); 
                                                                    }
                     | IDENTIFIER ASS { yyerror("missing value for assignment."); }
                     | '*' %prec PTR IDENTIFIER ASS { yyerror("missing value for assignment."); }
                     | IDENTIFIER INDEX_OPEN expression INDEX_CLOSE ASS { yyerror("missing value for assignment."); }
                     | IDENTIFIER INDEX_OPEN INDEX_CLOSE ASS expression { yyerror("Index must be provided"); }
                     | ASS expression { yyerror("missing variable indetifier"); } 
                     | expression ASS expression { yyerror("cannot assign value to an expression."); };

string_declaration: STRING string_id_declaration    {
                                                        add_string_variables(current_scope, $2);
                                                        
                                                        $$ = mknode("VARDEC"); 
                                                        node* typenode = mknode("STRING");
                                                        add_nodes_to_node(typenode, $2); 
                                                        add_child($$, typenode);
                                                    }
                    | STRING COLON { yyerror("No need for the colon"); }

string_id_declaration: string_id_declaration COMMA IDENTIFIER INDEX_OPEN LIT_INT INDEX_CLOSE string_declaration_value   {
                                                                                                                            if(check_symbol(current_scope, $3, SYMBOL_VARIABLE))
                                                                                                                            {
                                                                                                                                yyerror("Symbol is already in use");
                                                                                                                            }

                                                                                                                            node* varnode = mknode($3); 
                                                                                                                            node* sizenode = mknode("SIZE");
                                                                                                                            add_child(sizenode, mknode(itos($5)));
                                                                                                                            add_child(varnode, sizenode);
                                                                                                                            add_child(varnode, $7);
                                                                                                                            add_child($$, varnode);
                                                                                                                        } |
                        IDENTIFIER INDEX_OPEN LIT_INT INDEX_CLOSE string_declaration_value  {
                                                                                                if(check_symbol(current_scope, $1, SYMBOL_VARIABLE))
                                                                                                {
                                                                                                    yyerror("Symbol is already in use");
                                                                                                }

                                                                                                $$ = mknode("VARDECS");
                                                                                                node* varnode = mknode($1); 
                                                                                                node* sizenode = mknode("SIZE");
                                                                                                add_child(sizenode, mknode(itos($3)));
                                                                                                add_child(varnode, sizenode);
                                                                                                add_child(varnode, $5);
                                                                                                add_child($$, varnode);
                                                                                            };

string_declaration_value:  ASS expression { if($2->type != TYPE_STRING) yyerror("Value for a string variable can only be of type string"); $$ = $2; }
                          | { $$ = NULL; }

variable_declaration: VAR variable_type COLON { make_scope(); add_variable(current_scope, "vartype", $2->type); } variable_id_declaration   { 
                                                                                                                                                exit_scope();

                                                                                                                                                $$ = mknode("VARDEC"); 
                                                                                                                                                node* typenode = $2; 
                                                                                                                                                add_nodes_to_node(typenode, $5); 
                                                                                                                                                add_child($$, typenode); 
                                                                                                                                            }
                      | VAR STRING { yyerror("This in not a valid string declaration, strings are declared like this: \"string x[30]\""); }
                      | VAR COLON { yyerror("Missing variable type"); }
                      | VAR variable_id_declaration { yyerror("Missing variable type");  }

variable_id_declaration: variable_id_declaration COMMA IDENTIFIER variable_declaration_value { 
                                                                                                Type vartype = current_scope->symbols->return_type;

                                                                                                if(check_symbol(current_scope->parent, $3, SYMBOL_VARIABLE))
                                                                                                {
                                                                                                    yyerror("Symbol is already in use");
                                                                                                }

                                                                                                if($4 && $4->type != vartype)
                                                                                                {
                                                                                                    if(!is_pointer_type(vartype) || (is_pointer_type(vartype) && $4->type != TYPE_NULL))
                                                                                                    {
                                                                                                        yyerror("Type of the value doesn't match the variable type");
                                                                                                    }
                                                                                                }

                                                                                                add_variable(current_scope->parent, $3, vartype);

                                                                                                node* varnode = mknode($3); 
                                                                                                add_child(varnode, $4); 
                                                                                                add_child($$, varnode); 
                                                                                             }
                      | IDENTIFIER variable_declaration_value { 
                                                                Type vartype = current_scope->symbols->return_type;

                                                                if(check_symbol(current_scope->parent, $1, SYMBOL_VARIABLE))
                                                                {
                                                                    yyerror("Symbol is already in use");
                                                                }

                                                                if($2 && $2->type != vartype)
                                                                {
                                                                    if(!is_pointer_type(vartype) || (is_pointer_type(vartype) && $2->type != TYPE_NULL))
                                                                    {
                                                                        yyerror("Type of the value doesn't match the variable type");
                                                                    }
                                                                }
                                                                                                
                                                                add_variable(current_scope->parent, $1, vartype);

                                                                $$ = mknode("VARDECS"); 
                                                                node* varnode = mknode($1); 
                                                                add_child(varnode, $2); 
                                                                add_child($$, varnode); 
                                                              }

variable_declaration_value: ASS expression { $$ = $2; }
                            | {$$ = NULL; };

expression: expression '+' expression { $$ = mknode("+"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression '-' expression { $$ = mknode("-"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression '*' expression { $$ = mknode("*"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression DIV expression { $$ = mknode("/"); add_child($$, $1); add_child($$, $3); $$->type = get_expression_type($1, $3); } |
            expression EQ expression    { 
                                            if($1->type != $3->type) 
                                                yyerror("Both expressions must be from the same type"); 
                                            if($1->type != TYPE_CHAR && $1->type != TYPE_BOOL && $1->type != TYPE_INT && $1->type != TYPE_FLOAT && $1->type != TYPE_DOUBLE && !is_pointer_type($1->type))
                                                    yyerror("Expressions must be of a valid type (int, float, double, char, bool)");
                                            $$ = mknode("=="); 
                                            add_child($$, $1); 
                                            add_child($$, $3); 
                                            $$->type = TYPE_BOOL; 
                                        } |
            expression NOT_EQ expression    { 
                                                if($1->type != $3->type) 
                                                    yyerror("Both expressions must be from the same type"); 
                                                if($1->type != TYPE_CHAR && $1->type != TYPE_BOOL && $1->type != TYPE_INT && $1->type != TYPE_FLOAT && $1->type != TYPE_DOUBLE && !is_pointer_type($1->type))
                                                    yyerror("Expressions must be of a valid type (int, float, double, char, bool)");

                                                $$ = mknode("!="); 
                                                add_child($$, $1); 
                                                add_child($$, $3); 
                                                $$->type = TYPE_BOOL; 
                                            } |
            expression GRTR expression  { 
                                            if($1->type != TYPE_FLOAT && $1->type != TYPE_INT && $1->type != TYPE_DOUBLE)
                                            {
                                                yyerror("Invalid type for Greater expression, must be int, float or double");
                                            }

                                            if($3->type != TYPE_FLOAT && $3->type != TYPE_INT && $3->type != TYPE_DOUBLE)
                                            {
                                                yyerror("Invalid type for Greater expression, must be int, float or double");
                                            }

                                            $$ = mknode(">"); 
                                            add_child($$, $1);
                                            add_child($$, $3); 
                                            $$->type = TYPE_BOOL; 
                                        } |
            expression GRTR_EQ expression   { 
                                                if($1->type != TYPE_FLOAT && $1->type != TYPE_INT && $1->type != TYPE_DOUBLE)
                                                {
                                                    yyerror("Invalid type for Greater or Equal expression, must be int, float or double");
                                                }

                                                if($3->type != TYPE_FLOAT && $3->type != TYPE_INT && $3->type != TYPE_DOUBLE)
                                                {
                                                    yyerror("Invalid type for Greater or Equal expression, must be int, float or double");
                                                }

                                                $$ = mknode(">="); 
                                                add_child($$, $1); 
                                                add_child($$, $3);
                                                 $$->type = TYPE_BOOL;  
                                            } |
            expression LESS expression  { 
                                            if($1->type != TYPE_FLOAT && $1->type != TYPE_INT && $1->type != TYPE_DOUBLE)
                                            {
                                                yyerror("Invalid type for Less than expression, must be int, float or double");
                                            }

                                            if($3->type != TYPE_FLOAT && $3->type != TYPE_INT && $3->type != TYPE_DOUBLE)
                                            {
                                                yyerror("Invalid type for Less than expression, must be int, float or double");
                                            }
                                            
                                            $$ = mknode("<"); 
                                            add_child($$, $1); 
                                            add_child($$, $3); 
                                            $$->type = TYPE_BOOL;  
                                        } |
            expression LESS_EQ expression   { 
                                                if($1->type != TYPE_FLOAT && $1->type != TYPE_INT && $1->type != TYPE_DOUBLE)
                                                {
                                                    yyerror("Invalid type for Less or Equal expression, must be int, float or double");
                                                }

                                                if($3->type != TYPE_FLOAT && $3->type != TYPE_INT && $3->type != TYPE_DOUBLE)
                                                {
                                                    yyerror("Invalid type for Less or Equal expression, must be int, float or double");
                                                }

                                                $$ = mknode("<="); 
                                                add_child($$, $1); 
                                                add_child($$, $3); 
                                                $$->type = TYPE_BOOL;  
                                            } |
            expression AND expression   { 
                                            if($1->type != TYPE_BOOL || $3->type != TYPE_BOOL) 
                                                yyerror("AND operator must have boolean expressions"); 
                                            $$ = mknode("&&"); 
                                            add_child($$, $1); 
                                            add_child($$, $3); 
                                            $$->type = TYPE_BOOL;  
                                        } |
            expression OR expression    { 
                                            if($1->type != TYPE_BOOL || $3->type != TYPE_BOOL) 
                                                yyerror("OR operator must have boolean expressions"); 
                                            $$ = mknode("||"); 
                                            add_child($$, $1); 
                                            add_child($$, $3);
                                            $$->type = TYPE_BOOL;  
                                        } |
            NOT expression { if($2->type != TYPE_BOOL) yyerror("NOT operator can only be used on boolean expressions"); $$ = mknode("!"); add_child($$, $2); $$->type = TYPE_BOOL; } |
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
        REF expression  { 
                            if(!is_referenceable_type($2->type))
                                yyerror("Reference is only available for reference types");

                            $$ = mknode("REF"); 
                            add_child($$, $2);
                            $$->type = type_to_pointer($2->type);
                        } |
        '*' expression %prec PTR    { 
                                        if(!is_pointer_type($2->type))
                                            yyerror("Deref is only available for pointers");

                                        $$ = mknode("DEREF"); 
                                        add_child($$, $2); 
                                        $$->type = pointer_to_type($2->type);
                                    } |
        '+' expression %prec UPLUS { $$ = $2;} |
        '-' expression %prec UMINUS { $$ = mknode("-"); add_child($$, $2); $$->type = $2->type; } |
        IDENTIFIER INDEX_OPEN expression INDEX_CLOSE    { 
                                                            if(!check_symbol_recursive(current_scope, $1, SYMBOL_VARIABLE)) 
                                                                yyerror("Variable must be declared before the use statement");

                                                            Symbol* sym = get_symbol(current_scope, $1, SYMBOL_VARIABLE);
                                                            if(sym->return_type != TYPE_STRING)
                                                                yyerror("Index in array can only be used on a string variable.");

                                                            if($3->type != TYPE_INT)
                                                                yyerror("Index must be an integer");

                                                            $$ = mknode(ConcatString("INDEX ", $1)); 
                                                            add_child($$, $3); 
                                                            $$->type = TYPE_CHAR;
                                                        } |
        function_call { $$ = $1; } ;

function_call: IDENTIFIER PAREN_OPEN call_arguments PAREN_CLOSE { 
                                                                    if(!check_symbol_recursive(current_scope, $1, SYMBOL_FUNCTION)) 
                                                                        yyerror("Function must be declared before the call statement");

                                                                    Symbol* sym = get_symbol(current_scope, $1, SYMBOL_FUNCTION);

                                                                    Symbol* current_function = get_current_function(current_scope);

                                                                    if(current_function->is_static && !sym->is_static)
                                                                        yyerror("Cannot call a non-static function from a static function");

                                                                    if(!sym->is_public && current_function->is_public && !is_function_exists_in_scope(current_scope, sym))
                                                                        yyerror("Cannot call a private function from a public function from a different scope");

                                                                    check_call_arguments(sym, $3);

                                                                    $$ = mknode("FUNC_CALL"); 
                                                                    add_child($$, mknode($1)); 
                                                                    add_child($$, $3); 

                                                                    $$->type = sym->return_type;
                                                                }

call_arguments: call_arguments COMMA expression { add_child($$, $3); }
               | expression { $$ = mknode("ARGS"); add_child($$, $1); }
               | { $$ = mknode("ARGS NONE"); };

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
    Type return_type = check_return_required(current_scope);
    if(return_type != TYPE_VOID)yyerror("Return value is required for this function"); 
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
    Type return_type = check_return_required(current_scope);
    if(return_type != TYPE_VOID) yyerror("Return value is required for this function");  
    $$ = mknode("BODY"); 
    add_nodes_to_node($$, $1);
}
| statements { 
    Type return_type = check_return_required(current_scope);
    if(return_type != TYPE_VOID) yyerror("Return value is required for this function");  
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
    Type return_type = check_return_required(current_scope);
    if(return_type != TYPE_VOID) yyerror("Return value is required for this function");  
    $$ = mknode("BODY"); 
    add_child($$, mknode("EMPTY"));
}

code_block: BLOCK_OPEN { make_scope(); } block { exit_scope(); } BLOCK_CLOSE { $$ = $3; }

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

code_block_statement:  possible_statements { $$ = $1; }
                       | return_statement { $$ = $1; }

code_block_declarations: variable_declarations { $$ = $1; }
                         | function { yyerror("Function declaration cannot be inside a code block"); }

loop_statement: WHILE PAREN_OPEN expression PAREN_CLOSE statement_block     { 
                                                                                if($3->type != TYPE_BOOL)
                                                                                    yyerror("While statement must have a boolean expression");
    
                                                                                $$ = mknode("WHILE"); 
                                                                                add_child($$, $3); 
                                                                                add_child($$, $5); 
                                                                            }
               | DO code_block WHILE PAREN_OPEN expression PAREN_CLOSE SEMICOL     { 
                                                                                            if($5->type != TYPE_BOOL)
                                                                                                yyerror("Do-While statement must have a boolean expression");

                                                                                            $$ = mknode("DO"); 
                                                                                            add_child($$, $2); 
                                                                                            add_child($$, $5); 
                                                                                        }
               | FOR PAREN_OPEN for_init SEMICOL expression SEMICOL variable_assignment PAREN_CLOSE statement_block { 
                                                                                                                                    if($5->type != TYPE_BOOL)
                                                                                                                                        yyerror("For statement must have a boolean expression");
                                                                                                                                        
                                                                                                                                    $$ = mknode("FOR"); 
                                                                                                                                    add_child($$, $3); 
                                                                                                                                    add_child($$, $5); 
                                                                                                                                    add_child($$, $7); 
                                                                                                                                    add_child($$, $9); 
                                                                                                                                } ;

for_init: variable_assignment { $$ = $1; }
          | { yyerror("For loop must have an initialization statement, which can only be a variable assignment"); }

if_statement: IF PAREN_OPEN expression PAREN_CLOSE statement_block %prec LOWER_THAN_ELSE    { 
                                                                                                if($3->type != TYPE_BOOL)
                                                                                                    yyerror("If statement must have a boolean expression"); 
                                                                                                
                                                                                                $$ = mknode("IF"); 
                                                                                                add_child($$, $3); 
                                                                                                add_child($$, $5); 
                                                                                            }
        | IF PAREN_OPEN expression PAREN_CLOSE statement_block ELSE statement_block     { 
                                                                                            if($3->type != TYPE_BOOL)
                                                                                                yyerror("If statement must have a boolean expression");

                                                                                            $$ = mknode("IF_ELSE"); 
                                                                                            add_child($$, $3); 
                                                                                            add_child($$, $5); 
                                                                                            add_child($$, $7); 
                                                                                        };

statement_block: possible_statements { $$ = $1; } 
                 | return_statement { $$ = $1; } ;

statements: possible_statements { $$ = mknode("STATEMENTS"); add_child($$, $1); } |
            statements possible_statements { add_child($$, $2); } ;

declarations: variable_declarations functions_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); add_nodes_to_node($$,$2); }|
              variable_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); } |
              functions_declarations { $$ = mknode("DECLARATIONS"); add_nodes_to_node($$, $1); } |
              functions_declarations variable_declarations { yyerror("Function declarations must be after variable declarations"); } ;

variable_declarations: possible_variable_declaration SEMICOL { $$ = mknode("VAR_DECS"); add_child($$, $1); } |
                        variable_declarations possible_variable_declaration SEMICOL { add_child($$, $2); } ;

possible_variable_declaration: variable_declaration { $$ = $1; } |
                               string_declaration { $$ = $1; } ;

functions_declarations: function { $$ = mknode("FUNC_DECS"); add_child($$, $1); } |
                        functions_declarations function { add_child($$, $2); } ;

possible_statements: variable_assignment SEMICOL { $$ = $1; } |
                     loop_statement { $$ = $1; } |
                     if_statement { $$ = $1; } |
                     function_call SEMICOL  { $$ = $1; } |
                     code_block { $$ = $1; };

return_statement: RETURN expression SEMICOL { 
                                                Type return_type = check_return_required(current_scope);
                                                if(return_type == TYPE_VOID)
                                                    yyerror("Return value is not allowed for this function"); 

                                                if(return_type != $2->type) 
                                                    yyerror("Return type does not match the function return type");

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