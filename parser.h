#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>

#define POINTER_SIZE 4

int yylex();
void yyerror(const char *s);

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
    TYPE_PTR_CHAR,
    TYPE_NULL,
    TYPE_VOID
} Type;

typedef enum pretty{
    EMPTY,
    FUNC,
    LABEL,
    CODE
} pretty;

typedef enum SymbolType
{
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION
} SymbolType;

typedef struct node
{
	char *token;
	struct node **children;
    int children_count;
    Type type;
    char* code;
    char* var;
    int byte_size;
} node;

typedef struct Symbol
{
    char *name;
    SymbolType type;
    Type return_type;
    int array_size;
    Type* args;
    int args_count;
    bool is_public;
    bool is_static;
    struct Symbol *next;
} Symbol;

typedef struct Scope
{
    Symbol *symbols;
    struct Scope *parent;
} Scope;

Scope *current_scope;

node *mknode(char *token);
void kill_node(node *node);

void add_child(node *parent, node *child);
void add_nodes_to_node(node *parent, node *child);

void printtree(node *tree, int indent, FILE *file);
void print_tree_to_file(node *tree);
void printIndent(int indent, FILE *file);

void print_code_to_file(char* code);

char* ConcatString(const char *s1, const char *s2);
char* dsprintf(const char* format, ...);
char* string();
void  append(char* dest, const char* src);

char* ctos(char c);
char* itos(int i);
char* ftos(float f);

void make_scope();
void add_function(Scope *scope, char *name, Type return_type, node *args, node* is_public, node* is_static);
void add_variable(Scope *scope, char *name, Type type);
void add_string_variable(Scope *scope, char *name, int array_size);
void add_variables(Scope *scope, node *variables, Type type);
void add_string_variables(Scope *scope, node *variables);
void add_arguments_to_scope(Scope *scope, node *args);
void exit_scope();

bool check_symbol(Scope *scope, char *name, SymbolType type);
bool check_symbol_recursive(Scope *scope, char *name, SymbolType type);
Symbol *get_symbol(Scope *scope, char *name, SymbolType type);
bool check_main_exists(Scope *scope);
Type get_return_type(Scope *scope);
Type check_return_required(Scope *scope);
void kill_scope(Scope *scope);

Symbol *get_function(Scope *scope);
Symbol *get_current_function(Scope *scope);

bool is_pointer(Symbol *symbol);
bool is_pointer_type(Type type);
bool is_referenceable(Symbol *symbol);
bool is_referenceable_type(Type type);
Type type_to_pointer(Type type);
Type pointer_to_type(Type type);

Type get_expression_type(node *e1, node *e2);

int size(node* n);

void check_call_arguments(Symbol *function, node *args);
bool is_function_in_scope(Scope *scope, Symbol *symbol);
bool is_function_exists_in_scope(Scope *scope, Symbol *symbol);
bool is_function_exists_in_scope_rec(Scope *scope, Symbol *symbol);

//tac
static int count_label = 0;
static int count_var   = 0;

char* freshLabel();
char* freshVar();
char* gen(const char* lhs, const char* val1, const char* op, const char* val2);
void tacunary(node* dest, const char* op, node* src);
void tacbinary(node* dest, node* var1, const char* op, node* var2);
void tacor(node*, node*,node*);
void tacand(node*, node*, node*);
void tacnot(node* curr, node* nod);
void tacindex(node* curr, char* id, node* expr);
void tacasstoindex(node* curr, char* id, node* index, node* value);
void tacass(node* dest, node* src);
void tacfunccall(node* curr, node* func, bool ret);
void tacifstmt(node* curr, node* expr, node* tstmt, node* fstmt);
void tacdowhile(node* curr, node* expr, node* stmt);
void tacwhile(node* curr, node* expr, node* stmt);
void tacfor(node* curr, node* init, node* expr, node* update, node* stmt);
void tacfuncdec(node* curr, char* id, node* body);

int count_substring(const char *str, const char *substr);

int yycolumnno = 0;

pretty get_token(const char* s);
void prettify(FILE* in, FILE* out);

// Node functions

node *mknode(char *token)
{
    node *newnode = (node*)malloc(sizeof(node));
    char *newstr = (char*)malloc(sizeof(token) + 1);
    strcpy(newstr,token);
    newnode->token = newstr;
    newnode->children = NULL;
    newnode->children_count = 0;
    newnode->code = string();
    newnode->var = string();
    newnode->byte_size = 0;

    return newnode;
}

void kill_node(node *node)
{
    if(!node)
        return;

    for(int i = 0; i < node->children_count; i++)
    {
        kill_node(node->children[i]);
    }

    free(node->token);
    free(node->children);
    free(node->var);
    free(node->code);
    free(node);
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

void print_tree_to_file(node *tree)
{
    FILE *file = fopen("tree.txt", "w");
    if(file == NULL)
    {
        yyerror("Could not open file");
    }

    printtree(tree, 0, file);

    fclose(file);
}

void printtree(node *tree, int indent, FILE *file)
{
    if(!tree)
        return;
    
    bool print_paren = false;

    printIndent(indent, file);

    print_paren = tree->children_count > 0;

    if(print_paren)
        fprintf(file, "(");   

    fprintf(file, "%s\n", tree->token);
    
    for(int i = 0; i < tree->children_count; i++)
    {
        printtree(tree->children[i], indent + 1, file);
    }

    if(print_paren)
    {
        printIndent(indent, file);
        fprintf(file, ")\n");
    }
}

void printIndent(int indent, FILE *file)
{
    for(int i = 0; i < indent; i++)
    {
        fprintf(file, " ");
    }
}

void print_code_to_file(char* code)
{
    FILE *temp = fopen("temp.3ac", "w");
    if(temp == NULL)
    {
        yyerror("Could not open file");
    }

    fprintf(temp, "%s", code);

    FILE *out = fopen("code.3ac", "w");
    if(out == NULL)
    {
        yyerror("Could not open file");
    }

    fclose(temp);

    temp = fopen("temp.3ac", "r");
    if(temp == NULL)
    {
        yyerror("Could not open file");
    }

    prettify(temp, out);

    fclose(out);
    fclose(temp);
    remove("temp.3ac");
}

// String functions

char* ConcatString(const char *s1, const char *s2)
{
    char *result = (char*)malloc(strlen(s1) + strlen(s2) + 1);
    // strcpy(result, s1);
    // strcat(result, s2);
    int index = 0;
    const char* c = s1;
    while(*c != '\0')
        result[index++] = *c++;
    c = s2;
    while(*c != '\0')
        result[index++] = *c++;
    result[index] = '\0';
    return result;
}

char *dsprintf(const char *format, ...)
{
    va_list args;
    va_start(args, format);

    va_list args_copy;
    va_copy(args_copy,args);
    int size = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);

    if (size < 0) {
        va_end(args);
        return NULL;
    }

    char* buffer = (char*)malloc(size + 1);
    if (!buffer) {
        va_end(args);
        return NULL;
    }

    vsnprintf(buffer, size + 1, format, args);
    va_end(args);

    return buffer;
}

//creates empty string
char *string()
{
    char* res = (char*)malloc(sizeof(char));
    res[0] = '\0';
    return res;
}

//modify the given string `dest` and append src at the end.
void append(char *dest, const char *src)
{
    if(!src || strlen(src) == 0)
        return;
    char* res = ConcatString(dest, src);
    //free(dest);
    dest = strdup(res);
}

// TAC functions

char* gen(const char *lhs, const char *val1, const char *op, const char *val2)
{
    char* res = strdup(lhs);
    res = ConcatString(res, " = ");
    res = ConcatString(res, val1);
    res = ConcatString(res," ");
    res = ConcatString(res, op);
    res = ConcatString(res," ");
    res = ConcatString(res, val2);
    res = ConcatString(res,"\n");

    res = ConcatString("", res);
    return res;
}

//generating 3ac for unary operator: var1 = op var0
void tacunary(node *dest, const char* op, node *src)
{
    char* generated = gen(dest->var, "", op, src->var);
    dest->code = ConcatString(dest->code, src->code);
    dest->var = ConcatString(op, src->var);
    free(generated);
}

//generating 3ac for binary operator: var2 = var0 op var1
void tacbinary(node *dest, node *var1, const char *op, node *var2)
{
    dest->var = freshVar();
    char* generated = gen(dest->var, var1->var, op, var2->var);
    dest->code = ConcatString(dest->code, var1->code);
    dest->code = ConcatString(dest->code, var2->code);
    dest->code = ConcatString(dest->code, generated);
    dest->byte_size += var1->byte_size + var2->byte_size + size(dest);
}

void tacor(node* curr, node* expr1, node* expr2){
    char* str;
    curr->var = freshVar();
    char* lt = freshLabel();
    char* next = freshLabel();

    curr->code = ConcatString(curr->code, expr1->code);

    str = dsprintf("if %s goto %s\n", expr1->var, lt);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->code = ConcatString(curr->code, expr2->code);

    str = dsprintf("if %s goto %s\n", expr2->var, lt);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s = 0\n", curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("goto %s\n", next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:%s = 1\n", lt, curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:",next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += 4 + expr1->byte_size + expr2->byte_size;
}

void tacand(node* curr, node* expr1, node* expr2){
    char* str;
    curr->var = freshVar();
    char* lf = freshLabel();
    char* next = freshLabel();

    curr->code = ConcatString(curr->code, expr1->code);

    str = dsprintf("ifz %s goto %s\n", expr1->var, lf);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->code = ConcatString(curr->code, expr2->code);

    str = dsprintf("ifz %s goto %s\n", expr2->var, lf);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s = 1\n", curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("goto %s\n", next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:%s = 0\n", lf, curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:",next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += 4 + expr1->byte_size + expr2->byte_size;
}

void tacnot(node *curr, node *nod)
{
    char* str;
    curr->var = freshVar();
    char* lf = freshLabel();
    char* next = freshLabel();

    curr->code = ConcatString(curr->code, nod->code);

    str = dsprintf("ifz %s goto %s\n", nod->var, lf);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s = 0\n", curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("goto %s\n", next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:%s = 1\n", lf, curr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:",next);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += 4 + nod->byte_size;
}

void tacindex(node *curr, char *id, node *expr)
{
    char* str;
    char* ptr = freshVar();
    curr->var = freshVar();

    curr->code = ConcatString(curr->code, expr->code);

    str = dsprintf("%s = %s + %s\n", ptr, id, expr->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s = *%s\n", curr->var, ptr);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += POINTER_SIZE + 1 + expr->byte_size;
}

void tacasstoindex(node* curr, char* id, node* index, node* value)
{
    char* str;
    char* ptr = freshVar();

    curr->code = ConcatString(curr->code, index->code);
    curr->code = ConcatString(curr->code, value->code);

    str = dsprintf("%s = %s + %s\n", ptr, id, index->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("*%s = %s\n", ptr, value->var);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += POINTER_SIZE + index->byte_size + value->byte_size;
}

void tacfunccall(node* curr, node* func, bool ret)
{
    char* str;
    char* var = string();
    int byte_size = 0;

    if(ret)
    {
        var = freshVar();
        curr->var = strdup(var);
        var = ConcatString(var, " = ");

        byte_size += size(func);
    }

    int size_t = 0;

    char* func_id = strdup(func->children[0]->token);
    node* args = func -> children[1];

    for(int i = args->children_count-1; i >= 0; i--)
    {
        node* arg = args->children[i];

        curr->code = ConcatString(curr->code, arg->code);

        str = dsprintf("PushParam %s\n", arg->var);
        curr->code = ConcatString(curr->code, str);
        free(str);

        byte_size += arg->byte_size;
        size_t += size(arg);
    }

    str = dsprintf("%sLCall %s\n", var, func_id);
    curr->code = ConcatString(curr->code, str);
    free(str);

    if(size_t)
    {
        str = dsprintf("PopParams %d\n", size_t);
        curr->code = ConcatString(curr->code, str);
        free(str);
    }

    curr->byte_size += byte_size;
}

void tacifstmt(node* curr, node* expr, node* tstmt, node* fstmt)
{
    char* str;
    char* fl = freshLabel();
    char* next;

    if(fstmt) next = freshLabel();

    curr->code = ConcatString(curr->code, expr->code);

    str = dsprintf("ifz %s goto %s\n", expr->var, fl);
    str = ConcatString(str, tstmt->code);

    if(fstmt) str = ConcatString(str, dsprintf("goto %s\n", next));

    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:", fl);

    if(fstmt)
    {
        str = ConcatString(str, fstmt->code);
    }

    curr->code = ConcatString(curr->code, str);
    free(str);

    if(fstmt)
    {
        str = dsprintf("%s:", next);
        curr->code = ConcatString(curr->code, str);
        free(str);
    }

    curr->byte_size += expr->byte_size + tstmt->byte_size + fstmt->byte_size;
}

void tacdowhile(node* curr, node* expr, node* stmt)
{
    char* looplabel = freshLabel(); 
    char* str;

    str = dsprintf("%s:%s", looplabel, stmt->code);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->code = ConcatString(curr->code, expr->code);

    str = dsprintf("if %s goto %s\n", expr->var, looplabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += expr->byte_size + stmt->byte_size;
}

void tacwhile(node* curr, node* expr, node* stmt)
{
    char* looplabel = freshLabel(); 
    char* endlabel = freshLabel();
    char* str;

    str = dsprintf("%s:%s", looplabel, expr->code);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("ifz %s goto %s\n", expr->var, endlabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->code = ConcatString(curr->code, stmt->code);

    str = dsprintf("goto %s\n", looplabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:", endlabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += expr->byte_size + stmt->byte_size;
}

void tacfor(node* curr, node* init, node* expr, node* update, node* stmt)
{
    char* looplabel = freshLabel(); 
    char* updatelabel = freshLabel();
    char* afterupdate = freshLabel();
    char* endlabel = freshLabel();
    char* str;

    curr->code = ConcatString(curr->code, init->code);

    str = dsprintf("%s:%s", looplabel, expr->code);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("ifz %s goto %s\n", expr->var, endlabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("goto %s\n", afterupdate);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:", updatelabel);
    str = ConcatString(str, update->code);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("goto %s\n", looplabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:", afterupdate);
    str = ConcatString(str, stmt->code);
    curr->code = ConcatString(curr->code, str);
    free(str);
    
    str = dsprintf("goto %s\n", updatelabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("%s:", endlabel);
    curr->code = ConcatString(curr->code, str);
    free(str);

    curr->byte_size += init->byte_size + expr->byte_size + update->byte_size + stmt->byte_size;
}

void tacfuncdec(node* curr, char* id, node* body)
{
    char* endLbl;
    char* str;
    bool empty_return = count_substring(body->code, "%s") != 0;

    str = dsprintf("%s:\n", id);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = dsprintf("BeginFunc %d\n", body->byte_size);
    curr->code = ConcatString(curr->code, str);
    free(str);

    if(empty_return)
    {
        endLbl = freshLabel();
    }

    str = dsprintf(body->code, endLbl);
    curr->code = ConcatString(curr->code, str);
    free(str);

    str = string();
    if(empty_return) str = dsprintf("%s:", endLbl);
    str = dsprintf("%sEndFunc\n\n\n", str);
    curr->code = ConcatString(curr->code, str);
    free(str);
}

char* ctos(char c)
{
    char *result = (char*)malloc(4);
    sprintf(result,"\'%c\'", c);
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

// Scope functions

void make_scope()
{
    Scope *new_scope = (Scope*)malloc(sizeof(Scope));
    new_scope->symbols = NULL;
    new_scope->parent = current_scope;
    current_scope = new_scope;
}

void add_function(Scope *scope, char *name, Type return_type, node *args, node* is_public, node* is_static)
{
    if(strcmp(name, "main") == 0 && scope->parent != NULL)
    {
        yyerror("Main function cannot be declared outside of the global scope");
    }

    if(check_symbol(scope, name, SYMBOL_FUNCTION))
    {
        yyerror("Symbol already exists");
    }

    Symbol *new_symbol = (Symbol*)malloc(sizeof(Symbol));
    new_symbol->name = name;
    new_symbol->type = SYMBOL_FUNCTION;
    new_symbol->return_type = return_type;
    new_symbol->is_public = (strcmp(is_public->token, "PUBLIC") == 0);
    new_symbol->is_static = (strcmp(is_static->token, "STATIC") == 0);

    if(args)
    {
        new_symbol->args = (Type*)malloc(sizeof(Type) * args->children_count);

        int index = 0;
        for(int i = 0; i < args->children_count; i++)
        {
            node* arg = args->children[i];
            for(int j = 0; j < arg->children_count; j++)
            {
                new_symbol->args[index++] = arg->type;
            }
        }
        new_symbol->args_count = index;
    }
    else
    {
        new_symbol->args_count = 0;
        new_symbol->args = NULL;
    }

    new_symbol->next = scope->symbols;
    scope->symbols = new_symbol;
}

void add_variable(Scope *scope, char *name, Type type)
{
    if(strcmp(name, "main") == 0)
    {
        yyerror("Variable name cannot be main");
    }

    Symbol *new_symbol = (Symbol*)malloc(sizeof(Symbol));
    new_symbol->name = name;
    new_symbol->type = SYMBOL_VARIABLE;
    new_symbol->return_type = type;
    new_symbol->next = scope->symbols;
    scope->symbols = new_symbol;
}

void add_string_variable(Scope *scope, char *name, int array_size)
{
    Symbol *new_symbol = (Symbol*)malloc(sizeof(Symbol));
    new_symbol->name = name;
    new_symbol->type = SYMBOL_VARIABLE;
    new_symbol->return_type = TYPE_STRING;
    new_symbol->array_size = array_size;
    new_symbol->next = scope->symbols;
    scope->symbols = new_symbol;
}

void add_variables(Scope *scope, node *variables, Type type)
{
    for(int i = 0; i < variables->children_count; i++)
    {
        node* variable = variables->children[i];
        char* name = strdup(variable->token);
        add_variable(scope, name, type);
    }
}

void add_string_variables(Scope *scope, node *variables)
{
    for(int i = 0; i < variables->children_count; i++)
    {
        node* variable = variables->children[i];
        char* name = strdup(variable->token);
        node* size = variable->children[0];
        int array_size = atoi(size->children[0]->token);
        add_string_variable(scope, name, array_size);
    }
}

void add_arguments_to_scope(Scope *scope, node *args)
{
    for(int i = 0; i < args->children_count; i++)
    {
        node* arg = args->children[i];
        for(int j = 0; j < arg->children_count; j++)
        {
            char* name = strdup(arg->children[j]->token);
            add_variable(scope, name, arg->type);
        }
    }
}

void exit_scope()
{
    Scope *parent = current_scope->parent;
    kill_scope(current_scope);
    current_scope = parent;
}

bool check_symbol(Scope *scope, char *name, SymbolType type)
{
    if(!scope)
        return false;


    Symbol *current = scope->symbols;
    while(current)
    {
        if(strcmp(current->name, name) == 0)
            return true;
        current = current->next;
    }
    return false;
}

bool check_symbol_recursive(Scope *scope, char *name, SymbolType type)
{
    if(!scope)
        return false;

    if(check_symbol(scope, name, type))
        return true;

    return check_symbol_recursive(scope->parent, name, type);
}

Symbol *get_symbol(Scope *scope, char *name, SymbolType type)
{
    if(!scope)
        return NULL;

    Symbol *current = scope->symbols;
    while(current)
    {
        if(strcmp(current->name, name) == 0 && current->type == type)
            return current;
        current = current->next;
    }
    return get_symbol(scope->parent, name, type);
}

bool check_main_exists(Scope *scope)
{
    Symbol *current = scope->symbols;
    while(current)
    {
        if(current->type == SYMBOL_FUNCTION && strcmp(current->name, "main") == 0)
        {
            if(!current->is_public)
            {
                yyerror("Main function must be public");
            }

            if(!current->is_static)
            {
                yyerror("Main function must be static");
            }

            if(current->args_count != 0)
            {
                yyerror("Main function must have no arguments");
            }

            if(current->return_type != TYPE_VOID)
            {
                yyerror("Main function must be a void function");
            }

            return true;
        }
        current = current->next;
    }
    return false;
}

Type get_return_type(Scope *scope)
{
    Symbol *current = scope->symbols;
    while(current)
    {
        if(current->type == SYMBOL_FUNCTION)
        {
            return current->return_type;
        }
        current = current->next;
    }
    return get_return_type(scope->parent);
}

Type check_return_required(Scope *scope)
{
    Scope *current = scope;
    if(scope->parent)
    {
        current = scope->parent;
    } 

    return get_return_type(current);
}

Symbol *get_function(Scope *scope)
{
    Symbol *current = scope->symbols;
    while(current)
    {
        if(current->type == SYMBOL_FUNCTION)
        {
            return current;
        }
        current = current->next;
    }
    return get_function(scope->parent);
}

Symbol *get_current_function(Scope *scope)
{
    Scope *current = scope;
    if(scope->parent)
    {
        current = scope->parent;
    } 

    return get_function(current);
}

void kill_scope(Scope *scope)
{
    //dont touch the parent
    Symbol *current = scope->symbols;
    while(current)
    {
        Symbol *next = current->next;
        free(current);
        current = next;
    }
}

// Helper functions

bool is_pointer(Symbol *symbol)
{
    return symbol->return_type == TYPE_PTR_INT || symbol->return_type == TYPE_PTR_FLOAT || symbol->return_type == TYPE_PTR_DOUBLE || symbol->return_type == TYPE_PTR_CHAR;
}

bool is_pointer_type(Type type)
{
    return type == TYPE_PTR_INT || type == TYPE_PTR_FLOAT || type == TYPE_PTR_DOUBLE || type == TYPE_PTR_CHAR;
}

bool is_referenceable(Symbol *symbol)
{
    return symbol->return_type == TYPE_INT || symbol->return_type == TYPE_FLOAT || symbol->return_type == TYPE_DOUBLE || symbol->return_type == TYPE_CHAR;
}

bool is_referenceable_type(Type type)
{
    return type == TYPE_INT || type == TYPE_FLOAT || type == TYPE_DOUBLE || type == TYPE_CHAR;
}

Type type_to_pointer(Type type)
{
    switch(type)
    {
        case TYPE_INT:
            return TYPE_PTR_INT;
        case TYPE_FLOAT:
            return TYPE_PTR_FLOAT;
        case TYPE_DOUBLE:
            return TYPE_PTR_DOUBLE;
        case TYPE_CHAR:
            return TYPE_PTR_CHAR;
        default:
            yyerror("Invalid type for pointer");
    }
    // should never reach here
    return TYPE_NULL;
}

Type pointer_to_type(Type type)
{
    switch(type)
    {
        case TYPE_PTR_INT:
            return TYPE_INT;
        case TYPE_PTR_FLOAT:
            return TYPE_FLOAT;
        case TYPE_PTR_DOUBLE:
            return TYPE_DOUBLE;
        case TYPE_PTR_CHAR:
            return TYPE_CHAR;
        default:
            yyerror("Invalid pointer type");
    }
    // should never reach here
    return TYPE_NULL;
}

Type get_expression_type(node* e1, node* e2)
{
    if(e1->type != TYPE_FLOAT && e1->type != TYPE_INT && e1->type != TYPE_DOUBLE)
    {
        yyerror("Invalid type for basic math expression, must be int, float or double");
    }

    if(e2->type != TYPE_FLOAT && e2->type != TYPE_INT && e2->type != TYPE_DOUBLE)
    {
        yyerror("Invalid type for basic math expression, must be int, float or double");
    }

    if(e1->type == TYPE_FLOAT || e2->type == TYPE_FLOAT)
    {
        return TYPE_FLOAT;
    }

    if(e1->type == TYPE_DOUBLE || e2->type == TYPE_DOUBLE)
    {
        return TYPE_DOUBLE;
    }

    return TYPE_INT;
}

int size(node *n)
{
    switch ((int)n->type)
    {
        case TYPE_INT:
        case TYPE_BOOL:
        case TYPE_STRING:
        case TYPE_PTR_INT:
        case TYPE_PTR_FLOAT:
        case TYPE_PTR_DOUBLE:
        case TYPE_PTR_CHAR:
            return 4;
        case TYPE_CHAR:
            return 1;
        case TYPE_FLOAT:
        case TYPE_DOUBLE:
            return 8;
    }
    return 0;
}

void check_call_arguments(Symbol *function, node *args)
{
    if(function->args_count != args->children_count)
    {
        yyerror("Function arguments count does not match the function declaration");
    }

    for(int i = 0; i < args->children_count; i++)
    {
        node *arg = args->children[i];
        if(arg->type != function->args[i])
        {
            yyerror("Function argument type does not match the function declaration");
        }
    }
}

bool is_function_exists_in_scope(Scope *scope, Symbol *symbol)
{
    return is_function_in_scope(scope, symbol) || is_function_in_scope(scope->parent, symbol);
}

bool is_function_exists_in_scope_rec(Scope *scope, Symbol *symbol)
{
    bool exists = is_function_in_scope(scope, symbol);
    if(exists)
    {
        return true;
    }

    return is_function_exists_in_scope(scope->parent, symbol);
}

bool is_function_in_scope(Scope *scope, Symbol *symbol)
{
    if(!scope)
        return false;
    Symbol *current = scope->symbols;
    while(current)
    {
        if(current->type == SYMBOL_FUNCTION && strcmp(current->name, symbol->name) == 0)
        {
            return true;
        }
        current = current->next;
    }
    return false;
}

char* freshLabel()
{
    char* numstr=itos(count_label++);
    char* res = ConcatString("_L",numstr);
    free(numstr);
    return res;
}

char* freshVar()
{
    char* numstr=itos(count_var++);
    char* res = ConcatString("_t",numstr);
    free(numstr);
    return res;
}

int count_substring(const char *str, const char *substr) {
    int count = 0;
    const char *temp = str;
    
    // Loop through the string
    while ((temp = strstr(temp, substr)) != NULL) {
        count++;
        temp += strlen(substr);  // Move the pointer forward by the length of the substring
    }
    
    return count;
}

pretty get_token(const char *s)
{
    if(*s == '\0')
        return EMPTY;
    if(s[0] == '_' && s[1] == 'L')
        return LABEL;
    if(s[strlen(s)-2] == ':')
        return FUNC;
    return CODE;
}

void prettify(FILE *in, FILE *out)
{
    char* SPACE = "\t\t\t";
    char line[30];
    char* temp = NULL;
    pretty token;

    while(fgets(line, 30, in)){
        token = get_token(line);
        switch(token){
            case EMPTY:
                fprintf(out, "\n");
                break;
            case FUNC:
                fputs(line, out);
                break;
            case LABEL:
                temp = strtok(line, ":");
                int lblLen = strlen(temp) + 1;
                char* temp2 = line + lblLen;
                fprintf(out, "\t%s:\t%s", temp, temp2);
                break;
            case CODE:
                fprintf(out, "%s%s", SPACE, line);
                break;
        }
    }
}