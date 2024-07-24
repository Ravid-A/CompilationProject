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
    TYPE_PTR_CHAR,
    TYPE_VOID
} Type;

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
} node;

typedef struct Symbol
{
    char *name;
    SymbolType type;
    Type return_type;
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

char* ConcatString(char *s1, char *s2);

char* ctos(char c);
char* itos(int i);
char* ftos(float f);

void make_scope();
void add_function(Scope *scope, char *name, Type return_type, node *args, node* is_public, node* is_static);
void add_variable(Scope *scope, char *name, Type type);
void exit_scope();
bool check_symbol(Scope *scope, char *name);
bool check_main_exists(Scope *scope);
Type get_return_type(Scope *scope);
bool check_return_required(Scope *scope);
void kill_scope(Scope *scope);
void kill_symbol(Symbol *symbol);

int yycolumnno = 0;

// Node functions

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
        fprintf(file, " \t");
    }
}

// String functions

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
    if(check_symbol(scope, name))
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
    Symbol *new_symbol = (Symbol*)malloc(sizeof(Symbol));
    new_symbol->name = name;
    new_symbol->type = SYMBOL_VARIABLE;
    new_symbol->return_type = type;
    new_symbol->next = scope->symbols;
    scope->symbols = new_symbol;
}

void exit_scope()
{
    Scope *parent = current_scope->parent;
    kill_scope(current_scope);
    current_scope = parent;
}

bool check_symbol(Scope *scope, char *name)
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
    return check_symbol(scope->parent, name);
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

bool check_return_required(Scope *scope)
{
    Scope *current = scope;
    if(scope->parent)
    {
        current = scope->parent;
    } 

    return get_return_type(current) != TYPE_VOID;
}

void kill_scope(Scope *scope)
{
    //dont touch the parent
    Symbol *current = scope->symbols;
    while(current)
    {
        Symbol *next = current->next;
        kill_symbol(current);
        current = next;
    }
    free(scope);
}

void kill_symbol(Symbol *symbol)
{
    free(symbol->name);
    free(symbol);
}