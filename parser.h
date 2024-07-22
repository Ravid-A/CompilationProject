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

typedef struct stack {
    bool *data;
    int size;
    int capacity;
} stack;

void push(stack stack, bool value);
bool pop(stack stack);

typedef struct Symbol
{
    char *name;
    SymbolType type;
    Type return_type;
    struct Symbol *next;
} Symbol;

typedef struct Scope
{
    Symbol *symbols;
    struct Scope *parent;
} Scope;

Scope *current_scope;

void make_scope()
{
    Scope *new_scope = (Scope*)malloc(sizeof(Scope));
    new_scope->symbols = NULL;
    new_scope->parent = current_scope;
    current_scope = new_scope;
}

void add_symbol(Scope *scope, char *name, SymbolType type, Type return_type)
{
    Symbol *new_symbol = (Symbol*)malloc(sizeof(Symbol));
    new_symbol->name = name;
    new_symbol->type = type;
    new_symbol->return_type = return_type;
    new_symbol->next = scope->symbols;
    scope->symbols = new_symbol;
}

void exit_scope()
{
    Scope *parent = current_scope->parent;
    free(current_scope);
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