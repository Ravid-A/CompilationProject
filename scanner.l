
DIGIT       [0-9]
DIGIT_HEX   [A-F]
NONZERO     [1-9]
SIGN        [\-\+]
LETTER      [a-zA-Z]
CHARACHTER  [a-zA-Z0-9]
char        \'.\'
int_dec     0|{NONZERO}{DIGIT}*
int_hex     0(x|X){DIGIT_HEX}+
int         (^{SIGN})?({int_hex}|{int_dec})
ex_notation (e|E){int_dec}
double      (^{SIGN})?({int_dec}?.{int_dec}|{int_dec}.{int_dec}?){ex_notation}?
float       ({double}|{int}.)(f|F)
string      \"[^\n"]*\"
identifier  {LETTER}({CHARACHTER}|_)*

%%
"/*"        { return COMMENT_OPEN; }
"*/"        { return COMMENT_END; }

bool        { return BOOL; }
char        { return CHAR; }
int         { return INT; }
double      { return DOUBLE; }
float       { return FLOAT; }
string      { return STRING; }
null        { return NULL_TOKEN; }
"int*"      { return PTR_INT; }
"float*"    { return PTR_FLOAT; }
"double*"   { return PTR_DOUBLE; }
"char*"     { return PTR_CHAR; }
var         { return VAR; }
if          { return IF; }
else        { return ELSE; }
while       { return WHILE; }
do          { return DO; }
for         { return FOR; }
"args>>"    { return ARGS; }
public      { return PUBLIC; }
private     { return PRIVATE; }
static      { return STATIC; }
return      { return RETURN; }
void        { return VOID; }

"&&"        { return AND; }
"=="        { return EQ; }
">"         { return GRTR; }
">="        { return GRTR_EQ; }
"<"         { return LESS; }
"<="        { return LESS_EQ; }
"!"         { return NOT; }
"!="        { return NOT_EQ; }
"||"        { return OR; }
"/"         { return DIV; }
"-"         { return SUB; }
"+"         { return ADD; }
[.]+\*      { return MUL; }
"&"[.]+     { return REF; }
\*[.]+      { return DEREF; }

true|false      { yylval.bval = strcmp(yytext, "true") == 0; return LIT_BOOL; }
{char}          { yylval.cval = yytext[1]; return LIT_CHAR; }
{int_dec}           { yylval.ival = strtol(yytext, NULL, 10); return LIT_INT; }
{int_hex}           { yylval.ival = strtol(yytext, NULL, 16); return LIT_INT; }
{double}        { yylval.rval = atof(yytext); return LIT_DOUBLE; }
{float}         { yylval.rval = atof(yytext); return LIT_FLOAT; }
{string}        { yylval.sval = strdup(yytext); return LIT_STRING; }
{identifier}    { yylval.sval = strdup(yytext); return IDENTIFIER; }

";"         { return SEMICOL; }
","         { return COMMA; }
"|"         { return STRLEN; }
"{"         { return BLOCK_OPEN; }
"}"         { return BLOCK_CLOSE; }
"("         { return PAREN_OPEN; }
")"         { return PAREN_CLOSE; }
"["         { return INDEX_OPEN; }
"]"         { return INDEX_CLOSE; }
"<-"        { return ASS; }

[ \t\s]      { /* ignore whitespace */ };
[ \n]        { yylineno++; }
.            { return yytext[0]; }
%%