
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
real      (^{SIGN})?({int_dec}?.{int_dec}|{int_dec}.{int_dec}?){ex_notation}?
string      \"[^\n"]*\"
identifier  {LETTER}({CHARACHTER}|_)*

%%
"/*"        { return COMMENT_OPEN; }
"*/"        { return COMMENT_END; }

bool        { return BOOL; }
char        { return CHAR; }
int         { return INT; }
real        { return REAL; }
string      { return STRING; }
null        { return NULL_TOKEN; }
"int*"      { return PTR_INT; }
"real*"     { return PTR_REAL; }
"char*"     { return PTR_CHAR; }
var         { return VAR; }
if          { return IF; }
else        { return ELSE; }
while       { return WHILE; }
do          { return DO; }
for         { return FOR; }
function    { return FUNCTION; }
"arg->"     { return ARGS; }
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
"%"         { return MOD; }  
"&"[.]+     { return REF; }
\*[.]+      { return DEREF; }

true|false      { yylval.bval = strcmp(yytext, "true") == 0; return LIT_BOOL; }
{char}          { yylval.cval = yytext[1]; return LIT_CHAR; }
{int_dec}       { yylval.ival = strtol(yytext, NULL, 10); return LIT_INT; }
{int_hex}       { yylval.ival = strtol(yytext, NULL, 16); return LIT_INT; }
{real}          { yylval.rval = atof(yytext); return LIT_REAL; }
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