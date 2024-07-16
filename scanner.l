DIGIT [0-9]
DIGIT_HEX [0-9a-fA-F]
NONZERO [1-9]
SIGN [\-\+]
LETTER [a-zA-Z]
CHARACHTER [a-zA-Z0-9]
char \'.\'
int_dec 0|{NONZERO}{DIGIT}*
int_hex 0[xX]{DIGIT_HEX}+
int (^{SIGN})?({int_hex}|{int_dec})
ex_notation (e|E){int_dec}
real (^{SIGN})?({int_dec}\.{DIGIT}+|{int_dec}?\.{DIGIT}+){ex_notation}?|{int_dec}{ex_notation}
double {real}
float {real}f
string \"[^\n"]*\"
identifier {LETTER}({CHARACHTER}|_)*
newline \n|\r\n|\r|\r

%%

bool { yycolumnno += yyleng; return BOOL; }
char { yycolumnno += yyleng; return CHAR; }
int { yycolumnno += yyleng; return INT; }
double { yycolumnno += yyleng; return DOUBLE; }
float { yycolumnno += yyleng; return FLOAT; }
string { yycolumnno += yyleng; return STRING; }
null { yycolumnno += yyleng; return NULL_TOKEN; }
"int*" { yycolumnno += yyleng; return PTR_INT; }
"float*" { yycolumnno += yyleng; return PTR_FLOAT; }
"double*" { yycolumnno += yyleng; return PTR_DOUBLE; }
"char*" { yycolumnno += yyleng; return PTR_CHAR; }
var { yycolumnno += yyleng; return VAR; }
if { yycolumnno += yyleng; return IF; }
else { yycolumnno += yyleng; return ELSE; }
while { yycolumnno += yyleng; return WHILE; }
do { yycolumnno += yyleng; return DO; }
for { yycolumnno += yyleng; return FOR; }
public { yycolumnno += yyleng; return PUBLIC; }
private { yycolumnno += yyleng; return PRIVATE; }
static { yycolumnno += yyleng; return STATIC; }
"args>>" { yycolumnno += yyleng; return ARGS; }
return { yycolumnno += yyleng; return RETURN; }
void { yycolumnno += yyleng; return VOID; }
"&&" { yycolumnno += yyleng; return AND; }
"==" { yycolumnno += yyleng; return EQ; }
">" { yycolumnno += yyleng; return GRTR; }
">=" { yycolumnno += yyleng; return GRTR_EQ; }
"<" { yycolumnno += yyleng; return LESS; }
"<=" { yycolumnno += yyleng; return LESS_EQ; }
"!" { yycolumnno += yyleng; return NOT; }
"!=" { yycolumnno += yyleng; return NOT_EQ; }
"||" { yycolumnno += yyleng; return OR; }
"&"[.]+ { yycolumnno += yyleng; return REF; }
\*[.]+ { yycolumnno += yyleng; return DEREF; }
"/" { yycolumnno += yyleng; return DIV; }
"-" { yycolumnno += yyleng; return SUB; }
"+" { yycolumnno += yyleng; return ADD; }
\* { yycolumnno += yyleng; return MUL; }
"%" { yycolumnno += yyleng; return MOD; }
: { yycolumnno += yyleng; return COLON; }
true|false { yycolumnno += yyleng; yylval.bval = strcmp(yytext, "true") == 0; return LIT_BOOL; }
{char} { yycolumnno += yyleng; yylval.cval = yytext[1]; return LIT_CHAR; }
{int} { yycolumnno += yyleng; yylval.ival = atoi(yytext); return LIT_INT; }
{double} { yycolumnno += yyleng; yylval.rval = atof(yytext); return LIT_DOUBLE; }
{float} { yycolumnno += yyleng; yylval.rval = atof(yytext); return LIT_FLOAT; }
{string} { yycolumnno += yyleng; yylval.sval = strdup(yytext); return LIT_STRING; }
{identifier} { yycolumnno += yyleng; yylval.sval = strdup(yytext); return IDENTIFIER; }
";" { yycolumnno += yyleng; return SEMICOL; }
"," { yycolumnno += yyleng; return COMMA; }
"|" { yycolumnno += yyleng; return STRLEN; }
"{" { yycolumnno += yyleng; return BLOCK_OPEN; }
"}" { yycolumnno += yyleng; return BLOCK_CLOSE; }
"(" { yycolumnno += yyleng; return PAREN_OPEN; }
")" { yycolumnno += yyleng; return PAREN_CLOSE; }
"[" { yycolumnno += yyleng; return INDEX_OPEN; }
"]" { yycolumnno += yyleng; return INDEX_CLOSE; }
"<-" { yycolumnno += yyleng; return ASS; }
\/\*(.|{newline})*\*\/ { yycolumnno += yyleng; /* ignore comments */ }
[ \t\s] { yycolumnno += yyleng; /* ignore whitespace */ }
{newline} { yycolumnno = 0; yylineno++; }
. { yycolumnno += yyleng; yyerror("INVALID TOKEN"); }

%%