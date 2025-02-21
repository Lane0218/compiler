grammar SysY;

/*===-------------------------------------------===*/
/* Lexer rules                                     */
/*===-------------------------------------------===*/

// fragments
fragment DecDigit: [0-9];
fragment OctDigit: [0-7];
fragment HexDigit: [0-9a-fA-F];
fragment OctPrefix: '0';
fragment HexPrefix: '0' [xX];
fragment NonZeroDecDigit: [1-9];
fragment ESC: '\\"' | '\\\\';

fragment NonDigit: '_' | [a-zA-Z];

fragment DecInteger: NonZeroDecDigit DecDigit*;
fragment OctInteger: OctPrefix OctDigit*;
fragment HexInteger: HexPrefix HexDigit+;

fragment DecFraction: (DecInteger)? '.' DecInteger | DecInteger '.';
fragment DecExponent: ('e'|'E') ('+'|'-')? DecInteger;
fragment DecFloat: DecFraction (DecExponent)? | DecInteger DecExponent;

fragment HexFraction: HexDigit* '.' HexDigit+ | HexDigit+ '.';
fragment BinExponent: ('p'|'P') ('+'|'-')? HexDigit+;
fragment HexFloat: HexPrefix HexFraction BinExponent | HexInteger BinExponent;

// keywords
INT: 'int';
FLOAT: 'float';
CONST: 'const';
VOID: 'void';
IF: 'if';
ELSE: 'else';
WHILE: 'while';
BREAK: 'break';
CONTINUE: 'continue';
RETURN: 'return';

// operators
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';
MOD: '%';
EQ: '==';
GT: '>';
GE: '>=';
LT: '<';
LE: '<=';
NE: '!=';
NOT: '!';
AND: '&&';
OR: '||';

// punctuations
COMMA: ',';
SEMI: ';';
LBRACK: '[';
RBRACK: ']';
ASSIGN: '=';
LBRACE: '{';
RBRACE: '}';
LPAREN: '(';
RPAREN: ')';

// identifier
IDENT: NonDigit (DecDigit | NonDigit)*;


// literals
ILITERAL: DecInteger|OctInteger|HexInteger;
FLITERAL: DecFloat| HexFloat;
//INTEGER: DecInteger|OctInteger|HexInteger;
//FLOATING: DecFloat| HexFloat;


// string
STRING: '"' (ESC | .)*? '"';

// white space and comments
WS: [ \t\r\n] -> skip;
LINECOMMENT: '//' .*? '\r'? '\n' -> skip;
BLOCKCOMMENT: '/*' .*? '*/' -> skip;

/*===-------------------------------------------===*/
/* Syntax rules                                    */
/*===-------------------------------------------===*/

module: compUnit EOF;
compUnit: (decl | funcDef)+;
decl: constDecl | varDecl;

constDecl: 'const' bType constDef (',' constDef)* ';';
bType: 'int' | 'float';
constDef: IDENT ('[' constExp ']')* '=' constInitVal;
constInitVal:
    constExp
    | '{' (constInitVal (',' constInitVal)*)? '}';

varDecl: bType varDef (',' varDef)* ';';
varDef: IDENT ('[' constExp ']')* ('=' initVal)?;
initVal:
    exp
    | '{' (initVal (',' initVal)*)? '}';

funcDef: funcType IDENT '(' funcFParams* ')' block;
funcType: 'void' | 'int' | 'float';
funcFParams: funcFParam (',' funcFParam)*;
funcFParam: bType IDENT ('[' ']' ('[' exp ']')*)?;

block: '{' blockItem* '}';
blockItem: decl | stmt;
stmt:
    lVal '=' exp ';'
    | (exp)? ';'
    | block
    | 'if' '(' cond ')' stmt ('else' stmt)?
    | 'while' '(' cond ')' stmt
    | 'break' ';'
    | 'continue' ';'
    | 'return' (exp)? ';';

exp: addExp;
cond: lOrExp;
lVal: IDENT ('[' exp ']')*;
primaryExp:
    '(' exp ')'
    | lVal
    | number;
number: ILITERAL | FLITERAL;
//number: INTEGER | FLOATING;
unaryExp:
    primaryExp
    | IDENT '(' funcRParams ')'
    | unaryOp unaryExp;
unaryOp: '+' | '-' | '!';
funcRParams: funcRParam (',' funcRParam)*;
funcRParam: exp;
mulExp:
    unaryExp
    | mulExp ('*' | '/' | '%') unaryExp;
addExp:
    mulExp
    | addExp ('+' | '-') mulExp;
relExp:
    addExp
    | relExp ('<' | '>' | '<=' | '>=') addExp;
eqExp:
    relExp
    | eqExp ('==' | '!=') relExp;
lAndExp:
    eqExp
    | lAndExp '&&' eqExp;
lOrExp:
    lAndExp
    | lOrExp '||' lAndExp;
constExp: addExp;

string: STRING;