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

fragment NonDigit: [a-zA-Z];

fragment DecInteger: NonZeroDecDigit DecDigit*;
fragment OctInteger: OctPrefix OctDigit*;
fragment HexInteger: HexPrefix HexDigit+;

fragment DecFraction:
    (DecInteger)? '.' DecDigit+
    | DecInteger '.';
fragment DecExponent: ('e' | 'E') ('+' | '-')? DecInteger;

fragment HexFraction:
    HexDigit* '.' HexDigit+
    | HexDigit+ '.';
fragment BinExponent: ('p' | 'P') ('+' | '-')? HexDigit+;

fragment DecFloat:
    DecFraction (DecExponent)?
    | DecInteger DecExponent;

fragment HexFloat:
    HexPrefix HexFraction BinExponent
    | HexPrefix HexDigit+ BinExponent;

// keywords
CONST : 'const' ;
VOID : 'void' ;
INT: 'int';
FLOAT : 'float' ;
IF : 'if' ;
ELSE : 'else' ;
WHILE : 'while' ;
BREAK : 'break' ;
CONTINUE : 'continue' ;
RETURN : 'return' ;

// operators
SUB : '-' ;
BANG : '!' ;
MUL : '*' ;
DIV : '/' ;
MOD : '%' ;
LT : '<' ;
GT : '>' ;
LE : '<=' ;
GE : '>=' ;
EQUAL_EQUAL : '==' ;
NOT_EQUAL : '!=' ;
AND : '&&' ;
OR : '||' ;
ADD: '+';

// punctuations
COMMA: ',' ;
SEMI : ';' ;
LBRACK : '[' ;
RBRACK : ']' ;
ASSIGN : '=' ;
LBRACE : '{' ;
RBRACE : '}' ;
LPAREN : '(' ;
RPAREN : ')' ;

// identifier
IDENT: NonDigit (NonDigit | DecDigit)*;

// literals
INTEGER:
    DecInteger
    | OctInteger
    | HexInteger;

FLOATING:
    DecFloat
    | HexFloat;

DECIMAL_FLOAT_CONST:
    (NonZeroDecDigit DecDigit*)? '.' DecDigit+ ('e' ('+' | '-')? DecDigit+)?;
HEXADECIMAL_FLOAT_CONST: HexPrefix HexDigit+ '.' HexDigit+ ('e' ('+' | '-')? HexDigit+)?;

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
number: INTEGER | FLOATING;
unaryExp:
    primaryExp
    | IDENT '(' funcRParams ')'
    | unaryOp unaryExp;
unaryOp: '+' | '-' | '!';
funcRParams: exp (',' exp)*;
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