grammar Kju;

root : block+ EOF ;

block : export
      | def ;

/// Blanks

Comment : '#' ~[\r\n]* '\r'? '\n' -> channel(HIDDEN) ;

WS : [ \t\r\n]+ -> channel(HIDDEN) ;

/// Ops | Also some tokens b/c ANTLR4 bugs and concatenates lexems.

orelse : '||' | 'orelse' ;  // || && will are to replace their synonyms
andalso : '&&' | 'andalso' ;

compOp : '<' | '=<' | '==' | '>=' | '>' | '/=' | '=/=' | '=:=' ;

listOp : '++' | '--' ;

addOp : '+' | '-' | 'bsl' | 'bsr'
      | 'or' | 'xor' | 'bor' | 'bxor' ;

mulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' ;

prefixOp : '+' | '-' | 'not' | 'bnot' ;

when : 'when' | '|' ;  // | is just to test support. Will not be part of language.

etc : '...' ;//| '…' ;

fun_ : 'fun' ;

lra : '->' ;//| '→' ;

angll : '<' ;//| '‹' ;
anglr : '>' ;//| '›' ;

generator : '<-' | '<=' | '<~' | '<:' ;

/// Tokens

atom : Atom ;
Atom : [a-z] ~[ \t\r\n()\[\]{}:;,/>]* //[_a-zA-Z0-9]*
     | '\'' ( '\\' (~'\\'|'\\') | ~[\\''] )* '\'' ;
    // Add A-Z to the negative match to forbid camelCase
    // Add '›' and other unicode? rhs.

var : Var ;
Var : [A-Z_] ~[ \t\r\n()\[\]{}:;,>=|*/+-]* ; //[_a-zA-Z0-9]*

// When using negative match, be sure to also negative match
//   previously-defined rules.

float_ : Float ;
Float : [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

integer : Integer ;
Integer : [0-9]+ ('#' [0-9a-zA-Z]+)? ;

char_ : Char ;
Char : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

string : String ;
String : '"' ( '\\' (~'\\'|'\\') | ~[\\""] )* '"' ;

/// export

export : 'export' fa* 'end' ;

fa : atom '/' integer (',' integer)* ;

/// def

def : spec?     func
    | spec? fun_func ;

func : atom args guard? ('='|lra) seqExprs ; // Both usable as 'f()' as lhs makes sense only then.

fun_func : fa           ('='|lra) seqExprs ;

args : '(' matchables? ')' ;

guard : when exprA ;

/// spec

spec : atom '::' tyFun
     | atom '::' tyFun when tyGuards ;

tyGuards : tyGuard+ ;
tyGuard : subtype
        | var '::' tyMax ;

tyMaxs : tyMax (',' tyMax)* ;
tyMax : var '::' tyMaxAlt
      |          tyMaxAlt ;

tyMaxAlt : type '|' tyMaxAlt
         | type ;

subtype :       atom (':' atom)? '(' tyMaxs?  ')'
        | angll atom (':' atom)?    (tyMax+)? anglr ;

type : type '..'     type
     | type addOp    type
     | type mulOp    type
     |      prefixOp type
     | '(' tyMax ')'
     | var | atom | integer
     | subtype
     | '['               ']'
     | '[' tyMax         ']'
     | '[' tyMax ',' etc ']'
     //| tyRecord
     //| tyMap
     | '{' tyMaxs? '}'
     //| binaryType
     | fun_ '(' tyFun? ')' ;

tyFun : '(' (etc | tyMaxs)? ')' lra tyMax ;

//binaryType

/// expr | seqExprs | exprA

expr    : (expr125|lastOnly)              '='     (expr   |lastOnly|functionCall)
        |  expr125 ;

expr125 : (expr150|lastOnly|functionCall) '!'     (expr125|lastOnly|functionCall)
        |  expr150 ;

expr150 : (expr160|lastOnly|functionCall) orelse  (expr150|lastOnly|functionCall)
        |  expr160 ;

expr160 : (expr200|lastOnly|functionCall) andalso (expr160|lastOnly|functionCall)
        |  expr200 ;

expr200 : (expr300|lastOnly|functionCall) compOp  (expr200|lastOnly|functionCall)
        |  expr300 ;

expr300 : (expr400|lastOnly|functionCall) listOp  (expr300|lastOnly|functionCall)
        |  expr400 ;

expr400 : (expr500|lastOnly|functionCall) addOp   (expr400|lastOnly|functionCall)
        |  expr500 ;

expr500 : (expr600|lastOnly|functionCall) mulOp   (expr500|lastOnly|functionCall)
        |  expr600 ;

expr600 :                              prefixOp   (exprMax|lastOnly|functionCall)
        |                                         exprMax ;

exprMax : atomic
        //| recordExpr
        | list
        //| binary
        | tuple
        | lr | br | tr // range
        | lc | bc | tc // comprehension
        | begin
        | if_
        | case_
        | receive
        | fun
        | try_
        ;

lastOnly : var
         | atom
         | '(' exprA ')' ;

seqExprs : (functionCall|expr)+ lastOnly?
         |                      lastOnly ;

exprAs : exprA (',' exprA)* ;
exprA : lastOnly | functionCall | expr ;

exprMs : exprM (',' exprM)* ;
exprM : lastOnly | exprMax ;

matchables : matchable (',' matchable)* ;
matchable : var
          | atom
          | atomic
          //| recordExpr
          | list
          //| binary
          | tuple
          | '(' matchable ')'
          | matchable '=' matchable
          | (var|list) listOp (var|list) // and recordExpr
          | prefixOp (char_|integer|float_) ;

/// Detailed expressions

params : '(' exprAs? ')' ;
functionCall : mf  params
             | mf_ params ;

atomic : char_
       | integer
       | float_
       | string+
       ;

list : '['       ']'
     | '[' exprA tail ;
tail :           ']'
     | '|' exprA ']'
     | ',' exprA tail ;

//recordExpr : '‹'

// binary : '<<'

tuple : '{' exprAs? '}' ;

lc :  '[' seqExprs gens ']'  ;
bc : '<<' seqExprs gens '>>' ;
tc :  '{' seqExprs gens '}'  ;

lr :  '[' exprA '..' exprA ']'  ;
br : '<<' exprA '..' exprA '>>' ;
tr :  '{' exprA '..' exprA '}'  ;

begin : 'begin' seqExprs 'end' ;

if_ : 'if' exprA 'then' seqExprs 'else' seqExprs 'end' ;

case_ : 'case' exprA of 'end' ;

receive : 'receive' clauses                'end'
        | 'receive'         'after' clause 'end'
        | 'receive' clauses 'after' clause 'end' ;

fun : fun_ mf       '/' integer
    | fun_ mf_      '/' (var|integer)
    | fun_ mf  args '/' integer
    | fun_ mf_ args '/' (var|integer)
    | fun_ funClause+ 'end'
    | fun '.' fun ;

try_ : 'try' seqExprs of? 'catch' catchClauses                  'end'
     | 'try' seqExprs of? 'catch' catchClauses 'after' seqExprs 'end'
     | 'try' seqExprs of?                      'after' seqExprs 'end' ;

/// Utils | Exists only for compactness

clauses : (clause | clauseGuard)+ ;
clause :      matchable       lra seqExprs ;
clauseGuard : matchable guard lra seqExprs ;

funClause : args       guard? lra seqExprs ;

mf :           ':'
   |           ':' lastOnly
   |               lastOnly ;
mf_ : lastOnly ':' lastOnly ;

gens : gen_ (gen_ | gen | exprA)* ;
gen_ : '|' gen ;
gen : matchable generator exprA ;

catchClauses : catchClause+ ;
catchClause : exprM? ':'? (clause|clauseGuard) ;

of : 'of' clauses ;
