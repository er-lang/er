grammar Kju;
import Lexer;

root : block+ EOF ;

block : export
      | import_
      | def ;

/// Ops | Also some tokens as ANTLR4 concatenates lexemes.

orelse : '||' | 'orelse' ;  // || && are to replace their synonyms
andalso : '&&' | 'andalso' ;

compOp : '<' | '=<' | '==' | '>=' | '>' | '/=' | '=/=' | '=:=' | '\u2264' | '\u2265' | '\u2260' ;

listOp : '++' | '--' ;

addOp : '+' | '-' | 'bsl' | 'bsr' | 'or' | 'xor' | 'bor' | 'bxor' ;

mulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' ;

prefixOp : '+' | '-' | 'not' | 'bnot' ;

when : 'when' | '|' ;  // | is just to test support. Will not be part of language.

etc : '...' | '\u2026' ;

fun_ : 'fun' ;

lra : '->' | '\u2192' ;

angll : '<' | '\u2039' ;
anglr : '>' | '\u203a' ;

generator : '<-' | '<=' | '<~' | '<:' ;

/// Tokens

atom : Atom ;
var : Var ;
float_ : Float ;
integer : Integer ;
char_ : Char ;
string : String+ ;

/// export

export : 'export' fas ;

fas : fa+ ;
fa : atom '/' integer (',' integer)* ;

/// import

import_ : 'import' fas 'from' atom
        | 'import' repo+ ;

repo : string atom ;

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
tyMax : (var '::')? type ('|' type)* ;

subtype :       atom (':' atom)? '(' tyMaxs? ')'
        | angll atom (':' atom)?     tyMax*  anglr ;

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
     | tyBinary
     | fun_ '(' tyFun? ')' ;

tyFun : '(' (etc | tyMaxs)? ')' lra tyMax ;

tyBinary : '<<'                               '>>'
         | '<<' tyBinaryBase                  '>>'
         | '<<'                  tyBinaryUnit '>>'
         | '<<' tyBinaryBase ',' tyBinaryUnit '>>' ;
tyBinaryBase : var ':'         type ;
tyBinaryUnit : var ':' var '*' type ;

/// expr | seqExprs | exprA

expr    : (expr125|lastOnly) '=' (expr|last)
        |  expr125 ;

expr125 : (expr150|last)     '!' (expr125|last)
        |  expr150 ;

expr150 : (expr160|last) orelse  (expr150|last)
        |  expr160 ;

expr160 : (expr200|last) andalso (expr160|last)
        |  expr200 ;

expr200 : (expr300|last) compOp  (expr200|last)
        |  expr300 ;

expr300 : (expr400|last) listOp  (expr300|last)
        |  expr400 ;

expr400 : (expr500|last) addOp   (expr400|last)
        |  expr500 ;

expr500 : (expr600|last) mulOp   (expr500|last)
        |  expr600 ;

expr600 :             prefixOp   (exprMax|last)
        |                         exprMax ;

exprMax : atomic
        //| recordExpr
        | list
        | binary
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
exprA : last     | expr    ;

exprM : lastOnly | exprMax ;

// Make sure to always have this order! (conflicts on :-notation)
last : functionCall | lastOnly ;

matchables : matchable (',' matchable)* ;
matchable : matchable   listOp matchable
          | matchable    addOp matchable
          | matchable    mulOp matchable
          | matchable prefixOp matchable
          | matchable      '=' matchable // lesser precedence
          | var | atom | '(' matchable ')'
          | atomic //| mapExpr | recordExpr
          | list | binary | tuple ;

/// Detailed expressions

params : '(' exprAs? ')' ;
functionCall : mf  params
             | mf_ params ;

atomic : char_
       | integer
       | float_
       | string
       ;

list : '['       ']'
     | '[' exprA tail ;
tail :           ']'
     | '|' exprA ']'
     | ',' exprA tail ;

//recordExpr : '‹'

binary : '<<' binElements? '>>' ;
binElements : binElement (',' binElement)* ;
binElement : exprA (':' exprM)? ('/' binType+)? ;
binType : atom (':' integer)? ;

tuple : '{' exprAs? '}' ;

lc :  '[' seqExprs gens ']'  ;
bc : '<<' seqExprs gens '>>' ;
tc :  '{' seqExprs gens '}'  ;

lr :  '[' exprA '..' exprA ']'  ;
br : '<<' exprA '..' exprA '>>' ;
tr :  '{' exprA '..' exprA '}'  ;

begin : 'begin' seqExprs 'end' ;

if_ : 'if' exprA seqExprs 'else' seqExprs 'end' ;

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

mf :            ':'
   |            ':'   lastOnly
   |                  lastOnly ;
mf_ : (lastOnly ':')+ lastOnly ;

gens : gen_ (gen_ | gen | exprA)* ;
gen_ : '|' gen ;
gen : matchable generator exprA ;

catchClauses : catchClause+ ;
catchClause : exprM? ':'? (clause|clauseGuard) ;

of : 'of' clauses ;
