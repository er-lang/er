// EBNF LL(*) grammar. Lexer split due to size.

grammar Kju;
import Lexer;

root : block* EOF ;

block : export
      | import_
      | defrecord
      | defty
      | def ;

/// Ops | Also some tokens as ANTLR4 concatenates lexemes.

orelse :  '||' | 'orelse'  ;  // || && are to replace their synonyms
andalso : '&&' | 'andalso' ;

compOp : '<' | '=<' | '==' | '>=' | '>' | '/=' | '=/=' | '=:='
       | '\u2264'|'\u2265' | '\u2260'|'\u2248' | '\u2261'|'\u2262' ; // ≤ ≥ ≠ ≈ ≡ ≢

listOp : '++' | '--' ;

addOp : '+' | '-' | 'bsl' | 'bsr' | 'or' | 'xor' | 'bor' | 'bxor' | '\u22c1' ; // ⋁

mulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' | '\u22c0' ; // ⋀

unOp : '+' | '-' | 'not' | 'bnot' | '\u00ac' ; // ¬

when : 'when' | '|' ;  // | is just to test support. Will not be part of language.

etc : '...' | '\u2026' ; // …

fun_ : 'fun' ;

lra :  '->' | '\u2192' ; // →
bil : '<<' | '\u00ab' ; // «
bir : '>>' | '\u00bb' ; // »

generator : '<-' | '<=' | '<~' | '\u2190' | '\u21d0' | '\u219c' ; // ← ⇐ ↜

/// Tokens

atom : Atom ;
var : Var ;
float_ : Float ;
integer : Integer ;
char_ : Char ;
string : String | BString ;

/// export

export : 'export' fas ;

fas : (fa (',' integer)*)+ ;
fa : atom '/' integer ;

/// import

import_ : 'import' fas 'from' atom
        | 'import' repo+ ;

repo : string atom ;

/// record

defrecord : atom 'of' tyRecordFields ;

tyRecordFields : '{' (tyRecordField (',' tyRecordField)*)? '}' ;
tyRecordField : atom ('=' exprA)? ('::' type ('|' type)*)? ;

/// def

def : spec?     func
    | spec? fun_func ;

func : atom args guard? ('='|lra) seqExprs ; // Both usable as 'f()' as lhs makes sense only then.

fun_func : fa           ('='|lra) seqExprs ;

args : '(' matchables? ')' ;

guard : when exprA ; // && || replaces Erlang's ,;

/// defty

defty : atom '(' tyMaxs? ')' '::' tyMax (when tyGuards)? ;

/// spec

spec : atom '::'  tyFun          (when tyGuards)?
     | fa   '::' (tyFun|subtype) (when tyGuards)? ;

tyGuards : tyGuard+ ;
tyGuard : subtype
        | (var '::')+ tyMax ;

tyMaxs : tyMax (',' tyMax)* ;
tyMax : (var '::')? type ('|' type)* ;

subtype : atom (':' atom)* '(' tyMaxs? ')' ;

type : type '..'  type
     | type addOp type
     | type mulOp type
     |       unOp type
     | '(' tyMax ')'
     | var | atom | integer
     | subtype
     | '['               ']'
     | '[' tyMax         ']'
     | '[' tyMax ',' etc ']'
     | tyRecord
     | tyMap
     | '{' tyMaxs? '}'
     | tyBinary
     | fun_ '(' tyFun? ')' ;

tyFun : '(' (etc|tyMaxs)? ')' lra tyMax ;

tyRecord : '#{' atom '}' ;

tyMap : '#{' tyMapAssocs? '}' ;
tyMapAssocs : tyMapAssoc (',' tyMapAssoc)* ;
tyMapAssoc : tyMax '=>' tyMax ;

tyBinary : bil                               bir
         | bil tyBinaryBase                  bir
         | bil                  tyBinaryUnit bir
         | bil tyBinaryBase ',' tyBinaryUnit bir ;
tyBinaryBase : var ':'         type ;
tyBinaryUnit : var ':' var '*' type ;

/// expr | seqExprs | exprA

expr    : (expr125|lastOnly) '=' (expr|last)
        |  expr125 ;

expr125 : (expr150|last)     '!' (expr125|last)
        |  expr150 ;

expr150 : (expr160|last)  orelse (expr150|last)
        |  expr160 ;

expr160 : (expr200|last) andalso (expr160|last)
        |  expr200 ;

expr200 : (expr300|last)  compOp (expr200|last)
        |  expr300 ;

expr300 : (expr400|last)  listOp (expr300|last)
        |  expr400 ;

expr400 : (expr500|last)   addOp (expr400|last)
        |  expr500 ;

expr500 : (expr600|last)   mulOp (expr500|last)
        |  expr600 ;

expr600 :                   unOp (exprMax|last)
        |                         exprMax ;

exprMax : record | term
        |      lr | br | tr // ranges
        | mc | lc | bc | tc // comprehensions
        | begin
        | if_
        | case_
        | receive
        | fun
        | try_ ;

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
          |               unOp matchable
          | matchable      '=' matchable // lesser precedence
          | '(' matchable ')'
          | var | atom
          | record | term ;

/// Detailed expressions

params : '(' exprAs? ')' ;
functionCall : mf  params
             | mf_ params ;

term : char_
     | integer
     | float_
     | string
  // | atom can't fit here, but it's a term.
     | map
     | list
     | binary
     | tuple ;

list : '['       ']'
     | '[' exprA tail ;
tail :           ']'
     | '|' exprA ']'
     | ',' exprA tail ;

// Key-Value Stores
S : '>' ;
record : '#{' atom '}'   | '#{' exprA atom S  atom  '}'
       | '#{'  exprA? atom S    recAssocs '}' ;
map :    '#{'      '}'   | '#{' exprA      S  exprM '}'
    |    '#{' (exprA       S)?  mapAssocs '}' ;
recAssocs : recAssoc (',' recAssoc)* ;
mapAssocs : mapAssoc (',' mapAssoc)* ;
recAssoc : (atom|var)    '=' exprA ;
mapAssoc : exprA (':='|'=>') exprA ;

binary : bil binElements? bir ;
binElements : binElement (',' binElement)* ;
binElement : exprA (':' exprM)? ('/' binType+)? ;
binType : atom (':' integer)? ;

tuple : '{' exprAs? '}' ;

lc :  '[' seqExprs         gens ']' ;
bc :  bil seqExprs         gens bir ;
mc : '#{' exprA '=>' exprA gens '}' ; //seqExprs ?
tc :  '{' seqExprs         gens '}' ;

lr :  '[' exprA '..' exprA ']' ;
br :  bil exprA '..' exprA bir ;
tr :  '{' exprA '..' exprA '}' ;

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
   |                  lastOnly ;
mf_ : (lastOnly ':')+ lastOnly ;

gens : gen_ (gen_ | gen | exprA)* ;
gen_ : '|' gen ;
gen : matchable ':=' matchable '<-'      exprA
    | matchable                generator exprA ;

catchClauses : catchClause+ ;
catchClause : exprM? ':'? (clause|clauseGuard) ;

of : 'of' clauses ;
