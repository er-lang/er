// EBNF LL(*) grammar. Lexer split due to size.

grammar Kju;
import Lexer;

root : block* EOF ;

block : defty
      | def
      | attribute ;

/// Ops | Also some tokens as ANTLR4 concatenates lexemes.

dim : '::' | '\u2237' ; // ∷

orelse :  '||' | 'orelse'  ;  // || && are to replace their synonyms
andalso : '&&' | 'andalso' ;

compOp : '<' | '=<' | '==' | '>=' | '>' | '/=' | '=/=' | '=:='
       | '\u2264'|'\u2265' | '\u2260'|'\u2248' | '\u2261'|'\u2262' ; // ≤ ≥ ≠ ≈ ≡ ≢

listOp : '++' | '--' ;

addOp : '+' | '-' | 'bsl' | 'bsr' | 'or' | 'xor' | 'bor' | 'bxor' | '\u22c1' ; // ⋁

mulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' | '\u22c0' ; // ⋀

unOp : '+' | '-' | 'not' | 'bnot' | '\u00ac' ; // ¬

when : 'when' | '|' ;  //Just to test support of |. Will not be part of language.

etc : '...' | '\u2026' ; // …

fun_ : 'fun' ;
composeOp : '.' ;
pipeOp : '|>' ;

lra : '->' | '\u2192' ; // →
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

/// attribute

attribute : atom (term|fas) 'of' (term|tyRecordFields)
          | atom (term|fas) ;

fas: (fa (',' integer)*)+ ;
fa : atom '/' integer ;

tyRecordFields: '{' (tyRecordField (',' tyRecordField)*)? '}' ;
tyRecordField : atom ('=' expr)? (dim type ('|' type)*)? ;

/// def

def : spec?     func
    | spec? fun_func ;

func : atom args guard? lra seqExprs ;

fun_func : fa           lra seqExprs ;

args : '(' matchables? ')' ;

guard : when expr ; // && || replaces Erlang's ,; (in guards)

/// defty

defty : atom '(' tyMaxs? ')' dim tyMax (when tyGuards)? ;

/// spec

spec : atom dim  tyFun          (when tyGuards)?
     | fa   dim (tyFun|subtype) (when tyGuards)? ;

tyGuards: tyGuard+ ;
tyGuard : subtype
        | (var dim)+ tyMax ;

tyMaxs: tyMax (',' tyMax)* ;
tyMax : (var dim)? type ('|' type)* ;

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
tyMapAssocs: tyMapAssoc (',' tyMapAssoc)* ;
tyMapAssoc : tyMax '=>' tyMax ;

tyBinary : bil                               bir
         | bil tyBinaryBase                  bir
         | bil                  tyBinaryUnit bir
         | bil tyBinaryBase ',' tyBinaryUnit bir ;
tyBinaryBase : var ':'         type ;
tyBinaryUnit : var ':' var '*' type ;

/// expr | seqExprs | exprMax

exprs: expr (',' expr)* ;
expr : functionCall
     | expr      '!' expr
     | expr    mulOp expr
     | expr    addOp expr
     | expr   listOp expr
     | expr   compOp expr
     | expr  andalso expr
     | expr   orelse expr
     |          unOp expr
     | matchable '=' expr
     |      lr | br | tr // Ranges
     | mc | lc | bc | tc // Comprehensions
     | begin
     | if_
     | cond
     | case_
     | receive
     | fun
     | try_
     | expr   (pipeOp expr)+
     | expr composeOp expr
     | '`'  expr
     | '<|' expr '|>'
     | exprMax ;

exprMax : var | '(' expr ')'
        | term | record ;

seqExprs : expr+ ;

matchables: matchable (',' matchable)* ;
matchable : matchable  mulOp matchable
          | matchable  addOp matchable
          | matchable listOp matchable
          |             unOp matchable
          | matchable    '=' matchable // Lesser precedence
          | '`'  matchable
          | '<|' matchable '|>'
          | var | '(' matchable ')'
          | term | record ;

/// Detailed expressions

params : '(' exprs? ')' ;
functionCall : mf  params
             | mf_ params ;

term : char_
     | integer
     | float_
     | string
     | atom
     | map
     | list
     | binary
     | tuple ;

list : '['      ']'
     | '[' expr tail ;
tail :          ']'
     | '|' expr ']'
     | ',' expr tail ;

// Key-Value Stores
record : recEmpty | recCreateMatch | recRead | '#{' expr atom  recAssocs '}' ;
map    : mapEmpty | mapCreateMatch | mapRead | '#{' expr       mapAssocs '}' ;
recEmpty : '#{' atom '}' ;
mapEmpty : '#{'      '}' ;
recCreateMatch : '#{' atom  recAssocs '}' ;
mapCreateMatch : '#{'       mapAssocs '}' ;
recRead : '#{' expr atom  atom                   '}' ;
mapRead : '#{' expr       (functionCall|exprMax) '}' ;
//^: (…) instead of expr disambiguates wrt recCreateMatch.
recAssocs: recAssoc (',' recAssoc)* ;
recAssoc : (atom|var)    '=' expr ;
mapAssocs: mapAssoc (',' mapAssoc)* ;
mapAssoc : expr  (':='|'=>') expr ;

binary : bil binElements? bir ;
binElements: binElement (',' binElement)* ;
binElement : expr (':' exprMax)? ('/' binType ('-' binType)*)? ;
binType : atom (':' integer)? ;

tuple : '{' exprs? '}' ;

lc :  '[' seqExprs       gens ']' ;
bc :  bil seqExprs       gens bir ;
mc : '#{' expr '=>' expr gens '}' ; //seqExprs? FIXME
tc :  '{' seqExprs       gens '}' ;

lr :  '[' expr '..' expr ']' ;
br :  bil expr '..' expr bir ;
tr :  '{' expr '..' expr '}' ;

begin : 'begin' seqExprs 'end' ;

if_ : 'if' expr expr 'if' 'not' expr ;

cond : 'cond' (condClause)+ 'end' ;
condClause : expr lra seqExprs ;

case_ : 'case' expr of 'end' ;

receive : 'receive' clauses                'end'
        | 'receive'         'after' clause 'end'
        | 'receive' clauses 'after' clause 'end' ;

fun : fun_ mf       '/' integer
    | fun_ mf_      '/' (var|integer)
    | fun_ mf  args '/' integer
    | fun_ mf_ args '/' (var|integer)
    | fun_ funClause+ 'end' ;

try_ : 'try' seqExprs of? 'catch' catchClauses                  'end'
     | 'try' seqExprs of? 'catch' catchClauses 'after' seqExprs 'end'
     | 'try' seqExprs of?                      'after' seqExprs 'end' ;

/// Utils | Exists mainly for compactness

of : 'of' clauses ;

clauses : (clause | clauseGuard)+ ;
clause :      matchable       lra seqExprs ;
clauseGuard : matchable guard lra seqExprs ;

funClause : args       guard? lra seqExprs ;

catchClauses: catchClause+ ;
catchClause : (exprMax ':')? (clause|clauseGuard) ;

mf :           ':'
   |                 exprMax ;
mf_ : (exprMax ':')+ exprMax ;

gen_ : '|' gen ;
gens: gen_ (gen_ | gen | expr)* ;
gen : matchable ':=' matchable '<-'      expr
    | matchable                generator expr ;
