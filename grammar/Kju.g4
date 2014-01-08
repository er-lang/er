grammar Kju;

root : block+ EOF ;

block : funDef ;

/// Blanks

Comment : '#' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;


/// Ops

compOp : '<' | '=<' | '==' | '=>' | '>' | '/=' | '=/=' | '=:=' ;

listOp : '++' | '--' ;

addOp : '+' | '-' | 'bsl' | 'bsr'
      | 'or' | 'xor' | 'bor' | 'bxor' ;

mulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' ;

prefixOp : '+' | '-' | 'not' | 'bnot' ;

end : 'end' | '.' ;

when : 'when' | '|' ;

/// Tokens

Atom : [a-z]~[ \t\r\n()\[\]{}:.]*
     | '\'' ( '\\' (~'\\'|'\\') | ~[\\''] )* '\'' ;
    // Add A-Z to the negative match to forbid camelCase

// When using negative match, be sure to also negative match
//   previously-defined rules.

Var : [A-Z_][0-9a-zA-Z_]* ;

Float : '-'? [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

Integer : '-'? [0-9]+ ('#' [0-9a-zA-Z]+)? ;

Char : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

String : '"' ( '\\' (~'\\'|'\\') | ~[\\""] )* '"' ;

atomic : Char
       | Integer
       | Float
       | Atom
       | String+
       ;

/// funDef

funDef : Atom args guard? '=' seqExprs end ;

args : '(' exprM? ')' ;

guard : when exprs (';' exprs)* ;

/// expr | seqExprs | exprAll

exprs : expr  (',' expr)* ;
expr    : (expr150|allowedLast) ('='|'!') (expr150|allowedLast)
        |  expr150 ;

expr150 : (expr160|allowedLast) 'orelse'  (expr150|allowedLast)
        |  expr160 ;

expr160 : (expr200|allowedLast) 'andalso' (expr160|allowedLast)
        |  expr200 ;

expr200 : (expr300|allowedLast) compOp    (expr200|allowedLast)
        |  expr300 ;

expr300 : (expr400|allowedLast) listOp    (expr300|allowedLast)
        |  expr400 ;

expr400 : (expr500|allowedLast) addOp     (expr400|allowedLast)
        |  expr500 ;

expr500 : (expr600|allowedLast) mulOp     (expr500|allowedLast)
        |  expr600 ;

expr600 :                       prefixOp  (expr700|allowedLast)
        |                                  expr700 ;

expr700 : functionCall
        //| recordExpr
        | exprMax
        ;

exprMax : atomic
        | list
        //| binary
        | tuple
        //| lc
        //| bc
        //| tc
        //| begin
        //| if
        | case_
        //| receive
        //| fun
        //| try
        ;

allowedLasts : allowedLast (',' allowedLasts)* ;
allowedLast : Var
            | '(' (expr|allowedLast) ')'
            ;

seqExprs : expr+ allowedLast?
         | expr* allowedLast ;
// f () = B = A (B). #=> ok
// f () = (B) B = A. #=> line 1:11 mismatched input 'B' expecting {'.', 'end'}

exprAlls : exprAll  (',' exprAll)* ;
exprAll : expr | allowedLast ;

exprMs : exprM  (',' exprM)* ;
exprM : exprMax | allowedLast ;

/// Detailed expressions

functionCall : exprM ':' exprM args
             |           exprM args
             |       ':' exprM args
             |       ':'       args ;

//recordExpr : '‹'

list : '['           ']'
     | '[' exprAll tail ;
tail :               ']'
     | '|' exprAll   ']'
     | ',' exprAll tail ;

// binary : '<<'

tuple : '{' exprAlls? '}' ;

case_ : 'case' expr 'of' clauses end ;

/// Utils

clauses : clause+ ;
clause : exprM guard? body ;

body : '->' seqExprs ;
