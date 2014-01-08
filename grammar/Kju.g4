grammar Kju;

root : block+ EOF ;

block : funDef ;

/// Blanks

Comment : '#' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;


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

End : 'end' | '.' ;

When : 'when' | '|' ;

/// Ops

CompOp : '<' | '=<' | '==' | '=>' | '>' | '/=' | '=/=' | '=:=' ;

ListOp : '++' | '--' ;

AddOp : '+' | '-' | 'bsl' | 'bsr'
      | 'or' | 'xor' | 'bor' | 'bxor' ;

MulOp : '*' | '/' | 'div' | 'rem' | 'and' | 'band' ;

PrefixOp : '+' | '-' | 'not' | 'bnot' ;

/// funDef

funDef : Atom args guard? '=' seqExprs End ;

args : '(' allowedLasts? ')' ;

exprs :         expr  (',' expr )* ;

guard : When exprs (';' exprs)* ;

/// expr | seqExprs

expr    : (expr150|allowedLast) ('='|'!') (expr150|allowedLast)
        |  expr150 ;

expr150 : (expr160|allowedLast) 'orelse'  (expr150|allowedLast)
        |  expr160 ;

expr160 : (expr200|allowedLast) 'andalso' (expr160|allowedLast)
        |  expr200 ;

expr200 : (expr300|allowedLast) CompOp    (expr200|allowedLast)
        |  expr300 ;

expr300 : (expr400|allowedLast) ListOp    (expr300|allowedLast)
        |  expr400 ;

expr400 : (expr500|allowedLast) AddOp     (expr400|allowedLast)
        |  expr500 ;

expr500 : (expr600|allowedLast) MulOp     (expr500|allowedLast)
        |  expr600 ;

expr600 :                       PrefixOp  (expr700|allowedLast)
        |                                  expr700 ;

expr700 : functionCall
        //| recordExpr
        | exprMax
        ;

exprMax : atomic
        ;

allowedLast : Var
            | '(' (expr|allowedLast) ')'
            ;

allowedLasts : allowedLast (',' allowedLasts)* ;

seqExprs : expr* allowedLast? ;
// f () = B = A (B). #=> ok
// f () = (B) B = A. #=> line 1:11 mismatched input 'B' expecting {'.', 'end'}


functionCall : (exprMax|allowedLast) ':' (exprMax|allowedLast) args
             |                           (exprMax|allowedLast) args
             |                       ':' (exprMax|allowedLast) args
             |                       ':'                       args ;

//recordExpr : '‹'
