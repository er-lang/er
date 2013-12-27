grammar Kju;

root : block+ EOF ;

block : funDef ;


/// Tokens

tokAtom : TokAtom ;
TokAtom : [a-z]~[\s()\[\]{}:]*
    | '\'' ( '\\' (~'\\'|'\\') | ~[\\''] )* '\'' ;
    // Add A-Z to the negative match to forbid camelCase

// When using negative match, be sure to also negative match
//   previously-defined rules.

tokVar : TokVar ;
TokVar : [A-Z_][0-9a-zA-Z_]* ;

tokFloat : TokFloat ;
TokFloat : '-'? [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

tokInteger : TokInteger ;
TokInteger : '-'? [0-9]+ ('#' [0-9a-zA-Z]+)? ;

tokChar : TokChar ;
TokChar : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

tokString : TokString ;
TokString : '"' ( '\\' (~'\\'|'\\') | ~[\\""] )* '"' ;

atomic : tokChar
       | tokInteger
       | tokFloat
       | tokAtom
       | (tokString)+
       ;

tokEnd : 'end' | '.' ;

tokWhen : 'when' | '|' ;

/// Blanks

Comment : '#' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;


/// funDef

funDef : tokAtom args guard? '=' seqExprs tokEnd ;

args : '(' allowedLasts? ')' ;

exprs :         expr  (',' expr )* ;

guard : tokWhen exprs (';' exprs)* ;

// expr | seqExprs

expr    : (expr150|allowedLast) '=' (expr|allowedLast)
        | expr150 ;

expr150 : (expr160|allowedLast) 'orelse' (expr150|allowedLast)
        | expr160 ;

expr160 : (expr200|allowedLast) 'andalso' (expr160|allowedLast)
        | expr200 ;

expr200 : (exprMax|allowedLast) ':' (exprMax|allowedLast) args
        |                       ':' (exprMax|allowedLast) args
        |                       ':'                       args
        |                           (exprMax|allowedLast) args
        | exprMax ;

exprMax : atomic
        ;

allowedLast : tokVar
            | '(' (expr|allowedLast) ')'
            ;

allowedLasts : allowedLast (',' allowedLasts) ;

seqExprs : expr* allowedLast? ;
// f () = B = A (B). #=> ok
// f () = (B) B = A. #=> line 1:11 mismatched input 'B' expecting {'.', 'end'}
