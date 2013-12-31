grammar Kju;

root : block+ EOF ;

block : funDef ;

/// Blanks

Comment : '#' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;


/// Tokens

tokAtom : TokAtom ;
TokAtom : [a-z]~[ \t\r\n()\[\]{}:.]*
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


/// funDef

funDef : tokAtom args guard? '=' seqExprs tokEnd ;

args : '(' allowedLasts? ')' ;

allowedLasts : allowedLast (',' allowedLasts)* ;

exprs :         expr  (',' expr )* ;

guard : tokWhen exprs (';' exprs)* ;

/// expr | seqExprs

// Protected Exprs
pExpr : expr    | allowedLast ;
mExpr : exprMax | allowedLast ;

// Order matters
expr : pExpr '='       pExpr
     | pExpr 'orelse'  pExpr
     | pExpr 'andalso' pExpr
     | functionCall
     | exprMax
     ;

exprMax : atomic
        ;

allowedLast : tokVar
            | '(' pExpr ')'
            ;

seqExprs : expr* allowedLast? ;
// f () = B = A (B). #=> ok
// f () = (B) B = A. #=> line 1:11 mismatched input 'B' expecting {'.', 'end'}

functionCall : mExpr ':' mExpr args
             |       ':' mExpr args
             |       ':'       args
             |           mExpr args ;
