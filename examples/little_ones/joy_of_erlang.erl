-module(joy_of_erlang).
-compile(export_all).
%% http://www.evanmiller.org/joy-of-erlang.html

% Section 1

noop(A) -> A.

not_(true) -> false;
not_(false) -> true.

equals(A, A) -> true;
equals(A, B) -> false.

% AND: true if both arguments are true, false otherwise
and_(true, true) -> true;
and_(_, _) -> false.

% OR: true if either or both arguments are true, false otherwise
or_(false, false) -> false;
or_(_, _) -> true.

% XOR: true if one argument is true and the other false; otherwise, false
xor_(A, A) -> false;
xor_(_, _) -> true.

% NAND: false if both arguments are true, true otherwise
nand(true, true) -> false;
nand(_, _) -> true.

% NOR: true if both arguments are false, true otherwise
nor(false, false) -> true;
nor(_, _) -> false.


% Section 2

incr(A) -> A + 1.
decr(A) -> A - 1.

add(A, 0) -> A;
add(A, B) when B < 0 -> add(decr(A), incr(B));
add(A, B) -> add(incr(A), decr(B)).

sub(A, 0) -> A;
sub(A, B) when B < 0 -> sub(incr(A), incr(B));
sub(A, B) -> sub(decr(A), decr(B)).

multiply(A, 0) -> 0;
multiply(A, B) when B < 0 -> sub(multiply(A, incr(B)), A);
multiply(A, B) -> add(A, multiply(A, decr(B))).

remainder(A, B) when A < B -> A;
remainder(A, B) -> remainder(sub(A, B), B).

quotient(A, B) -> quotient(A, B, 0).

% Internal function includes an accumulator
quotient(A, B, Answer) when A < B -> Answer;
quotient(A, B, Answer) -> quotient(sub(A, B), B, incr(Answer)).

raise(A, 0) -> 1;
raise(A, B) -> multiply(A, raise(A, decr(B))).

% Section 3

capfirst([Head | Tail]) when Head >= $a, Head =< $z ->
    [Head + ($A - $a) | Tail];
capfirst(Other) ->
    Other.

% public
uppercase(String) ->
   uppercase(String, []).

% internal
uppercase([], Output) ->
    lists:reverse(Output);
uppercase([Char | Rest], Output) when Char >= $a, Char =< $z ->
    uppercase(Rest, [Char + ($A - $a) | Output]);
uppercase([Char | Rest], Output) ->
    uppercase(Rest, [Char | Output]).

titlecase(String) ->
    titlecase(String, []).

titlecase([], Output) ->
    lists:reverse(Output);
titlecase([Char | Rest], [] = Output) when Char >= $a, Char =< $z ->
    titlecase(Rest, [Char + ($A - $a) | Output]);
titlecase([Char | Rest], [$\  |_] = Output) when Char >= $a, Char =< $z ->
    titlecase(Rest, [Char + ($A - $a) | Output]);
titlecase([Char | Rest], Output) ->
    titlecase(Rest, [Char | Output]).


% Section 4

% public
atoi([$- | String]) -> % negative
    -1 * atoi(String, 0);
atoi(String) -> % non-negative
    atoi(String, 0).

% internal
atoi([], Acc) ->
    Acc;
atoi([C | Rest], Acc) when C >= $0, C =< $9 ->
    atoi(Rest, 10 * Acc + (C - $0)).

% public
to_string(0) ->
    [$0];
to_string(Integer) when Integer < 0 -> % negative
    [$-|to_string(-1 * Integer, [])];
to_string(Integer) -> % positive
    to_string(Integer, []).

% internal
to_string(0, Acc) ->
    Acc;
to_string(Integer, Acc) ->
    to_string(Integer div 10, [(Integer rem 10) + $0 | Acc]).

% public
num2excel(Number) ->
    num2excel((Number-1) div 26, (Number-1) rem 26, []).

% internal
num2excel(0, Remainder, Acc) ->
    [(Remainder + $A)|Acc];
num2excel(Quotient, Remainder, Acc) ->
    num2excel((Quotient-1) div 26, (Quotient-1) rem 26, [(Remainder + $A)|Acc]).

% public
wordcount(Input) ->
    wordcount(Input, 0).

% internal
wordcount([], Count) ->
    Count;
% End of the input. Count the last word, if we didn't already
wordcount([C1], Count) when C1 =/= $\  ->
    Count+1;
% End of a word. Count it.
wordcount([C1, C2|Rest], Count) when C1 =/= $\ , C2 =:= $\  ->
    wordcount([C2|Rest], Count + 1);
% Not the end of a word. Don't count it.
wordcount([_|Rest], Count) ->
    wordcount(Rest, Count).

% public
escape(String) ->
    escape(String, []).

% internal
escape([], Acc) ->
    lists:reverse(Acc);
escape([$< | Rest], Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape([$> | Rest], Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape([$& | Rest], Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

% public
wordwrap(Input) ->
    wordwrap(Input, [], [], 0, 80).

% internal
% No more input, we're done
wordwrap([], Acc, WordAcc, LineLength, WrapAt) ->
    lists:reverse(WordAcc ++ Acc);

% Premature newline
wordwrap([$\n | Rest], Acc, WordAcc, LineLength, WrapAt) ->
    wordwrap(Rest, [$\n | WordAcc ++ Acc], [], 0, WrapAt);

% Hit the wrap length at a space character. Add a newline
wordwrap([$\  | Rest], Acc, WordAcc, WrapAt, WrapAt) ->
    wordwrap(Rest, [$\n | WordAcc ++ Acc], [], 0, WrapAt);

% Hit a space character before the wrap length. Keep going
wordwrap([$\  | Rest], Acc, WordAcc, LineLength, WrapAt) ->
    wordwrap(Rest, [$\  | WordAcc ++ Acc], [], LineLength + 1 + erlang:length(WordAcc), WrapAt);

% Overflowed the current line while building a word. Push on a newline character and keep building a word
wordwrap([C | Rest], Acc, WordAcc, LineLength, WrapAt)
                        when erlang:length(WordAcc) + LineLength > WrapAt ->
    wordwrap(Rest, [$\n | Acc], [C | WordAcc], 0, WrapAt);

% Just building a word...
wordwrap([C | Rest], Acc, WordAcc, LineLength, WrapAt) ->
    wordwrap(Rest, Acc, [C | WordAcc], LineLength, WrapAt).
