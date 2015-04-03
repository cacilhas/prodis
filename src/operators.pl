% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(operators, [slice/3,
                      byte_list/2,
                      count_bits/2,
                      bit_not/2,
                      apply_not/2,
                      apply_and/3,
                      apply_or/3,
                      apply_xor/3,
                      term_string/2,
                      term_output/2]).

slice([], _, []) :- !.

slice(_, 0, []) :- !.

slice([X|Xs], N, [X|R]) :-
    N > 0,
    N1 is N - 1,
    slice(Xs, N1, R).


byte_list(Byte, L) :-
    var(L), !,
    integer(Byte), Byte >= 0, Byte < 256,
    byte_to_list(inner, Byte, Aux),
    append(Aux, [0, 0, 0, 0, 0, 0, 0, 0], Aux1),
    slice(Aux1, 8, L).

byte_list(Byte, L) :-
    var(Byte), !,
    is_list(L),
    length(L, 8),
    list_to_byte(L, 1, 0, Byte).

byte_to_list(inner, 0, []) :- !.

byte_to_list(inner, Byte, [Bit|L]) :-
    Bit is Byte mod 2,
    Rest is Byte div 2,
    byte_to_list(inner, Rest, L).

list_to_byte([], _, Byte, Byte).

list_to_byte([1|Xs], Index, Acc, Byte) :-
    Acc1 is Index + Acc, !,
    Index1 is Index << 1,
    list_to_byte(Xs, Index1, Acc1, Byte).

list_to_byte([0|Xs], Index, Acc, Byte) :-
    Index1 is Index << 1,
    list_to_byte(Xs, Index1, Acc, Byte).


count_bits(N, Bits) :-
    integer(N),
    N >= 0, !,
    byte_list(N, L),
    sum_list(L, Bits).

count_bits(L, Bits) :-
    is_list(L), !,
    count_bits(L, 0, Bits).

count_bits([], Bits, Bits).

count_bits([X|Xs], Acc, Bits) :-
    count_bits(X, C),
    Acc1 is Acc + C,
    count_bits(Xs, Acc1, Bits).


bit_not(0, 1).

bit_not(1, 0).


apply_not([], []).

apply_not([X|Xs], [Z|Zs]) :-
    byte_list(X, L),
    findall(Y, (member(M, L), bit_not(M, Y)), Aux),
    byte_list(Z, Aux),
    apply_not(Xs, Zs).


apply_and([], [], []) :- !.

apply_and([_|_], [], []) :- !.

apply_and([], [_|_], []) :- !.

apply_and([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X /\ Y,
    apply_and(Xs, Ys, Zs).


apply_or(Z, [], Z) :- !.

apply_or([], Z, Z) :- !.

apply_or([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X \/ Y,
    apply_or(Xs, Ys, Zs).


apply_xor(Z, [], Z) :- !.

apply_xor([], Z, Z) :- !.

apply_xor([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X xor Y,
    apply_or(Xs, Ys, Zs).


term_output(null, `$-1\r\n`) :- !.

term_output(true, `$:1\r\n`) :- !.

term_output(false, `$:0\r\n`) :- !.

term_output(Data, Output) :-
    number(Data), !,
    format(codes(Output), ':~w\r~n', [Data]).

term_output(Data, Output) :-
    string(Data), !,
    string_length(Data, Length),
    format(codes(Output), '$~w\r~n~s\r~n', [Length, Data]).

term_output(Data, Output) :-
    is_list(Data), !,
    length(Data, Length),
    format(codes(Acc), '*~w\r~n', [Length]),
    list_to_output(Data, Acc, Output).

term_output(Data, Output) :-
    format(codes(Output), '+~w\r~n', [Data]).


list_to_output([], Acc, Acc) :- !.

list_to_output([X|Xs], Acc, Output) :-
    term_output(X, R),
    format(codes(Acc2), '~s~s', [Acc, R]),
    list_to_output(Xs, Acc2, Output).


term_string(Term, Term) :-
    string(Term), !.

term_string(Term, String) :-
    is_list(Term), !,
    string_codes(String, Term).

term_string(Term, String) :-
    number(Term), !,
    format(atom(Aux), '~w', Term),
    term_string(Aux, String).

term_string(Term, String) :-
    atom(Term), !,
    atom_string(Term, String).


% vim:set et:syntax=prolog
