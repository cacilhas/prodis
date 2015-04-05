% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_operators, []).
:- ['../src/operators'].

:- begin_tests(operators).

test(slice, forall(member(Args, [[[], 4, []],
                                   [[1, 2, 3], 0, []],
                                   [[1, 2, 3], 2, [1, 2]]]))) :-
    apply(slice, Args).

% TODO: test other predicates

:- end_tests(operators).

% vim:set et:syntax=prolog
