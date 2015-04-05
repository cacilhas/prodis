% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_operators, []).
:- ['../src/operators'].

:- begin_tests(operators).

test(slice, forall(member(Args, [[[], 4, []],
                                   [[1, 2, 3], 0, []],
                                   [[1, 2, 3], 2, [1, 2]]]))) :-
    apply(slice, Args).

test(byte_list, [forall(member([Byte, List], [[0, [0, 0, 0, 0, 0, 0, 0, 0]],
                                                [10, [0, 1, 0, 1, 0, 0, 0, 0]],
                                                [65, [1, 0, 0, 0, 0, 0, 1, 0]],
                                                [255, [1, 1, 1, 1, 1, 1, 1, 1]]]))]) :-
    byte_list(Byte, X), X = List.

test(reverse_byte_list, [forall(member([Byte, List], [[0, [0, 0, 0, 0, 0, 0, 0, 0]],
                                                         [10, [0, 1, 0, 1, 0, 0, 0, 0]],
                                                         [65, [1, 0, 0, 0, 0, 0, 1, 0]],
                                                         [255, [1, 1, 1, 1, 1, 1, 1, 1]]]))]) :-
    byte_list(X, List), X = Byte.

test(count_bits, [forall(member([Byte, Count], [[0, 0],
                                                  [10, 2],
                                                  [65, 2],
                                                  [255, 8]]))]) :-
    count_bits(Byte, Count).

test(apply_not) :-
    apply_not([0, 10, 65, 255], [255, 245, 190, 0]).

test(apply_and) :-
    apply_and([75, 0], [10, 255], [10, 0]).

test(apply_or) :-
    apply_or([65, 0], [11, 255], [75, 255]).

test(apply_xor) :-
    apply_xor([65, 0, 255], [11, 255, 255], [74, 255, 0]).

test(term_output, [forall(member(Args, [[null, "$-1\r\n"],
                                          [42, ":42\r\n"],
                                          [true, ":1\r\n"],
                                          [false, ":0\r\n"],
                                          ["test", "$4\r\ntest\r\n"],
                                          [[null, "test"], "*2\r\n$-1\r\n$4\r\ntest\r\n"],
                                          [default, "+default\r\n"]]))]) :-
    apply(term_output, Args).

test(term_string, [forall(member(Args, [["test", "test"],
                                          [`test`, "test"],
                                          [42, "42"],
                                          [default, "default"]]))]) :-
    apply(term_string, Args).

:- end_tests(operators).

% vim:set et:syntax=prolog
