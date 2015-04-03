% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_database, []).
:- ['../database'].


current_setup :-
    database:connect(none, '/tmp/cache').

current_cleanup :-
    db_sync_all(gc),
    delete_file('/tmp/cache').


:- begin_tests(database, [setup(current_setup),
                           cleanup(current_cleanup)]).

test(regexists_true, [setup(regset("a key", "a value")),
                       cleanup(regdel(_))]) :-
    regexists("a key").

test(regexists_false, [fail]) :-
    regexists("a key").

test(regexists_field_true, [setup(regset("a key", "a field"-"a value")),
                             cleanup(regdel(_))]) :-
    regexists("a key", "a field").

test(regexists_field_false, [setup(regset("a key", "a field")),
                              cleanup(regdel(_)), fail]) :-
    regexists("a key", "a field").

test(regset_multiple_fields, [cleanup(regdel(_))]) :-
    regset("a key", "field 1"-"value 1"),
    regset("a key", "field 2"-"value 2"),
    regget("a key", "field 1", [type-string, key-"a key", field-"field 1", value-"value 1"]),
    regget("a key", "field 2", [type-string, key-"a key", field-"field 2", value-"value 2"]).

test(regdel_field, [cleanup(regdel(_))]) :-
     regset("a key", "field 1"-"value 1"),
     regset("a key", "field 2"-"value 2"),
     regdel("a key", "field 1"),
     \+ regexists("a key", "field 1"),
     regget("a key", "field 2", [type-string, key-"a key", field-"field 2", value-"value 2"]).

test(regset_null) :-
    regset("a key", "a value"),
    regset("a key", null),
    \+ regexists("a key").

test(regset_field_null) :-
    regset("a key", "a field"-"a value"),
    regset("a key", "a field"-null),
    \+ regexists("a key", "a field"),
    \+ regexists("a key").

test(regfields, [cleanup(regdel(_))]) :-
    regset("key 1", "field 1"-1),
    regset("key 2", "field 2"-2),
    regset("key 2", "field 3"-3),
    regfields("key 2", ["field 3", "field 2"]).

test(regkeys, [cleanup(regdel(_))]) :-
    regset("key 1", 1),
    regset("key 2", 2),
    regset("key 3", 3),
    regset("foo", 4),
    regkeys("key*", ["key 3", "key 2", "key 1"]).

:- end_tests(database).

% vim:set et:syntax=prolog



