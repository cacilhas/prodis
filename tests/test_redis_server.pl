% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(test_redis_server, []).
:- ['../src/redis_server', '../src/database'].


current_setup :-
    database:connect(none).

current_cleanup :-
    autosave:save,
    setting(prodis:cache, CacheFile),
    delete_file(CacheFile).


:- begin_tests(redis_server, [setup(current_setup),
                                cleanup(current_cleanup)]).

test('QUIT') :-
    redis_process_command("QUIT", [], "+CLOSED\r\n").

test('APPEND', [setup(regset("foo", "bar")),
                cleanup(regdel(_))]) :-
    redis_process_command("APPEND", ["foo", " baaz"], ":8\r\n"),
    regget("foo", [type-string, key-_, value-"bar baaz"]).

test('APPEND_unexistent', [cleanup(regdel(_))]) :-
    redis_process_command("APPEND", ["foo", "bar"], ":3\r\n"),
    regget("foo", [type-string, key-_, value-"bar"]).

test('APPEND_other_type', [setup(regset("foo", bar-baaz)),
                            cleanup(regdel(_))]) :-
    redis_process_command("APPEND", ["foo", "foobar"],
                            "-WRONGTYPE Operation against a key holding the wrong kind of value\r\n").

test('BITCOUNT', [setup((regset("foo", 10),
                          regset("bar", 4),
                          regset("baaz", 255))),
                   cleanup(regdel(_))]) :-
    redis_process_command("BITCOUNT", ["foo"], ":2\r\n"),
    redis_process_command("BITCOUNT", ["bar"], ":1\r\n"),
    redis_process_command("BITCOUNT", ["baaz"], ":8\r\n"),
    redis_process_command("BITCOUNT", ["zero"], ":0\r\n").

%%  TODO: BITOP

:- end_tests(redis_server).

% vim:set et:syntax=prolog
