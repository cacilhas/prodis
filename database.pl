% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(database, [regexists/1, regexists/2,
                     regdel/1, regdel/2,
                     regset/2,
                     regget/2, regget/3,
                     regfields/2, regkeys/2]).
:- [library(persistency)].

:- persistent
   register(type:oneof([number, string, hash, list]), key:string, value:any).


regexists(Key) :-
    regget(Key, Data),
    \+ member(value-null, Data).


regexists(Key, Field) :-
    regget(Key, Field, Value), Value \= null.


regdel(Key) :-
    with_mutex(database, retractall_register(_, Key, _)).


regdel(Key, Field) :-
    with_mutex(database, retractall_register(hash, Key, Field-_)).


regset(Key, null) :- !,
    regdel(Key).

regset(Key, Value) :-
    Value = _-_, !,
    regset(hash, Key, Value).

regset(Key, Value) :-
    number(Value), !,
    regset(number, Key, Value).

regset(Key, Value) :-
    (string(Value); atom(Value)), !,
    regset(string, Key, Value).


regget(Key, [type-Type, key-Key, value-Value]) :-
    with_mutex(database, register(Type, Key, Value)), !.

regget(Key, [type-none, key-Key, value-null]).

regget(Key, Field, [type-Type, key-Key, field-Field, value-Value]) :-
    with_mutex(database, register(hash, Key, Field-Value)),
    (Value = _-_ -> Type = hash;
     is_list(Value) -> Type = list;
     string(Value) -> Type = string;
     number(Value) -> Type = number;
     Type = term).


regfields(Key, Fields) :-
    with_mutex(database,
               findall(Field, register(hash, Key, Field-_), Fields)).


regkeys(Match, Keys) :-
    with_mutex(database,
               findall(Key, (register(_, Key, _),
                             wildcard_match(Match, Key)), Keys)).


regset(hash, Key, Value) :-
    Value = Field-null, !,
    regdel(Key, Field).

regset(hash, Key, Value) :-
    Value = Field-Value1, !,
    with_mutex(database, hash_regset(Key, Field, Value1)).

regset(Type, Key, Value) :-
    with_mutex(database,
               (retractall_register(_, Key, _),
                asserta_register(Type, Key, Value))).


hash_regset(Key, Field, Value) :-
    register(hash, Key, _), !,
    retractall_register(hash, Key, Field-_),
    asserta_register(hash, Key, Field-Value).

hash_regset(Key, Field, Value) :-
    retractall_register(_, Key, _),
    asserta_register(hash, Key, Field-Value).


connect(Sync) :-
    setting(prodis:cache, CacheFile),
    connect(Sync, CacheFile).

connect(Sync, CacheFile) :-
    db_attach(CacheFile, [sync(Sync)]).

% vim:set et:syntax=prolog
