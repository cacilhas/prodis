% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(prodis, [start_prodis/0]).
:- [library(settings), logging, autosave, listen].

:- set_prolog_flag(optimise, true).

:- if(current_prolog_flag(version_data, swi(6, _, _, _))).
:- set_prolog_flag(double_quotes, string).
:- endif.


%% Valid settings --------------------------------------------------------------
:- setting(host, atom, localhost, 'host to bind').
:- setting(port, positive_integer, 6379, 'port to bind').
:- setting(cache, atom, 'cache.db', 'cache file').
:- setting(cachedelay, positive_integer, 300, 'seconds between cache saving').
:- setting(logfile, atom, user, 'log file').
:- setting(loglevel, atom, debug, 'log level').
:- setting(password, list, [], 'list of valid passwords').
:- setting(conffile, atom, '', 'settings file').


start_prodis :-
    current_prolog_flag(argv, Args),
    load_parameters(Args),
    setting(prodis:conffile, Settings),
    load_settings(Settings),
    setting(prodis:logfile, LogFile),
    set_logfile(LogFile),
    log(info, 'starting server', []),
    thread_create(start_autosave, _, [alias(autosave),
                                      detached(true),
                                      at_exit(autosave:save)]),
    start_listening.


%% Load parameters from command line -------------------------------------------
load_parameters([]) :- !.

load_parameters(['-s', Settings | Rest]) :-
    set_setting(prodis:conffile, Settings), !,
    load_parameters(Rest).

load_parameters(['-l', LogFile | Rest]) :-
    set_setting(prodis:logfile, LogFile), !,
    load_parameters(Rest).


%% Auxiliar formats
load_parameters(['--settings', Settings | Rest]) :- !,
    load_parameters(['-s', Settings | Rest]).

load_parameters([Param | Rest]) :-
    concat('--settings=', Settings, Param), !,
    load_parameters(['-s', Settings | Rest]).

load_parameters([Param | Rest]) :-
    concat('-s', Settings, Param), !,
    load_parameters(['-s', Settings | Rest]).

load_parameters(['--log-file', LogFile | Rest]) :- !,
    load_parameters(['-l', LogFile | Rest]).

load_parameters([Param | Rest]) :-
    concat('--log-file=', LogFile, Param), !,
    load_parameters(['-l', LogFile | Rest]).

load_parameters([Param | Rest]) :-
    concat('-l', LogFile, Param), !,
    load_parameters(['-l', LogFile | Rest]).

load_parameters([Param | _]) :-
    format('unknown parameter: ~w~n', [Param]),
    !, fail.

% vim:set et:syntax=prolog
