% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(prodis, [start_prodis/0]).
:- [library(settings), 'src/logging', 'src/autosave', 'src/listen'].

:- set_prolog_flag(optimise, on).

:- if((current_prolog_flag(version, V), V < 70000)).
:- current_prolog_flag(version, V),
   format('prodis does not work on SWI ~w~n', [V]),
   halt.
:- endif.

:- if((current_prolog_flag(version, V), V < 70134)).
:- write('required SWI 0.1.34, use by your own risk').
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
    load_parameters,
    log(info, 'starting server', []),
    thread_create(start_autosave, _, [alias(autosave),
                                      detached(true),
                                      at_exit(autosave:save)]),
    start_listening.

load_parameters :-
    current_prolog_flag(argv, Args),
    load_parameters(Args),
    setting(prodis:conffile, Settings),
    load_settings(Settings),
    setting(prodis:logfile, LogFile),
    set_logfile(LogFile).


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
