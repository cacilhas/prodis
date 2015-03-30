% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(autosave, [start_autosave/0]).
:- [library(settings), library(persistency), logging, database].


start_autosave :-
	setting(prodis:cachedelay, Delay),
	Delay > 0, !,
	database:connect(none),
	mainloop(none).

start_autosave :-
	database:connect(flush),
	mainloop(flush).


save :-
    with_mutex(database, ignore(db_sync_all(gc))).


mainloop(State) :-
    setting(prodis:cachedelay, Delay),
	mainloop_step2(State, Delay).


%% Open as none with delay, OK
mainloop_step2(none, Delay) :-
    Delay > 0, !,
    sleep(Delay),
	log(debug, 'autosave', []),
    save,
    mainloop(none).

%% Open as flush with delay, should be none
mainloop_step2(flush, Delay) :-
	Delay > 0, !,
	log(debug, 'change to sync(none)', []),
	with_mutex(database, (db_sync_all(close),
	                      database:connect(none))),
	sleep(Delay),
	log(debug, 'autosave', []),
	save,
	mainloop(none).

%% Open as none without delay, should be flush
mainloop_step2(none, Delay) :-
	Delay =< 0, !,
	log(debug, 'change to sync(flush)', []),
	with_mutex(database, (db_sync_all(close),
	                      database:connect(flush))),
	sleep(10),
	mainloop(flush).

%% Open as flush without delay, OK
mainloop_step2(flush, Delay) :-
	Delay =< 0, !,
	sleep(10),
	mainloop(flush).

% vim:set et:syntax=prolog
