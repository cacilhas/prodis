% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(logging, [log/3, set_logfile/1]).
:- [library(settings)].

:- dynamic formatter/1.


log(Level, Format, Args) :-
    ignore(rlog(Level, Format, Args)).


set_logfile(user) :-
	told,
	tell(user).


set_logfile(LogFile) :-
    open(LogFile, append, Stream, [buffer(line),
								   close_on_abort(true),
								   encoding(utf8)]),
	told,
    tell(Stream).


rlog(Level, Format, Args) :-
    setting(prodis:loglevel, MinLog),
    level(Level, LevelNumber),
    level(MinLog, Min),
    LevelNumber >= Min,
    formatter(Formatter),
    use_module(Formatter),
    Formatter:format_log(Level, Format, Args, Log),
    call(Log).


set_formatter(Formatter) :-
    retractall(formatter(_)),
    asserta(formatter(Formatter)).


level(debug, 10).
level(info, 20).
level(warn, 30).
level(error, 40).
level(fatal, 50).

formatter(log_formatter).

% vim:set et:syntax=prolog
