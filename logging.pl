% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(logging, [log/3, set_logfile/1, set_log_formatter/1]).
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


set_log_formatter(Formatter) :-
    retractall(formatter(_)),
    asserta(formatter(Formatter)).


valid_levels([debug, info, warn, error, fatal]).


rlog(Level, Format, Args) :-
    setting(prodis:loglevel, MinLog),
    valid_levels(Levels),
    valid_log(Level, MinLog, Levels),
    formatter(Formatter),
    use_module(Formatter),
    Formatter:format_log(Level, Format, Args, Log),
    write(Log).


valid_log(_Current, _Min, []) :- !, fail.

valid_log(_Current, Min, [Min |_]) :- !.

valid_log(Current, _Min, [Current |_]) :- !, fail.

valid_log(Current, Min, [_|Rest]) :- valid_log(Current, Min, Rest).


formatter(log_formatter).

% vim:set et:syntax=prolog
